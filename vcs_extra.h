; Copyright (c) 2017-2021, Stephen Illingworth
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; v1.0 - 19/08/2017
; v1.1 - 08/02/2018   - __WAIT_SCANLINE_TIMER now calls WSYNC before setting timer
;                     - renamed labels so intention is clearer
; v1.2 - 21/01/2021   - corrected __WAIT_SCANLINE_TIMER again 

  include vcs_mdl.txt
  include dasm_extra.h

SELECT_SWITCH = %00000010

; -----------------------------------
; DISPLAY CONSTANTS

VBLANK_SCANLINES = $25    ; 37
DISPLAY_SCANLINES =  $C0    ; 192
OVERSCAN_SCANLINES = $1E  ; 30

; -----------------------------------
; USER INPUT
  MAC NEW_TRIGGER_CHECK
    ; > {player trigger} [V] (0,1)
    ; ! 8 bit address __STATE_INPT4 or __STATE_INPT5 
    ; + AXCZVN
    ; * BPL on trigger, BMI on no trigger

    IFCONST MDL_RANGE_CHECKING
      IF MDL_RANGE_CHECKING == TRUE
        IF {1} != 0 || {1} > 1
          DASM_MACRO_ERROR "'NEW_TRIGGER_CHECK': {1} must be 0 OR 1"
        ENDIF
      ENDIF
    ENDIF

    IF {1} == 0
      LDX INPT4
    ELSE 
      LDX INPT5
    ENDIF
    BMI .done          ; INPT4 is positive if trigger is pressed 

    ; read state of trigger from when last read
    IF {1} == 0
      LDA __STATE_INPT4
    ELSE
      LDA __STATE_INPT5
    ENDIF

    ; if stored state is also negative, then there has not been a state change
    ; if it is positive then there has been a state change
    ; this is opposite to the normal meaning of the trigger check so flip the bits to
    ; correct the meaning
    EOR #$FF
.done
    ; store state of trigger for next read
    IF {1} == 0
      STX __STATE_INPT4
    ELSE
      STX __STATE_INPT5
    ELSE
  ENDM

  MAC NEW_SWITCH_CHECK
    ; > {switch definition} [V]
    ; ! 8 bit address __STATE_SWCHB
    ; + AXCZVN
    ; * BNE on switch not set, BEQ on switch is set

    LDA SWCHB
    AND #{1}
    BNE .done
    LDA __STATE_SWCHB
    EOR #$FF
    AND #{1}
.done
  ENDM

  MAC STORE_SWITCH_STATE
    ; ! __STATE_SWCHB
    ; + ACZVN

    LDA SWCHB
    STA __STATE_SWCHB
  ENDM

; -----------------------------------
; SCANLINE TIMER
  MAC __WAIT_SCANLINE_TIMER
    ; ----
    ; original comment (with regard to 37 scan lines)
    ; ----
    ; hold VBLANK for 37 scan lines
    ; why give the timer a value of 43?
    ; 228 colour counts per scan line and 3 processor cycles per colour count
    ; = 76 processor cycles per scan line - 37 * 76 = 2812
    ; however:
    ;   the timer takes 5 cycles to set 
    ;   the checking loop (below) takes six cycles
    ;   = 11
    ; 2812 - 11 = 2801
    ; using the 64 cycle timer
    ; 2801 / 64 = 43
    ; ----

.SCANLINES SET {1}

.CLOCK_COUNTS_PER_SCANLINE = 228
.CLOCK_COUNTS_PER_CYCLE = 3
.CYCLES_PER_SCANLINE = .CLOCK_COUNTS_PER_SCANLINE / .CLOCK_COUNTS_PER_CYCLE
.KERNEL_TIMER_SET_IN_CYCLES = 6  ; not including initial WSYNC
.KERNEL_WAIT_LOOP = 7
.TIMER_VAL = (.CYCLES_PER_SCANLINE * .SCANLINES - .KERNEL_TIMER_SET_IN_CYCLES - .KERNEL_WAIT_LOOP) / 64

    LDA  #.TIMER_VAL    ; 2
    STA TIM64T                ; 4
    STA WSYNC
  ENDM

; -----------------------------------
; VSYNC KERNEL
  MAC VSYNC_KERNEL_BASIC
    VERTICAL_SYNC
  ENDM

; -----------------------------------
; VBLANK KERNEL
  ; empty vblank kernel - useful for when you don't want or need to do
  ; anything during the overscan. 
  MAC VBLANK_KERNEL_BASIC
    VBLANK_KERNEL_SETUP
    VBLANK_KERNEL_END
  ENDM

  ; alternative empty vblank which counts lines rather than using the timer
  MAC VBLANK_KERNEL_BASIC_NO_TIMER
    LDX  #(VBLANK_SCANLINES-2)
.vblank_loop
    STA WSYNC
    DEX
    BNE .vblank_loop
    ; turn beam back on at beginning of horizontal line
    STA WSYNC
    STA VBLANK
  ENDM

  MAC VBLANK_KERNEL_SETUP
    __WAIT_SCANLINE_TIMER #VBLANK_SCANLINES
  ENDM

  MAC VBLANK_KERNEL_END
.vblank_loop
    LDA INTIM
    BNE .vblank_loop
    ; turn beam back on at beginning of horizontal line
    STA WSYNC
    STA VBLANK
  ENDM

; -----------------------------------
; DISPLAY KERNEL
  ; unlike the VSYNC/VBLANK/OVERSCAN kernel macros you probably won't need to use these
  ; DISPLAY_KERNEL macros. the nature of the 2600 means you will be counting lines in the display
  ; and will know implicitely when the display is to complete. however, it is sometimes useful for
  ; for testing/development purposes to have a timed display and not worry too much about the
  ; number of lines being shown
  MAC DISPLAY_KERNEL_SETUP
    __WAIT_SCANLINE_TIMER #DISPLAY_SCANLINES
  ENDM

  MAC DISPLAY_KERNEL_END
.display_loop
    LDA INTIM
    BNE .display_loop
  ENDM

; -----------------------------------
; OVERSCAN KERNEL

  ; empty overscan kernel - useful for when you don't want or need to do
  ; anything during the overscan. 
  MAC OVERSCAN_KERNEL_BASIC
.overscan_kernel
    ; wait for overscan
    STA WSYNC
    LDA  #$2
    STA VBLANK

    LDX  #(OVERSCAN_SCANLINES-1)
.overscan_loop
    STA WSYNC
    DEX
    BNE .overscan_loop
  ENDM

  MAC OVERSCAN_KERNEL_SETUP
    ; wait for overscan
    STA WSYNC
    LDA  #$2
    STA VBLANK

    __WAIT_SCANLINE_TIMER #OVERSCAN_SCANLINES+1
  ENDM

  MAC OVERSCAN_KERNEL_END
.overscan_loop
    LDA INTIM
    BNE .overscan_loop
  ENDM

; -----------------------------------
; DEAD FRAME KERNEL
  MAC DEAD_FRAME
    VSYNC_KERNEL_BASIC
    VBLANK_KERNEL_BASIC

    STA WSYNC
    LDX  #DISPLAY_SCANLINES
.display_loop
    STA WSYNC
    DEX
    BNE .display_loop

    OVERSCAN_KERNEL_BASIC
  ENDM

; -----------------------------------
; MULTI COUNT

; Multi Count vs Two/Three Count comparison
; =========================================
;
;                              |------------ cycles ------------|
;                    RAM        setup      update    twos    threes    total (typical frame) [3]
; Two  [1]            1byte      5          12.5 (5)  3        -          15.5 (8)
; Three              1byte      5          12.5      -        5          17.5
;  Two & Three [1]    2byte      10        25 (17.5)  3        5          33 (25.5)
; Multi  [2]          1byte      5          19.75      5        7 (9)      29.75
;
;    [1] --> acumulator clobbering version of update in parenthesis
;    [2] --> threes comparison with set clear bit in parenthesis
;    [3] --> typical defined as one update, one twos comparison and one threes comparison


; Multi Count vs Two/Three Count side-effects
; ===========================================
;
; Two              clobbers Accumulator or X register
; Three            clobbers X register
; Multi            clobbers Accumulator


  MAC MULTI_COUNT_SETUP
    LDA #%10000010            ; 2
    STA __MULTI_COUNT_STATE    ; 3
    ; 5 cycles
  ENDM

  MAC MULTI_COUNT_UPDATE
    LDA __MULTI_COUNT_STATE    ; 3
    BMI .is_negative          ; 2/3

.is_positive
    SEC                        ; 2
    SBC #$1                    ; 3
    BMI .positive_reset        ; 2/3
    EOR #%10000000            ; 2
    JMP .store                ; 3

.positive_reset
    LDA #%10000010            ; 2
    JMP .store                ; 3

.is_negative
    EOR #%10000000            ; 2
    SEC                        ; 2
    SBC #$1                    ; 3
    BPL .store                ; 2/3
.negative_reset
    LDA #%00000010            ; 2

.store
    STA __MULTI_COUNT_STATE    ; 3
    ; cycles
    ; ------
    ; 20 - is_negative, negative_reset
    ; 19 - is_negative NO negative_reset
    ; 20 - is_positive, positive_reset
    ; 21 - is_positive NO positive_reset
    ; AVG = 19.75
  ENDM

  MAC MULTI_COUNT_TWO_CMP
    ; result (waiting for every other frame, or differentiating between the two states, is the same)
    ;    - branch on BEQ and BNE
    LDA #%10000000            ; 2
    AND __MULTI_COUNT_STATE      ; 3
    ; 5 cycles
  ENDM

  MAC MULTI_COUNT_THREE_CMP
    ; {set carry bit before subtract -> boolean}

    ; result (depends on what you are trying to achieve)
    ;    - differentiating between three states:
    ;      branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
    ;    - waiting for third frame
    ;      branch ON third frame - BEQ or BMI
    ;      branch NOT ON third frame - BNE or BPL

    LDA #%00000011            ; 2
    AND __MULTI_COUNT_STATE    ; 3

    IF {1} != FALSE
      SEC                      ; 2
    ENDIF

    SBC #1                    ; 2
    ; 7 cycles - or 9 if SEC is used
  ENDM

  MAC MULTI_COUNT_SIX_CMP
    ; result (waiting for sixth state)
    ;    - branch on BEQ (six count); BNE (non six count)
    ; (note that you can't differentiate the six states using this multi count method)
    LDA __MULTI_COUNT_STATE      ; 3
  ENDM

; -----------------------------------
; TWO/THREE COUNTS
  MAC TWO_COUNT_SETUP_X
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    LDX #$1                  ; 2
    STX __TWO_COUNT_STATE    ; 3
    ; 5 cycles
  ENDM

  MAC TWO_COUNT_UPDATE_X
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    LDX __TWO_COUNT_STATE      ; 3
    DEX                      ; 2
    BPL .store_cycle_count  ; 2/3
    LDX #$1                  ; 2
.store_cycle_count
    STX __TWO_COUNT_STATE      ; 3
    ; 12/13 cycles
  ENDM

  MAC TWO_COUNT_CMP_X
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    ; result - branch on BEQ and BNE
    LDX __TWO_COUNT_STATE      ; 3
    ; 3 cycles
  ENDM


  MAC TWO_COUNT_SETUP_A
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    LDA #$1                  ; 2
    STA __TWO_COUNT_STATE    ; 3
    ; 5 cycles
  ENDM

  MAC TWO_COUNT_UPDATE_A
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    LDA #%00000001          ; 2
    EOR __TWO_COUNT_STATE    ; 3
    ; 5 cycles
  ENDM

  MAC TWO_COUNT_CMP_A
    ; require 8bit memory address labelled __TWO_COUNT_STATE
    ; result - branch on BEQ and BNE
    LDA __TWO_COUNT_STATE    ; 3
    ; 3 cycles
  ENDM


  MAC THREE_COUNT_SETUP_X
    ; require 8bit memory address labelled _THREE_COUNT_STATE
    LDX #$2                  ; 2
    STX _THREE_COUNT_STATE  ; 3
    ; 5 cycles
  ENDM

  MAC THREE_COUNT_UPDATE_X
    ; require 8bit memory address labelled _THREE_COUNT_STATE
    LDX _THREE_COUNT_STATE  ; 3
    DEX                      ; 2
    BPL .store_cycle_count  ; 2/3
    LDX #$2                  ; 2
.store_cycle_count
    STX _THREE_COUNT_STATE  ; 3
    ; 12/13 cycles
  ENDM

  MAC THREE_COUNT_CMP_X
    ; require 8bit memory address labelled _THREE_COUNT_STATE
    ; result - branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
    LDX _THREE_COUNT_STATE  ; 3
    DEX                      ; 2
    ; 5 cycles
  ENDM

