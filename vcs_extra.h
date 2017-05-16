
; version 0.1 - 14/5/2017

VBLANK_SCANLINES = $25		; 37
VISIBLE_SCANLINES =	$C0		; 192
OVERSCAN_SCANLINES = $1E	; 30

;-----------------------------------
	MAC DEAD_FRAME
		VSYNC_KERNEL
		VBLANK_KERNEL_WAIT

		STA WSYNC
		LDX	#VISIBLE_SCANLINES
.display_loop
		STA WSYNC
		DEX
		BNE .display_loop
		OVERSCAN_KERNEL_EMPTY
	ENDM
;-----------------------------------


;-----------------------------------
	MAC POS_RESET
		;	{reset address}, {clock counts}
		STA WSYNC
		SLEEP ({2}-3)/3
		STA {1} ; three cycles
	ENDM


	MAC POS_SCREEN_LEFT
		;	{reset address}, {offset}
		IF {2} < 0 || {2} > 50
			ECHO "MACRO ERROR: 'POS_SCREEN_LEFT': {2} must be >=0 AND <= 50"
			ERR
		ENDIF
		POS_RESET {1}, 57+({2}*3)
	ENDM

	MAC POS_SCREEN_MID
		;	{reset address}, {offset}
		POS_RESET {1}, 132+({2}*3)
	ENDM

	MAC POS_SCREEN_RIGHT
		;	{reset address}, {offset}
		IF {2} < 0 || {2} > 50
			ECHO "MACRO ERROR: 'POS_SCREEN_LEFT': {2} must be >=0 AND <= 50"
			ERR
		ENDIF
		POS_RESET {1}, 207-({2}*3)
	ENDM
;-----------------------------------


;-----------------------------------
; fine-tuned positioning routines -- TODO: needs work

	MAC FINE_POS_SCREEN_RIGHT
	;	{reset address} {SINGLE|DOUBLE|QUADRUPLE|OCTUPLE}
		POS_RESET {1}, 215

		IF {2} == "SINGLE"
			LDA #$C0	; -64
		ENDIF
		IF {2} == "DOUBLE"
			LDA #$D0	; -48
		ENDIF
		IF {2} == "QUADRUPLE"
			LDA #$F0	;	-16
		ENDIF
		IF {2} == "OCTUPLE"
			LDA #$30	; +48
		ENDIF

		IF {1} == RESBL
			STA HMBL
		ENDIF
		IF {1} == RESM0
			STA HMM0
		ENDIF
		IF {1} == RESM1
			STA HMM1
		ENDIF
		IF {1} == RESP0
			STA HMP0
		ENDIF
		IF {1} == RESP1
			STA HMP1
		ENDIF
	ENDM

	MAC FINE_POS_ACTIVATE
		STA WSYNC
		STA HMOVE
	ENDM

	MAC FINE_POS_END
		; writing to HMCLR within 24 machine cycles of HMOVE will negate the HMOVE
		STA HMCLR
	ENDM
;-----------------------------------


;-----------------------------------
	; like SLEEP in vcs.h but for clock counts rather than cycles
	MAC SLEEP_CLOCK_COUNTS
.CLOCK_COUNTS		SET {1}
		IF .CLOCK_COUNTS < 1
				ECHO "MACRO ERROR: 'SLEEP_CLOCK_COUNTS': Duration must be > 0"
				ERR
		ENDIF
    REPEAT .CLOCK_COUNTS / 6
			SLEEP 2
    REPEND
	ENDM
;-----------------------------------


;-----------------------------------
	MAC WAIT_SCANLINE_TIMER
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

.VBLANK_SCANLINES SET {1}

.CLOCK_COUNTS_PER_SCANLINE = 228
.CLOCK_COUNTS_PER_CYCLE = 3
.CYCLES_PER_SCANLINE = .CLOCK_COUNTS_PER_SCANLINE / .CLOCK_COUNTS_PER_CYCLE
.VBLANK_KERNEL_TIMER_SET_IN_CYCLES = 5
.VBLANK_KERNEL_WAIT_LOOP = 6
.VBLANK_TIMER_VAL = (.CYCLES_PER_SCANLINE * .VBLANK_SCANLINES - .VBLANK_KERNEL_TIMER_SET_IN_CYCLES - .VBLANK_KERNEL_WAIT_LOOP) / 64

		LDA	#.VBLANK_TIMER_VAL
		STA TIM64T
	ENDM
;-----------------------------------


;-----------------------------------
	MAC VSYNC_KERNEL_BASIC
		VERTICAL_SYNC
	ENDM
;-----------------------------------


;-----------------------------------
	MAC VBLANK_KERNEL_BASIC
		LDX	#(VBLANK_SCANLINES-2)
.vblank_loop
		STA WSYNC
		DEX
		BNE .vblank_loop
		; turn beam back on at beginning of horizontal line
		STA WSYNC
		STA VBLANK
	ENDM
;-----------------------------------


;-----------------------------------
	MAC VBLANK_KERNEL_SETUP
		WAIT_SCANLINE_TIMER #VBLANK_SCANLINES
	ENDM


	MAC VBLANK_KERNEL_END
.vblank_loop
		LDA INTIM
		BNE .vblank_loop
		; turn beam back on at beginning of horizontal line
		STA WSYNC
		STA VBLANK
	ENDM
;-----------------------------------


;-----------------------------------
	; empty overscan kernel - useful for when you don't want/need to do
	; anything during the overscan. 
	MAC OVERSCAN_KERNEL_BASIC
.overscan_kernel
		; wait for overscan
		STA WSYNC
		LDA	#2
		STA VBLANK

		LDX	#(OVERSCAN_SCANLINES-1)
.overscan_loop
		STA WSYNC
		DEX
		BNE .overscan_loop
	ENDM
;-----------------------------------


;-----------------------------------
	MAC OVERSCAN_KERNEL_SETUP
		; wait for overscan
		STA WSYNC
		LDA	#2
		STA VBLANK

		WAIT_SCANLINE_TIMER #OVERSCAN_SCANLINES
	ENDM


	MAC OVERSCAN_KERNEL_END
.overscan_loop
		LDA INTIM
		BNE .overscan_loop
	ENDM
;-----------------------------------


;-----------------------------------
	MAC TWO_FRAME_SETUP
		; requires a 8bit memory address labelled TWO_FRAME_COUNT
		LDX #$1									; 2
		STX TWO_FRAME_COUNT			; 3
	ENDM

	MAC TWO_FRAME_UPDATE
		LDX TWO_FRAME_COUNT			; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$1									; 2
.store_cycle_count
		STX TWO_FRAME_COUNT			; 3
	ENDM

	MAC TWO_FRAME_TRIAGE
		; branch on BEQ and BNE
		LDX TWO_FRAME_COUNT			; 3
	ENDM
;-----------------------------------


;-----------------------------------
	MAC THREE_FRAME_SETUP
		; requires a 8bit memory address labelled THREE_FRAME_COUNT
		LDX #$2									; 2
		STX THREE_FRAME_COUNT		; 3
	ENDM

	MAC THREE_FRAME_UPDATE
		LDX THREE_FRAME_COUNT		; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$2									; 2
.store_cycle_count
		STX THREE_FRAME_COUNT		; 3
	ENDM

	MAC THREE_FRAME_TRIAGE
		; branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		LDX THREE_FRAME_COUNT		; 3
		DEX											; 2
	ENDM
;-----------------------------------
