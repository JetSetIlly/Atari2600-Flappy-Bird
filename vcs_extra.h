
; version history
; 0.1 - 14/05/2017
;	0.2 - 09/07/2017 - fine positioning routines

NULL = 0

; -----------------------------------
; DISPLAY CONSTANTS
VBLANK_SCANLINES = $25		; 37
DISPLAY_SCANLINES =	$C0		; 192
OVERSCAN_SCANLINES = $1E	; 30

; -----------------------------------
; COARSE POSITIONING (SLEEP TABLES)

	MAC DEF_POS_SLEEP_TABLE
SLEEP_TABLE .byte >.SLEEP_POS_00, <.SLEEP_POS_00, >.SLEEP_POS_04, <.SLEEP_POS_04, >.SLEEP_POS_08, <.SLEEP_POS_08, >.SLEEP_POS_12, <.SLEEP_POS_12, >.SLEEP_POS_16, <.SLEEP_POS_16, >.SLEEP_POS_20, <.SLEEP_POS_20, >.SLEEP_POS_24, <.SLEEP_POS_24, >.SLEEP_POS_28, <.SLEEP_POS_28, >.SLEEP_POS_32, <.SLEEP_POS_32, >.SLEEP_POS_36, <.SLEEP_POS_36, >.SLEEP_POS_40, <.SLEEP_POS_40, >.SLEEP_POS_44, <.SLEEP_POS_44, >.SLEEP_POS_48, <.SLEEP_POS_48, >.SLEEP_POS_52, <.SLEEP_POS_52

HBLANK_CYCLES = 20
POS_SCREEN_CYCLES = 11

; TODO: compress the NOP calls generated by the call to SLEEP
; result will look something like this:
;
;									NOP
;									......
; .SLEEP_POS_08		NOP
;									NOP
;									......
;									NOP
; .SLEEP_POS_04		NOP
;									NOP
;									......
;									NOP
; .SLEEP_POS_00		NOP
;									NOP
;									......
;									NOP
;									RTS

.SLEEP_POS_00 SLEEP HBLANK_CYCLES + 0 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_04 SLEEP HBLANK_CYCLES + 4 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_08 SLEEP HBLANK_CYCLES + 8 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_12 SLEEP HBLANK_CYCLES + 12 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_16 SLEEP HBLANK_CYCLES + 16 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_20 SLEEP HBLANK_CYCLES + 20 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_24 SLEEP HBLANK_CYCLES + 24 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_28 SLEEP HBLANK_CYCLES + 28 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_32 SLEEP HBLANK_CYCLES + 32 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_36 SLEEP HBLANK_CYCLES + 36 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_40 SLEEP HBLANK_CYCLES + 40 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_44 SLEEP HBLANK_CYCLES + 44 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_48 SLEEP HBLANK_CYCLES + 48 - POS_SCREEN_CYCLES
			RTS
.SLEEP_POS_52 SLEEP HBLANK_CYCLES + 52 - POS_SCREEN_CYCLES
			RTS
	ENDM

; -----------------------------------
; COARSE POSITIONING (ACCUMULATOR)

	MAC __POS_SCREEN_A
		;	{sprite}
		; accumulator to be preset with coarse position
		; clobbers X register
		; requires 16 bit memory location called _SLEEP_TABLE_JMP

		ASL
		TAX
		LDA SLEEP_TABLE,X
		STA _SLEEP_TABLE_JMP+1
		INX
		LDA SLEEP_TABLE,X
		STA _SLEEP_TABLE_JMP
		JSR .sub
		STA {1}
		JMP .done
.sub
		STA WSYNC
		JMP (_SLEEP_TABLE_JMP)
.done
	ENDM

; -----------------------------------
; COARSE POSITIONING (IMMEDIATE)
	MAC __POS_SCREEN
		;	{sprite}, {machine cycles}
		STA WSYNC
		SLEEP {2} - 1		; -1 to account for the following STA
		STA {1}					; 3 cycles
	ENDM

	MAC POS_SCREEN_LEFT
		;	{sprite}, {offset}
		IF {2} < 0 || {2} > 50
			ECHO "MACRO ERROR: 'POS_SCREEN_LEFT': {2} must be >=0 AND <= 50"
			ERR
		ENDIF
		__POS_SCREEN {1}, (60/3) + {2}

		; 62 clock counts
		; = 20 machine cycles
		;
		; 68 clock counts to visible screen
		; = 22 machine cycles
		; 
		; 3 cycles for STA {sprite}
		; = 20 + 3 = 23 cycles	
	ENDM

	MAC POS_SCREEN_MID
		;	{sprite}, {offset}
		__POS_SCREEN {1}, (132/3) + {2}
	ENDM

	MAC POS_SCREEN_RIGHT
		;	{sprite}, {offset}
		IF {2} < 0 || {2} > 50
			ECHO "MACRO ERROR: 'POS_SCREEN_LEFT': {2} must be >=0 AND <= 50"
			ERR
		ENDIF
		__POS_SCREEN {1}, (216/3) - {2}
	ENDM

; -----------------------------------
; FINE POSITIONING

	MAC FINE_POS_SCREEN_LEFT
	; {sprite} {position store} {sprite width} {margin)
	; position store:
	;		- memory address in which to store position
	;		- do not store if "position store" == NULL or 0 
	; margin:
	;		- positive -> pixels
	;		- negative -> multiples of sprite width
	; note that {sprite width} doesn't mean very much except when {margin} is negative
		IF !({3} == 1 || {3} == 2 || {3} == 4 || {3} == 8)
			ECHO "MACRO ERROR: 'FINE_POS_SCREEN_LEFT': value of {3} must be 1, 2, 4 or 8"
			ERR
		ENDIF
		IF {4} >= 0
			LDA #(01 + {4})
		ELSE
			LDA #(01 + ({3} * -{4}))
		ENDIF
		IF {2} != 0
			STA {2}
		ENDIF
		FINE_POS_SCREEN {1}
	ENDM

	MAC FINE_POS_SCREEN_RIGHT
	; {sprite} {position store} {sprite width} {margin}
	; position store:
	;		- memory address in which to store position
	;		- do not store if "position store" == "NONE" or 0 
	; margin:
	;		- positive -> pixels
	;		- negative -> multiples of sprite width
		IF !({3} == 1 || {3} == 2 || {3} == 4 || {3} == 8)
			ECHO "MACRO ERROR: 'FINE_POS_SCREEN_RIGHT': value of {3} must be 1, 2, 4 or 8"
			ERR
		ENDIF
		IF {4} >= 0
			LDA #(160 - {3} - {4})
		ELSE
			LDA #(160 - ({3} * (-{4}+1)))
		ENDIF
		IF {2} != 0
			STA {2}
		ENDIF
		FINE_POS_SCREEN {1}
	ENDM

	MAC FINE_POS_SCREEN
	; {sprite}
	; accumulator to be preset with position value in pixels
	; define FINE_POS_NO_LIMIT_CHECK to remove screen/jump-table boundary check

		; check for screen limits
	IFCONST FINE_POS_LIMIT_CHECK
		CMP #160
		BCC .end_limit_check
		LDA #160
.end_limit_check
	ENDIF

		; find quotient and remainder when dividing by 12
		;  - use the quotient (Y register) to coarsly place the "sprite"
		;  - we can then use the horizontal movement registers to fine tune it (using the remainder - acummulator)
		LDY #0
		CMP #0
		BEQ .done_coarse_div
.coarse_div
		CMP #12
		BCC .done_coarse_div
		SBC #12
		INY
		JMP .coarse_div
.done_coarse_div

.fine_positioning
		; adjust fine tuning value into +7/-8 range
		SEC
		SBC #$07
		; move registers use high nibble
		ASL
		ASL
		ASL
		ASL
		; negative - one's complement
		EOR #$F0

		; copy fine tuning value (remainder) into appropriate horizontal movement register
		; call FINE_POS_ACTIVATE or equivalent later, to perform the movement
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

.coarse_positioning
		; move quotient to accumulator
		TYA
		__POS_SCREEN_A {1}
	ENDM

	MAC FINE_POS_MOVE_RIGHT
		; {position store} {amount}
		; amount value should be positive
		LDA {1}
		CLC
		ADC {2}
		CMP #160
		BCC .fine_pos_move_done
		SEC
		SBC #160
.fine_pos_move_done
		STA {1}
	ENDM

	MAC FINE_POS_MOVE_LEFT
		; {position store} {amount}
		; amount value should be positive
		LDA {1}
		SEC
		SBC {2}
		CMP #160
		BCC .fine_pos_move_done
		CLC
		ADC #160
.fine_pos_move_done
		STA {1}
	ENDM

	; FINE_POS_ACTIVATE * FINE_POS_END are provided for completeness - in many projects
	; you'll be able to forgo using these macros and instead use existing calls to WSYNC/HOME/HMCLR
	MAC FINE_POS_ACTIVATE
		STA WSYNC
		STA HMOVE
	ENDM

	MAC FINE_POS_END
		; writing to HMCLR within 24 machine cycles of HMOVE will negate the HMOVE
		STA HMCLR
	ENDM

; -----------------------------------
; USER INPUT
	MAC DISCREET_TRIGGER_PLAYER0
		; return value: BPL on trigger, BMI on no trigger
		LDX INPT4
		BMI .done					; INPT4 is positive if trigger is pressed 
		LDA _STATE_INPT4	; read state of INPT4 from when last read
		; if _STATE_INPT4 is also negative, then there has not been a state change
		; if it is positive then there has been a state change
		; this is opposite to the normal meaning of INPT4 so flip the bits to
		; correct the meaning
		EOR #$FF
.done
		STX _STATE_INPT4	; store state of trigger for next read
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
		LDX	#(VBLANK_SCANLINES-2)
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
		LDA	#2
		STA VBLANK

		LDX	#(OVERSCAN_SCANLINES-1)
.overscan_loop
		STA WSYNC
		DEX
		BNE .overscan_loop
	ENDM

	MAC OVERSCAN_KERNEL_SETUP
		; wait for overscan
		STA WSYNC
		LDA	#2
		STA VBLANK

		__WAIT_SCANLINE_TIMER #OVERSCAN_SCANLINES
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
		LDX	#DISPLAY_SCANLINES
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
;															|------------ cycles ------------|
;										RAM				setup			update		twos		threes		total (typical frame) [3]
; Two	[1]						1byte			5					12.5 (5)	3				-					15.5 (8)
; Three							1byte			5					12.5			-				5					17.5
;	Two & Three [1]		2byte			10				25 (17.5)	3				5					33 (25.5)
; Multi	[2]					1byte			5					19.75			5				7 (9)			29.75
;
;		[1] --> acumulator clobbering version of update in parenthesis
;		[2] --> threes comparison with set clear bit in parenthesis
;		[3] --> typical defined as one update, one twos comparison and one threes comparison


; Multi Count vs Two/Three Count side-effects
; ===========================================
;
; Two							clobbers Accumulator or X register
; Three						clobbers X register
; Multi						clobbers Accumulator


	MAC MULTI_COUNT_SETUP
		LDA #%10000010						; 2
		STA _MULTI_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_UPDATE
		LDA _MULTI_COUNT_STATE		; 3
		BMI .is_negative					; 2/3

.is_positive
		SEC												; 2
		SBC #$1										; 3
		BMI .positive_reset				; 2/3
		EOR #%10000000						; 2
		JMP .store								; 3

.positive_reset
		LDA #%10000010						; 2
		JMP .store								; 3

.is_negative
		EOR #%10000000						; 2
		SEC												; 2
		SBC #$1										; 3
		BPL .store								; 2/3
.negative_reset
		LDA #%00000010						; 2

.store
		STA _MULTI_COUNT_STATE		; 3
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
		;		- branch on BEQ and BNE
		LDA #%10000000						; 2
		AND _MULTI_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_THREE_CMP
		; {1} == 0 -> do NOT set carry bit before subtract
		; {1} == n -> DO set carry bit before subtract

		; result (depends on what you are trying to achieve)
		;		- differentiating between three states:
		;			branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		;		- waiting for third frame
		;			branch ON third frame - BEQ or BMI
		;			branch NOT ON third frame - BNE or BPL

		LDA #%00000011						; 2
		AND _MULTI_COUNT_STATE		; 3

		IF {1} != 0
			SEC											; 2
		ENDIF

		SBC #1										; 2
		; 7 cycles - or 9 if SEC is used
	ENDM

	MAC MULTI_COUNT_SIX_CMP
		; result (waiting for sixth state)
		;		- branch on BEQ (six count); BNE (non six count)
		; (note that you can't differentiate the six states using this multi count method)
		LDA _MULTI_COUNT_STATE			; 3
	ENDM

; -----------------------------------
; TWO/THREE COUNTS
	MAC TWO_COUNT_SETUP_X
		; require 8bit memory address labelled _TWO_COUNT_STATE
		LDX #$1									; 2
		STX _TWO_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_UPDATE_X
		; require 8bit memory address labelled _TWO_COUNT_STATE
		LDX _TWO_COUNT_STATE			; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$1									; 2
.store_cycle_count
		STX _TWO_COUNT_STATE			; 3
		; 12/13 cycles
	ENDM

	MAC TWO_COUNT_CMP_X
		; require 8bit memory address labelled _TWO_COUNT_STATE
		; result - branch on BEQ and BNE
		LDX _TWO_COUNT_STATE			; 3
		; 3 cycles
	ENDM


	MAC TWO_COUNT_SETUP_A
		; require 8bit memory address labelled _TWO_COUNT_STATE
		LDA #$1									; 2
		STA _TWO_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_UPDATE_A
		; require 8bit memory address labelled _TWO_COUNT_STATE
		LDA #%00000001					; 2
		EOR _TWO_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_CMP_A
		; require 8bit memory address labelled _TWO_COUNT_STATE
		; result - branch on BEQ and BNE
		LDA _TWO_COUNT_STATE		; 3
		; 3 cycles
	ENDM


	MAC THREE_COUNT_SETUP_X
		; require 8bit memory address labelled _THREE_COUNT_STATE
		LDX #$2									; 2
		STX _THREE_COUNT_STATE	; 3
		; 5 cycles
	ENDM

	MAC THREE_COUNT_UPDATE_X
		; require 8bit memory address labelled _THREE_COUNT_STATE
		LDX _THREE_COUNT_STATE	; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$2									; 2
.store_cycle_count
		STX _THREE_COUNT_STATE	; 3
		; 12/13 cycles
	ENDM

	MAC THREE_COUNT_CMP_X
		; require 8bit memory address labelled _THREE_COUNT_STATE
		; result - branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		LDX _THREE_COUNT_STATE	; 3
		DEX											; 2
		; 5 cycles
	ENDM

; -----------------------------------
; OTHER (REFERENCE) ROUTINES

	MAC __MULT_3
	; MULTIPLY BY 3
		TSX				; store stack position
		PHA				; stack acumulator value
		ASL				; multiply by 2
		CLC
		ADC 0,X		; add last stack value - cumulative effect is multiplying by three
		TXS				; intentionally clobber SP - resetting stack position
	ENDM
