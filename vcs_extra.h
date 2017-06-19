
; version 0.1 - 14/5/2017

VBLANK_SCANLINES = $25		; 37
VISIBLE_SCANLINES =	$C0		; 192
OVERSCAN_SCANLINES = $1E	; 30

;-----------------------------------
	MAC DEAD_FRAME
		VSYNC_KERNEL_BASIC
		VBLANK_KERNEL_BASIC

		STA WSYNC
		LDX	#VISIBLE_SCANLINES
.display_loop
		STA WSYNC
		DEX
		BNE .display_loop

		OVERSCAN_KERNEL_BASIC
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


; Multi Count vs Two/Three Count comparison
; -----------------------------------------
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
; -----------------------------------------
;
; Two							clobbers Accumulator or X register
; Three						clobbers X register
; Multi						clobbers Accumulator


;-----------------------------------
	MAC MULTI_COUNT_SETUP
		LDA #%10000010						; 2
		STA MULTI_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_UPDATE
		LDA MULTI_COUNT_STATE			; 3
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
		STA MULTI_COUNT_STATE			; 3
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
		AND MULTI_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_THREE_CMP
		; {1} == 0 -> do NOT set carry bit before subtract
		; {1} == n -> DO set carry bit before subtract

		; result (depends on what you are trying to achieve)
		;		- differentiating between three states:
		;			branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		;		- waiting for third frame
		;			branch on BMI or BNE (BEQ or BPL does not work)

		LDA #%00000011						; 2
		AND MULTI_COUNT_STATE			; 3

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
		LDA MULTI_COUNT_STATE			; 3
	ENDM
;-----------------------------------


;-----------------------------------
	MAC TWO_COUNT_SETUP_X
		; requires a 8bit memory address labelled TWO_COUNT_STATE
		LDX #$1									; 2
		STX TWO_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_UPDATE_X
		LDX TWO_COUNT_STATE			; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$1									; 2
.store_cycle_count
		STX TWO_COUNT_STATE			; 3
		; 12/13 cycles
	ENDM

	MAC TWO_COUNT_CMP_X
		; result - branch on BEQ and BNE
		LDX TWO_COUNT_STATE			; 3
		; 3 cycles
	ENDM


	MAC TWO_COUNT_SETUP_A
		; requires a 8bit memory address labelled TWO_COUNT_STATE
		LDA #$1									; 2
		STA TWO_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_UPDATE_A
		LDA #%00000001					; 2
		EOR TWO_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC TWO_COUNT_CMP_A
		; result - branch on BEQ and BNE
		LDA TWO_COUNT_STATE			; 3
		; 3 cycles
	ENDM


	MAC THREE_COUNT_SETUP_X
		; requires a 8bit memory address labelled THREE_COUNT_STATE
		LDX #$2									; 2
		STX THREE_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC THREE_COUNT_UPDATE_X
		LDX THREE_COUNT_STATE		; 3
		DEX											; 2
		BPL .store_cycle_count	; 2/3
		LDX #$2									; 2
.store_cycle_count
		STX THREE_COUNT_STATE		; 3
		; 12/13 cycles
	ENDM

	MAC THREE_COUNT_CMP_X
		; result - branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		LDX THREE_COUNT_STATE		; 3
		DEX											; 2
		; 5 cycles
	ENDM
;-----------------------------------

