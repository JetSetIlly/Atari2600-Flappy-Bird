
; version history
; 0.1 - 14/05/2017
;	0.2 - 09/07/2017 - fine positioning routines
;	0.21 - 08/08/2017 - more efficient sleep table (used for coarse positioning)

	include dasm_extra.h

NULL = 0
TRUE = 1
FALSE = 0
SELECT_SWITCH = %00000010

; summary of optional code
; ========================
;
; RANGE_CHECKING -- enforces argument ranges
;
; TYPE_CHECKING -- enforces argument types (where possible)
;
; SIMPLE_MID_CORRECTION -- accounts for activision border when positioning from the middle


; -----------------------------------
; * MACRO DEFINITION LANGUAGE

; macro definition language
; =========================
;
; inputs
; ======
;
; input definitions are distinguised with a leading ">"
;
; arguments are declared ordinally. ie. the first type declaration refers to argument {1}
; 
; . each macro argument is named between braces. eg. {reset address}
;		. note that arguments cannot be referenced by name, only by number. eg {1}
; . the argument type is specified in square brackets. eg. [R]
;	. allowed values (of the argument type) are defined in parenthesis. eg (1..159)
;
; the argument types are as follows (more in the following section)
;	. R -> VCS register
;	. RR -> TIA read register
;	. RW -> TIA write register
; . V -> immediate value
; . A -> absolute address
; . AW -> absolute address (which is in RAM)
;	. VA -> immediate value OR an absolute address
; . Bool -> Boolean value 
;
; allowed values are defined as follows
;	. as a single value (of the type)
; . as a list of discreet values, separated by commas. eg (1,2,3,4)
; . as a range - two values separated by two periods. eg (1..159)
;
;
; inputs (presets)
; =======
; 
; preset requirements are also distinguised with a leading ">"
;
; . CPU register presets are descriptive
;		. A = description
;		. X = description
;		. Y = description
;
;
; input types
; ===========
;
;	R -> VCS register
;	RR -> TIA read register
;	RW -> TIA write register
; RT -> RIOT register
;				. values SHOULD be limited to addresses defined in vcs.h
;				. further limits will be defined by the name of the argument or in free-form comments
;
; V -> immediate value
;				. values can be DASM expressions
;				. values can include a leading # or not
;
; A -> absolute address
;				. values SHOULD be in the cartridge's memory space or console's RAM
;				. ie not in the console's ROM space

; AW -> absolute address
;				. values MUST be in the console's RAM space
;
;	VA -> immediate value OR an absolute address
;				. immediate values MUST include the leading #
;				. rules for A type apply for values that don't include the leading #
;
;
; other requirements
; ==================
; 
; other requirements are distinguished by a leading ! and are descriptive
;
; 
; side effects
; ============
;
; side effects are listed on a single line, distinguished by a leading +
;
; . affected registers/flags denoted by:
;		. A = Accumulator
;		. X = X register
;		. Y = Y register
;		. C = Carry flag
;		. Z = Zero flag
;		. V = Overflow flag
;		. N = Negative flag
;
;
; outputs
; =======
;
; outputs are distinguished by a leading <
; 
; . valid output locations are CPU registers (A, X or Y) or AW arguments (by name)
; . output defintions are descriptive. for example, for a division routine, an output
;		might be: A = remainder
;	. there can be side effects without an output. outputs are side effects that are useful
;
;
; other notes
; ===========
;
; other macro definition notes are distinguished by a leading *
;
;
; timings
; =======
;
; TODO


; -----------------------------------
; DISPLAY CONSTANTS

VBLANK_SCANLINES = $25		; 37
DISPLAY_SCANLINES =	$C0		; 192
OVERSCAN_SCANLINES = $1E	; 30

; -----------------------------------
; POSITION TABLES

	MAC DEF_SIMPLE_POS_TABLE

	PAGE_CHECK

SIMPLE_POS_TABLE .byte <.S00, <.S04, <.S08, <.S12, <.S16, <.S20, <.S24, <.S28, <.S32, <.S36, <.S40, <.S44, <.S48, <.S52

.S52	NOP			; 2
			NOP			; 2
.S48	NOP			; 2
			NOP			; 2
.S44	NOP			; 2
			NOP			; 2
.S40	NOP			; 2
			NOP			; 2
.S36	NOP			; 2
			NOP			; 2
.S32	NOP			; 2
			NOP			; 2
.S28	NOP			; 2
			NOP			; 2
.S24	NOP			; 2
			NOP			; 2
.S20	NOP			; 2
			NOP			; 2
.S16	NOP			; 2
			NOP			; 2
.S12	NOP			; 2
			NOP			; 2
.S08	NOP			; 2
			NOP			; 2
.S04	NOP			; 2
			NOP			; 2
.S00  NOP 0		; 3
			NOP 0		; 3
			NOP 0		; 3
			RTS			; 6

	PAGE_CHECK_END "SIMPLE_POS_TABLE"

	ENDM

	MAC DEF_FINE_POS_TABLE
	; FINE_POS_TABLE maps (0 to 11) onto (-6 to +5)
__FINE_POS_TABLE	HEX 70 60 50 40 30 20 10 00 F0 E0 D0 C0 B0 A0 90
FINE_POS_TABLE = __FINE_POS_TABLE - %11110001
	ENDM

	MAC INIT_SIMPLE_POS_TABLE
		LDA #>SIMPLE_POS_TABLE
		STA __SIMPLE_POS_TABLE_JMP+1
	ENDM



; -----------------------------------
; FINE POSITIONING

	MAC FINE_POS
		; > {reset address} [R]
		; > {position} [V|A]
		; ! FINE_POS_TABLE
		; + AXCZVN

		LDA {2}
		FINE_POS_A {1}
	ENDM

	MAC FINE_POS_A
		; > {reset address} [R]
		; > A = position value in pixels
		; ! FINE_POS_TABLE
		; + AXCZVN

		; find quotient and remainder when dividing by 12
		;  - use the quotient (X register) to coarsly place the "sprite"
		;  - we can then use the horizontal movement registers to fine tune it (using the remainder - acummulator)
		SEC
		STA WSYNC
.coarse_div
		SBC #15										; 2
		BCS .coarse_div						; 2/3
.done_coarse_div

		; adjust fine tuning value into +7/-8 range
		TAY											; 2
		LDA FINE_POS_TABLE,Y		; 4

		; copy fine tuning value (remainder) into appropriate horizontal movement register
		; call FINE_POS_ACTIVATE (or equivalent) later, to perform the movement
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
		; all 3 cycles

		NOP 0		; 3
		NOP 0		; 3
		STA {1}	; 3
	ENDM

	MAC FINE_POS_LEFT
		; > {reset address} [RW]
		; > {position store} [AW]
		; > {offset} [V] (1..159)
		; > {sprite width} [V] (0,1,2,4,8)
		; ! FINE_POS_TABLE
		; + AXCZVN
		; * sprite width is used to prevent sprite from wrapping around the screen

		IFCONST RANGE_CHECKING
			IF {3} < 0 || {3} > (159 - {4})
				DASM_MACRO_ERROR "'FINE_POS_LEFT': value of {3} must be >= 0 and <=", (160 - {4})
			ENDIF
		ENDIF

		LDA #(01 + {3})
		IF {2} != NULL
			STA {2}
		ENDIF
		FINE_POS_A {1}
	ENDM

	MAC FINE_POS_RIGHT
		; > {reset address} [RW]
		; > {position store} [AW]
		; > {offset} [V] (1..159)
		; > {sprite width} [V] (0,1,2,4,8)
		; ! FINE_POS_TABLE
		; + AXCZVN
		; * sprite width is used to prevent sprite from wrapping around the screen

		IFCONST RANGE_CHECKING
			IF {3} < 0 || {3} > (159 - {4})
				DASM_MACRO_ERROR "'FINE_POS_RIGHT': value of {3} must be >= 0 and <=", (160 - {4})
			ENDIF
		ENDIF

		LDA #(160 - {3} - {4})
		IF {2} != 0
			STA {2}
		ENDIF
		FINE_POS_A {1}
	ENDM

	MAC FINE_MOVE_RIGHT
		; > {position store} [AW]
		; > {amount} [VA]
		; > {loop} [bool]
		; > {sprite width} [V] (0,1,2,4,8)
		; + ACZVN

		IFCONST TYPE_CHECKING
			IF {3} != TRUE && {3} != FALSE
				DASM_MACRO_ERROR "'FINE_MOVE_RIGHT': {3} must be TRUE or FALSE"
			ENDIF
		ENDIF

		LDA {1}
		CLC
		ADC {2}

		IF {3} == TRUE
			CMP #160
			BCC .fine_move_done
			SBC #160	; carry is set
.fine_move_done
		ELSE
			CMP #(160 - {4})
			BCC .fine_move_done
			LDA #(160 - {4})
.fine_move_done
		ENDIF
		STA {1}

	ENDM

	MAC FINE_MOVE_LEFT
		; > {position store} [AW]
		; > {amount} [VA]
		; > {loop} [bool]
		; > {sprite width} [V] (0,1,2,4,8)
		; + ACZVN

		IFCONST TYPE_CHECKING
			IF {3} != TRUE && {3} != FALSE
				DASM_MACRO_ERROR "'FINE_MOVE_LEFT': {3} must be TRUE or FALSE"
			ENDIF
		ENDIF

		LDA {1}
		SEC
		SBC {2}

		IF {3} == TRUE
			BCS .fine_move_done
			ADC #160		; carry is clear
.fine_move_done
		ELSE
			BNE .fine_move_done
			LDA #1
.fine_move_done
		ENDIF
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
; SIMPLE POSITIONING

	; these simple positioning routines give a rough and ready approach to positioning
	; sprites. 
	; 
	; see section 7.0 of the "2600 Progammer's Guide" for more information
	;
	; note that very wide sprites will wrap around the screen when positioned at the limits of the screen
	; the limit testing offered by the macros are good for single width sprites (ie. 8 pixels)
	; to turn on the basic limit testing, define RANGE_CHECKING in your code
	
	MAC __SIMPLE_POS_X
		;	> {reset address} [RW]
		; > X = coarse position
		; ! SIMPLE_POS_TABLE
		; ! requires 16 bit memory location called __SIMPLE_POS_TABLE_JMP
		; + ACZVN

		LDA SIMPLE_POS_TABLE,X
		STA __SIMPLE_POS_TABLE_JMP
		JSR .sub
		STA {1}
		JMP .done
.sub
		STA WSYNC
		JMP (__SIMPLE_POS_TABLE_JMP)
.done
	ENDM

	MAC __SIMPLE_POS
		;	> {reset address} [RW]
		; > {machine cycles} [V]
		; + A

		STA WSYNC
		SLEEP #({2} - 1)		; -1 to account for the following STA
		STA {1}							; 3 cycles
	ENDM

	MAC SIMPLE_POS_LEFT
		;	> {reset address} [RW]
		; > {offset} [V]
		; + A

		IFCONST RANGE_CHECKING
			IF {2} < 0 || {2} > 48
				DASM_MACRO_ERROR "'SIMPLE_POS_LEFT': {2} must be >=0 AND <= 48"
			ENDIF
		ENDIF
		__SIMPLE_POS {1}, 21 + {2}
	ENDM

	MAC SIMPLE_POS_MID
		;	> {reset address} [RW]
		; > {offset} [V]
		; + A

		IFCONST RANGE_CHECKING
			IF ({2} < -25) || ({2} > 26)
				DASM_MACRO_ERROR "'SIMPLE_POS_MID': {2} must be >=-25 AND <= 26"
			ENDIF
		ENDIF

		IFCONST SIMPLE_MID_CORRECTION
			__SIMPLE_POS {1}, 44 + {2}
		ELSE
			__SIMPLE_POS {1}, 43 + {2}
		ENDIF

	ENDM

	MAC SIMPLE_POS_RIGHT
		;	> {reset address} [RW]
		; > {offset} [V]
		; + A

		IFCONST RANGE_CHECKING
			IF {2} < 0 || {2} > 50
				DASM_MACRO_ERROR "'SIMPLE_POS_RIGHT': {2} must be >=0 AND <= 50"
			ENDIF
		ENDIF
		__SIMPLE_POS {1}, 69 - {2}
	ENDM


; -----------------------------------
; USER INPUT
	MAC NEW_TRIGGER_CHECK
		; > {player trigger} [V] (0,1)
		; ! 8 bit address __STATE_INPT4 or __STATE_INPT5 
		; + AXCZVN
		; * BPL on trigger, BMI on no trigger

		IFCONST RANGE_CHECKING
			IF {1} != 0 || {1} > 1
				DASM_MACRO_ERROR "'NEW_TRIGGER_CHECK': {1} must be 0 OR 1"
			ENDIF
		ENDIF

		IF {1} == 0
			LDX INPT4
		ELSE 
			LDX INPT5
		ENDIF
		BMI .done					; INPT4 is positive if trigger is pressed 

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
		STA __MULTI_COUNT_STATE		; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_UPDATE
		LDA __MULTI_COUNT_STATE		; 3
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
		STA __MULTI_COUNT_STATE		; 3
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
		AND __MULTI_COUNT_STATE			; 3
		; 5 cycles
	ENDM

	MAC MULTI_COUNT_THREE_CMP
		; {set carry bit before subtract -> boolean}

		; result (depends on what you are trying to achieve)
		;		- differentiating between three states:
		;			branch on BEQ, BMI and BPL - check for equality before positivity (equality implies positivity)
		;		- waiting for third frame
		;			branch ON third frame - BEQ or BMI
		;			branch NOT ON third frame - BNE or BPL

		LDA #%00000011						; 2
		AND __MULTI_COUNT_STATE		; 3

		IF {1} != FALSE
			SEC											; 2
		ENDIF

		SBC #1										; 2
		; 7 cycles - or 9 if SEC is used
	ENDM

	MAC MULTI_COUNT_SIX_CMP
		; result (waiting for sixth state)
		;		- branch on BEQ (six count); BNE (non six count)
		; (note that you can't differentiate the six states using this multi count method)
		LDA __MULTI_COUNT_STATE			; 3
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

