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

	include vcs_mdl.txt
	include dasm_extra.h


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

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF {3} < 0 || {3} > (160 - {4})
					DASM_MACRO_ERROR "'FINE_POS_LEFT': value of {3} must be >= 0 and <=", (160 - {4})
				ENDIF

				IF {4} != 0 && {4} != 1 && {4} != 2 && {4} != 4 && {4} != 8
					DASM_MACRO_ERROR "'FINE_POS_LEFT': value of {4} must be 0, 1, 2, 4 or 8"
				ENDIF
			ENDIF
		ENDIF

		LDA #(02 + {3})
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

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF {3} < 0 || {3} > (160 - {4})
					DASM_MACRO_ERROR "'FINE_POS_RIGHT': value of {3} must be >= 0 and <=", (160 - {4})
				ENDIF

				IF {4} != 0 && {4} != 1 && {4} != 2 && {4} != 4 && {4} != 8
					DASM_MACRO_ERROR "'FINE_POS_RIGHT': value of {4} must be 0, 1, 2, 4 or 8"
				ENDIF
			ENDIF
		ENDIF

		LDA #(161 - {3} - {4})
		IF {2} != NULL
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
		; * {sprite width} is unused if loop == FALSE

		IFCONST MDL_TYPE_CHECKING
			IF MDL_TYPE_CHECKING == TRUE
				IF {3} != TRUE && {3} != FALSE
					DASM_MACRO_ERROR "'FINE_MOVE_RIGHT': {3} must be TRUE or FALSE"
				ENDIF
			ENDIF
		ENDIF

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF {4} != 0 && {4} != 1 && {4} != 2 && {4} != 4 && {4} != 8
					DASM_MACRO_ERROR "'FINE_MOVE_RIGHT': value of {4} must be 0, 1, 2, 4 or 8"
				ENDIF
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
		; + ACZVN

		IFCONST MDL_TYPE_CHECKING
			IF MDL_TYPE_CHECKING == TRUE
				IF {3} != TRUE && {3} != FALSE
					DASM_MACRO_ERROR "'FINE_MOVE_LEFT': {3} must be TRUE or FALSE"
				ENDIF
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
	; to turn on the basic limit testing, define MDL_RANGE_CHECKING in your code
	
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
		; + ACZVN

		STA WSYNC
		SLEEP #({2} - 1)		; -1 to account for the following STA
		STA {1}							; 3 cycles
	ENDM

	MAC SIMPLE_POS_LEFT
		;	> {reset address} [RW]
		; > {offset} [V]
		; + ACZVN

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF {2} < 0 || {2} > 48
					DASM_MACRO_ERROR "'SIMPLE_POS_LEFT': {2} must be >=0 AND <= 48"
				ENDIF
			ENDIF
		ENDIF
		__SIMPLE_POS {1}, 21 + {2}
	ENDM

	MAC SIMPLE_POS_MID
		;	> {reset address} [RW]
		; > {offset} [V]
		; + ACZVN
		; * define MDL_SIMPLE_MID_CORRECTION to correct for activision border

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF ({2} < -25) || ({2} > 26)
					DASM_MACRO_ERROR "'SIMPLE_POS_MID': {2} must be >=-25 AND <= 26"
				ENDIF
			ENDIF
		ENDIF

		IFCONST MDL_SIMPLE_MID_CORRECTION
			IF MDL_SIMPLE_MID_CORRECTION == TRUE
				__SIMPLE_POS {1}, 44 + {2}
			ELSE
				__SIMPLE_POS {1}, 43 + {2}
			ENDIF
		ELSE
			__SIMPLE_POS {1}, 43 + {2}
		ENDIF
	ENDM

	MAC SIMPLE_POS_RIGHT
		;	> {reset address} [RW]
		; > {offset} [V]
		; + ACZVN

		IFCONST MDL_RANGE_CHECKING
			IF MDL_RANGE_CHECKING == TRUE
				IF {2} < 0 || {2} > 50
					DASM_MACRO_ERROR "'SIMPLE_POS_RIGHT': {2} must be >=0 AND <= 50"
				ENDIF
			ENDIF
		ENDIF
		__SIMPLE_POS {1}, 69 - {2}
	ENDM



