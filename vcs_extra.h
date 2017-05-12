
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
	MAC POSITION_RESET
		STA WSYNC
		SLEEP_CLOCK_COUNTS {2}
		STA {1}
	ENDM

	MAC COURSE_POS_SCREEN_LEFT
		POSITION_RESET {1}, 48
	ENDM

	MAC COURSE_POS_SCREEN_MID
		POSITION_RESET {1}, 137
	ENDM

	MAC COURSE_POS_SCREEN_RIGHT
		POSITION_RESET {1}, 215
	ENDM

	MAC FINE_POS_SCREEN_RIGHT
		POSITION_RESET {1}, 215

		IF {2} == 1
			LDA #$C0	; -64
		ENDIF
		IF {2} == 2
			LDA #$D0	; -48
		ENDIF
		IF {2} == 4
			LDA #$F0	;	-16
		ENDIF
		IF {2} == 8 
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

	MAC ACTIVATE_FINE_TUNE
		STA WSYNC
		STA HMOVE
	ENDM

	MAC END_FINE_TUNE
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
		; original comment 
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
		LDX	#VBLANK_SCANLINES
.vblank_loop
		STA WSYNC
		DEX
		BNE .vblank_loop
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

		LDX	#OVERSCAN_SCANLINES
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
