VISIBLE_SCANLINES =	$C0	 ; 192

;-----------------------------------
	mac DEAD_FRAME
		VSYNC_KERNEL
		VBLANK_KERNEL_WAIT

		STA WSYNC
		LDX	#192			; 192 lines in display
.display_loop
		STA WSYNC
		DEX
		BNE .display_loop
		OVERSCAN_KERNEL_EMPTY
	endm
;-----------------------------------


;-----------------------------------
	mac IDLE_WSYNC_TO_VISIBLE_SCREEN_CUSP
		; cusp defined as "3 machine cycles to visible screen"
		; the zero page write to WSYNC adds abother 9 clock counts (3 cycles)
		STA WSYNC
		SLEEP_CLOCK_COUNTS 48
		; 48 clock counts leaves 9 clock counts (3 cycles) for writing to a reset player/missile/ball register
	endm
;-----------------------------------


;-----------------------------------
	mac IDLE_WSYNC_TO_VISIBLE_SCREEN_RIGHT
		STA WSYNC
		SLEEP_CLOCK_COUNTS 215
	endm

	mac IDLE_WSYNC_TO_VISIBLE_SCREEN_MIDDLE
		STA WSYNC
		SLEEP_CLOCK_COUNTS 140
	endm
;-----------------------------------


;-----------------------------------
	; like SLEEP in vcs.h but for clock counts rather than cycles
	mac SLEEP_CLOCK_COUNTS
.CLOCK_COUNTS		SET {1}
		IF .CLOCK_COUNTS < 1
				ECHO "MACRO ERROR: 'SLEEP_CLOCK_COUNTS': Duration must be > 0"
				ERR
		ENDIF
    REPEAT .CLOCK_COUNTS / 6
			SLEEP 2
    REPEND
	endm
;-----------------------------------


;-----------------------------------
	mac WAIT_SCANLINE_TIMER
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
	endm
;-----------------------------------


;-----------------------------------
	mac VSYNC_KERNEL_BASIC
	VERTICAL_SYNC
	endm
;-----------------------------------


;-----------------------------------
	mac VBLANK_KERNEL_BASIC
	LDX	#37			; 30 lines in overscan
.vblank_loop
	STA WSYNC
	DEX
	BNE .vblank_loop
	endm
;-----------------------------------


;-----------------------------------
	mac VBLANK_KERNEL_SETUP
	WAIT_SCANLINE_TIMER 37
	endm


	mac VBLANK_KERNEL_END
.vblank_loop
	LDA INTIM
	BNE .vblank_loop
	; turn beam back on at beginning of horizontal line
	STA WSYNC
	STA VBLANK
	endm
;-----------------------------------


;-----------------------------------
	; empty overscan kernel - useful for when you don't want/need to do
	; anything during the overscan. 
	mac OVERSCAN_KERNEL_BASIC
.overscan_kernel
	; wait for overscan
	STA WSYNC
	LDA	#2
	STA VBLANK

	LDX	#30			; 30 lines in overscan
.overscan_loop
	STA WSYNC
	DEX
	BNE .overscan_loop
	endm
;-----------------------------------


;-----------------------------------
	mac OVERSCAN_KERNEL_SETUP
	; wait for overscan
	STA WSYNC
	LDA	#2
	STA VBLANK

	WAIT_SCANLINE_TIMER 30
	endm


	mac OVERSCAN_KERNEL_END
.overscan_loop
	LDA INTIM
	BNE .overscan_loop
	endm
;-----------------------------------
