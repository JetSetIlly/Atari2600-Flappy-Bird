
	mac IDLE_WSYNC_TO_VISIBLE_SCREEN_CUSP
		; cusp defined as "3 machine cycles to visible screen"

		; the zero page write to WSYNC adds abother 9 clock counts (3 cycles)
		STA WSYNC
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		IDLE_6_CLOCK_COUNTS
		; total number of "idle" clock counts to 57
		; leaving another 9 clock counts (3 cycles) for writing to a reset player/missile/ball register
		; just in time for the visible screen
	endm

	mac IDLE_6_CLOCK_COUNTS
		NOP			; 2 cycles = 6 clock counts
	endm

CLOCK_COUNTS_PER_SCANLINE = 228
CLOCK_COUNTS_PER_CYCLE = 3
CYCLES_PER_SCANLINE = CLOCK_COUNTS_PER_SCANLINE / CLOCK_COUNTS_PER_CYCLE
VBLANK_KERNEL_TIMER_SET_IN_CYCLES = 5
VBLANK_KERNEL_WAIT_LOOP = 6
VBLANK_SCANLINES = 37	; 37 is the textbook value

VBLANK_TIMER_VAL = (CYCLES_PER_SCANLINE * VBLANK_SCANLINES - VBLANK_KERNEL_TIMER_SET_IN_CYCLES - VBLANK_KERNEL_WAIT_LOOP) / 64

	mac VSYNC_KERNEL
	VERTICAL_SYNC
	VBLANK_KERNEL_SETUP
	endm

	mac VBLANK_KERNEL_SETUP
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
	LDA	#VBLANK_TIMER_VAL
	STA TIM64T
	endm

	mac VBLANK_KERNEL_WAIT
vblank_loop
	LDA INTIM
	BNE vblank_loop
	endm
