
	mac IDLE_WSYNC_TO_VISIBLE_SCREEN
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
	endm

	mac IDLE_ONE_COLOR_CYCLE
		NOP
		NOP
	endm

COLOR_COUNTS_PER_SCANLINE = 228
CYCLES_PER_COLOUR_COUNT = 3
CYCLES_PER_SCANLINE = COLOR_COUNTS_PER_SCANLINE / CYCLES_PER_COLOUR_COUNT
VBLANK_KERNEL_TIMER_SET_IN_CYCLES = 5
VBLANK_KERNEL_WAIT_LOOP = 6

; define VBLANK_SCANLINES somewhere in your source

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
