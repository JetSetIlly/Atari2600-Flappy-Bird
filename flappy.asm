
	processor 6502
	include vcs.h
	include macro.h

	; program constants

; how often (in frames) each vblank kernel should run
VBLANK_CYCLE_COUNT	EQU		$3

; screen boundries for bird sprite
BIRD_POS_HIGH			EQU		$BF
BIRD_POS_LOW			EQU		$9

; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_UP_FRAMES			EQU		$5
FLY_GLIDE_FRAMES	EQU		FLY_UP_FRAMES + $3

; number of pixels bird sprite should fly up per frame
CLIMB_RATE				EQU		$4

FALL_RATE_INIT		EQU		$1
BIRD_POS_INIT			EQU		$BF
FLY_ANIM_INIT			EQU		$0

BACKGROUND_COLOR	EQU		$85
NUM_SCANLINES			EQU		192

	; data  - variables
	SEG.U RAM 
	ORG $80
VBLANK_CYCLE				ds 1
FIRE_HELD						ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS						ds 1	; between BIRD_POS_HIGH and BIRD_POS_LOW
FLY_FRAME						ds 1	; fly direction -> 0 = dive, 0..FLY_UP_FRAMES = climb, FLY_UP_FRAMES..FLY_GLIDE_FRAMES = glide
FALL_RATE						ds 1

	; sprite data
	SEG
	ORG $F000
SPRITE_DIVE				.byte	$7F,$72,$60,$40,$00
SPRITE_GLIDE			.byte	$00,$7F,$62,$00,$00
SPRITE_CLIMB			.byte	$40,$60,$70,$7F,$02
SPRITE_LINES			.byte	$4

; ----------------------------------
; SETUP

setup
	CLEAN_START

	; initialise variables
	LDA #BIRD_POS_INIT
	STA BIRD_POS

	LDA INPT4
	STA FIRE_HELD

	LDA #FLY_ANIM_INIT
	STA FLY_FRAME

	LDA #FALL_RATE_INIT
	STA FALL_RATE

	LDA #VBLANK_CYCLE_COUNT
	STA VBLANK_CYCLE

	; set background colour
	LDA #BACKGROUND_COLOR
	STA	COLUBK

; END - SETUP
; ----------------------------------


; ----------------------------------
; VSYNC KERNEL

vertical_loop
	VERTICAL_SYNC

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
	LDA	#43
	STA TIM64T

; END - VSYNC KERNEL
; ----------------------------------

	
; ----------------------------------
; VBLANK KERNEL

	; frame triage
	LDY VBLANK_CYCLE
	DEY
	BEQ frame_player_sprite
	STY VBLANK_CYCLE
	JMP end_frame_triage

	; FRAME - PLAYER SPRITE
frame_player_sprite
	; reset frame cycle
	LDY #VBLANK_CYCLE_COUNT
	STY VBLANK_CYCLE

	; check fire button
	LDA INPT4
	BMI joy_fire_off

	; fire is pressed

	; do nothing if fire was pressed during previous frame
	LDY FIRE_HELD
	BPL joy_fire_held

	; record held state
	STA FIRE_HELD

	; start new fly animation
	LDA #1
	STA FLY_FRAME
	JMP do_anim

joy_fire_off
	STA FIRE_HELD

	LDA FLY_FRAME
	BNE do_anim

	JMP fly_down

joy_fire_held
	LDA FLY_FRAME
	BEQ fly_down 
	; fall through

do_anim
	; have we been flying up for more than FLY_UP_FRAMES frames ...
	CMP #FLY_UP_FRAMES
	BPL glide_test

	; ... not yet, so fly up
	INC FLY_FRAME
	LDA BIRD_POS
	CLC
	ADC #CLIMB_RATE

	; check to see if we've reached top of flying area (ie. top of screen)
	CMP #BIRD_POS_HIGH
	BCS fly_highest

	; done with flying up
	STA BIRD_POS
	JMP fly_end

fly_highest
	LDA #BIRD_POS_HIGH
	STA BIRD_POS
	JMP fly_end

glide_test
	; have we been flying/gliding for more than FLY_GLIDE_FRAMES frames ...
	CMP #FLY_GLIDE_FRAMES
	BPL end_glide

	INC FLY_FRAME
	JMP fly_end

end_glide
	LDA #FLY_ANIM_INIT
	STA FLY_FRAME
	LDA #FALL_RATE_INIT
	STA FALL_RATE

fly_down
	LDA BIRD_POS
	SEC
	SBC FALL_RATE

	; check to see FALL RATE will take BIRD_POS below zero ...
	BCC	fly_lowest

	; ... actually, we don't want BIRD_POS to fall below BIRD_POS_LOW
	CMP #BIRD_POS_LOW
	BCC	fly_lowest

	STA BIRD_POS
	INC FALL_RATE		; no upper limit to FALL_RATE
	JMP fly_end

fly_lowest
	; TODO: test for game mode (easy/hard) and allow a "walking" bird sprite or cause death accordingly
	LDA #BIRD_POS_LOW
	STA BIRD_POS

fly_end
	; END - FRAME - PLAYER SPRITE

end_frame_triage
	; setup display kernel

	; Y register will now contain the current scanline for the duration of the display kernal
	LDY	#NUM_SCANLINES

	; set up horizontal movement
	LDA #0
	STA WSYNC
	STA HMOVE

	; wait for end of 37 scanlines of vblank
	; timer set in VSYNC KERNEL above
vblank_loop
	LDA INTIM
	BNE vblank_loop

	STA WSYNC
	STA VBLANK				; turn beam back on (writing zero)

; END - VBLANK KERNEL
; ----------------------------------

; ----------------------------------
; DISPLAY KERNEL

; Y register contain the current scanline for the duration of the display kernal

; X register contains current sprite line once current scan line equals BIRD_POS
;			in between jumps to sprite_on and sprite_off (which will span many iterations
;			of sprite_display_kernel_loop)

sprite_display_kernel_loop
	; wait for beginning of horizontal scan
	STA WSYNC

	; reset player sprite at beginning of scan line
	; note we'that we don't need anything more sophisticated than
	; this because the bird never moves horizontally
	STA RESP0
	; naive wait method (four cycles) for RESP0 to be acknowledged
	; TODO: do something useful here that doesn't involve the player sprite
	NOP
	NOP
	NOP
	NOP

	; if we're at scan line number BIRD_POS (ie. where the bird is) - turn on the sprite
	CPY BIRD_POS
	BEQ sprite_on

	; the bird sprite is more than one scanline tall.  check to see if we're still in
	; the sprite's range and move sprite line count to accumulator and branch accordingly
	; NOTE: vblank kernel may leave junk in X register, which will cause odd sprite drawing
	; if so, vblank kernel should reset X to #NUM_SCANLINES
	TXA
	BMI sprite_off
	JMP sprite_line	

sprite_on
	; number of lines in the sprite
	LDX	SPRITE_LINES

sprite_line
	LDA FLY_FRAME
	BEQ sprite_dive
	CMP FLY_UP_FRAMES
	BPL sprite_glide
	
	; sprite climb
	LDA SPRITE_CLIMB,X
	STA GRP0
	JMP sprite_line_next

sprite_dive
	LDA SPRITE_DIVE,X
	STA GRP0
	JMP sprite_line_next

sprite_glide
	LDA SPRITE_GLIDE,X
	STA GRP0

sprite_line_next
	DEX
	JMP next_kernel_loop

sprite_off
	LDA #0
	STA GRP0

next_kernel_loop
	DEY		; next scanline
	BNE sprite_display_kernel_loop 
	;JMP overscan_kernel	

; END - SPRITE LOOP

; END - DISPLAY KERNEL
; ----------------------------------


; ----------------------------------
; OVERSCAN KERNEL

overscan_kernel

	; wait for overscan
	STA WSYNC
	LDA	#2
	STA VBLANK

	LDY	#30			; 30 lines in overscan
overscan_loop

	; TODO custom overscan kernel

	STA WSYNC
	DEY
	BNE overscan_loop

; END - OVERSCAN KERNEL
; ----------------------------------

	JMP vertical_loop

	ORG $FFFA
	.word setup		; NMI
	.word setup		; RESET
	.word setup		;	IRQ
