
	processor 6502
	include vcs.h
	include macro.h
	include vcs_extra.h

; tunables 
; TODO: alter these according to console difficulty setting

OBSTACLE_SPEED		= $10
OBSTACLE_WIDTH		= $30
OBSTACLE_WINDOW		= $20

CLIMB_RATE				=	$4	 ; number of scanlines bird sprite should fly up per frame
CLIMB_FRAMES			=	$5
GLIDE_FRAMES			=	CLIMB_FRAMES + $3

OBSTACLE_DISAPPEAR_SPEED = $20


; program constants

BACKGROUND_COLOR	=	$85

	; how often (in frames) each vblank kernel should run
FRAME_CYCLE_COUNT		=	$3
FRAME_CYCLE_SPRITE	=	$1
FRAME_CYCLE_PFIELD	=	$2
FRAME_CYCLE_SPARE		=	$3

	; screen boundaries for bird sprite
BIRD_HIGH	=		$C0
BIRD_LOW	=		$10

	; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_CLIMB_START_FRAME	=		$0
FLY_DIVE_START_FRAME	=		$FF

BIRD_INIT				=	$BF
FLY_FRAME_INIT	=	FLY_DIVE_START_FRAME


; data  - variables
	SEG.U RAM 
	ORG $80			; start of 2600 RAM
FRAME_CYCLE					ds 1
FIRE_HELD						ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS						ds 1	; between BIRD_HIGH and BIRD_LOW
FLY_FRAME						ds 1	; <0 = dive; <= CLIMB_FRAMES = climb; <= GLIDE_FRAMES = glide
SPRITE_ADDRESS			ds 2	; which sprite to use in the display kernel

OBSTACLE_0_T					ds 1
OBSTACLE_0_B					ds 1
OBSTACLE_1_T					ds 1
OBSTACLE_1_B					ds 1
OBSTACLE_TOP_POINTER	ds 1	; points to OBSTACLE_TOPS

; pre-calculated ENAM0 and ENAM1 - $0 for obstacle/missile "off" - $2 for "on"
OBSTACLE_0_DRAW				ds 1
OBSTACLE_1_DRAW				ds 2

; start of cart ROM
	SEG
	ORG $F000	

; sprite data
SPRITES
SPRITE_WINGS_UP			HEX	FF 00 00 00 7F 72 60 40 
SPRITE_WINGS_FLAT		HEX	FF 00 00 00 7F 62 00 00
SPRITE_WINGS_DOWN		HEX	FF 40 60 70 7F 02 00 00
SPRITE_LINES				.byte	7
; note: first FF in each sprite is a boundry byte - value is unimportant

; table of obstacles
OBSTACLE_TOPS				HEX 20 30 40 50 60 70 80 90
OBSTACLE_TOP_MAX		= 7

; ----------------------------------
; SETUP

setup SUBROUTINE setup
	CLEAN_START

; END - SETUP
; ----------------------------------


; ----------------------------------
; GAME INITIALISATION

game_init SUBROUTINE game_init
	LDA #FRAME_CYCLE_COUNT
	STA FRAME_CYCLE

	LDA INPT4
	STA FIRE_HELD

	LDA #BIRD_INIT
	STA BIRD_POS

	LDA #FLY_FRAME_INIT
	STA FLY_FRAME

	; sprite to use in display kernel
	LDA #<SPRITES
	STA SPRITE_ADDRESS
	; all sprites are on the $F0 page so no need to change the following in the sprite-vblank kernel
	LDA #>SPRITES
	STA SPRITE_ADDRESS+1

	LDA #OBSTACLE_WIDTH
	STA NUSIZ0
	STA NUSIZ1

	; beginning obstacles
	LDY #$3
	STY OBSTACLE_TOP_POINTER
	LDA OBSTACLE_TOPS,Y
	STA OBSTACLE_0_T
	CLC
	ADC #OBSTACLE_WINDOW
	STA OBSTACLE_0_B

	LDY #$5
	STY OBSTACLE_TOP_POINTER
	LDA OBSTACLE_TOPS,Y
	STA OBSTACLE_1_T
	CLC
	ADC #OBSTACLE_WINDOW
	STA OBSTACLE_1_B

	; set background colour
	LDA #BACKGROUND_COLOR
	STA	COLUBK

.position_elements
	; we only need to position elements once. we use the very first frame of the game sequence to do this.
	STA WSYNC

	; make sure beam is off
	LDA	#2
	STA VBLANK

	; reset sprite objects to leftmost of the screen
	; we could use the IDLE_WSYNC_TO_VISIBLE_SCREEN_CUSP macro
	; but resettting anywhere in the horizontal blank will have the
	; same effect
	STA RESP0
	STA RESP1
	STA RESBL

	; draw ball
.draw_ball
	STA ENABL
	LDA #$21
	STA HMBL
	STA WSYNC
	STA HMOVE

	; place obstacle 1 (missile 1) at right most screen edge
	IDLE_WSYNC_TO_VISIBLE_SCREEN_RIGHT
	STA RESM1

	IDLE_WSYNC_TO_VISIBLE_SCREEN_MIDDLE
	STA RESM0

	; the ball horizontal movement was just for fine tuning the backstop
	; reset all horizontal before setting the speed of flight (which will persist
	; throughout the game)
	; note that we need to wait at least 24 machine cycles since HMOVE
	; or the motion will not have occurred
	STA HMCLR
	
	; speed of flight
	LDA #OBSTACLE_SPEED
	STA HMM0
	STA HMM1

; END - GAME INITIALISATION
; ----------------------------------


; ----------------------------------
; GAME

game SUBROUTINE game

.vertical_loop
	VSYNC_KERNEL_BASIC
	
; ----------------------------------
; > VBLANK KERNEL
	VBLANK_KERNEL_SETUP

	; frame triage - cycle through vblank kernels every FRAME_CYCLE frames
	LDX FRAME_CYCLE
	CPX #FRAME_CYCLE_SPRITE
	BEQ .frame_player_sprite
	CPX #FRAME_CYCLE_PFIELD
	BEQ .frame_obstacles
	; fall through 

	; -------------
	; FRAME - SPARE
	JMP .end_frame_triage
	; END - FRAME - SPARE
	; -------------

	; -------------
	; FRAME - OBSTACLES
.frame_obstacles
	; check collisions
	BIT CXM0FB
	BVS .reset_obstacle_0
	BIT CXM1FB
	BVS .reset_obstacle_1

	; a frame has occurred without collision - reset speed and size of both obstacles (missiles)
	LDA #OBSTACLE_SPEED
	STA HMM0
	STA HMM1
	LDA #OBSTACLE_WIDTH
	STA NUSIZ0
	STA NUSIZ1

	; check bird collision
	BIT CXM1P
	BMI .bird_collision
	BIT CXM0P
	BVS .bird_collision

	JMP .end_frame_triage

; TODO: make reset_obstacle_0 and reset_obstacle_1 more efficient
.reset_obstacle_0
	; get new top for obstacle 0
	LDY OBSTACLE_TOP_POINTER
	LDA OBSTACLE_TOPS,Y
	STA OBSTACLE_0_T
	CLC
	ADC #OBSTACLE_WINDOW
	STA OBSTACLE_0_B

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_DISAPPEAR_SPEED
	STA HMM0
	LDA #0
	STA NUSIZ0
	JMP .obstacle_reset_done

.reset_obstacle_1
	; get new top for obstacle 1
	LDY OBSTACLE_TOP_POINTER
	LDA OBSTACLE_TOPS,Y
	STA OBSTACLE_1_T
	CLC
	ADC #OBSTACLE_WINDOW
	STA OBSTACLE_1_B

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_DISAPPEAR_SPEED
	STA HMM1
	LDA #0
	STA NUSIZ1

.obstacle_reset_done
	JMP .end_frame_triage

.bird_collision
	; TODO: better collision handling
	BRK

	; END - FRAME - OBSTACLES
	; -------------


	; -------------
	; FRAME - PLAYER SPRITE
.frame_player_sprite
	; check fire button
	LDA INPT4
	BMI .continue_anim

	; change next obstacle - we won't be using this until next frame so
	; we've deferred limiting  the pointer to the overscan kernel
	INC OBSTACLE_TOP_POINTER

	; do nothing if fire is being held
	LDX FIRE_HELD
	BPL .continue_anim

	; start new fly animation
	LDA #FLY_CLIMB_START_FRAME
	STA FLY_FRAME

	; flip sprite
	LDA SPRITE_ADDRESS
	CMP #<SPRITE_WINGS_DOWN
	BEQ .flip_sprite_use_flat

	LDA #<SPRITE_WINGS_DOWN
	STA SPRITE_ADDRESS
	JMP .flip_sprite_end

.flip_sprite_use_flat
	LDA #<SPRITE_WINGS_FLAT
	STA SPRITE_ADDRESS
.flip_sprite_end


.continue_anim
	STA FIRE_HELD
	LDA FLY_FRAME
	BMI .fly_down

	; have we been flying up for more than CLIMB_FRAMES frames ...
	LDA FLY_FRAME
	CMP #CLIMB_FRAMES
	BPL .glide_test

	; ... not yet, so fly up
	INC FLY_FRAME
	LDA BIRD_POS
	CLC
	ADC #CLIMB_RATE

	; check to see if we've reached top of flying area (ie. top of screen)
	CMP #BIRD_HIGH
	BCS .fly_highest

	; done with flying up
	STA BIRD_POS
	JMP .fly_end

.fly_highest
	LDA #BIRD_HIGH
	STA BIRD_POS
	JMP .fly_end

.glide_test
	; have we been flying/gliding for more than GLIDE_FRAMES frames ...
	CMP #GLIDE_FRAMES
	BPL .end_glide

	; make sure we're using the flat wing sprite when gliding
	LDA #<SPRITE_WINGS_FLAT
	STA SPRITE_ADDRESS

	INC FLY_FRAME
	JMP .fly_end

.end_glide
	LDA #FLY_DIVE_START_FRAME
	STA FLY_FRAME

.fly_down
	; use fly down sprite
	LDA #<SPRITE_WINGS_UP
	STA SPRITE_ADDRESS

	LDA BIRD_POS
	CLC
	ADC FLY_FRAME

	; check to see FALL RATE will take BIRD_POS below zero ...
	BCC	.fly_lowest

	; ... actually, we don't want BIRD_POS to fall below BIRD_LOW
	CMP #BIRD_LOW
	BCC	.fly_lowest

	STA BIRD_POS
	DEC FLY_FRAME
	JMP .fly_end

.fly_lowest
	; TODO: test for game mode (easy/hard) and allow a "walking" bird sprite or cause death accordingly
	LDA #BIRD_LOW
	STA BIRD_POS

.fly_end
	; fall through to end_frame_triage

	; END - FRAME - PLAYER SPRITE
	; -------------

.end_frame_triage
	; reset collision flags every frame
	STA CXCLR

	; setup display kernel

	; first scanline always contains the obstacles
	; - whether or not the next scanline will contain the obstacle
	; is decided in the display kernel, at the end of the preceeding scanline
	LDA #$2
	STA OBSTACLE_0_DRAW
	STA OBSTACLE_1_DRAW

	; X register will now contain the current scanline for the duration of the display kernal
	LDX	#VISIBLE_SCANLINES

	; preload Y register with number of sprite lines
	LDY SPRITE_LINES

	; we need the number of scanlines in the accumulator at the beginning of the .display_loop
	; - saving precious cycles for the HBLANK
	TXA

	; set up horizontal movement
	STA WSYNC
	STA HMOVE

	; wait for end of vblank kernel 
	VBLANK_KERNEL_END

; ----------------------------------
; > DISPLAY KERNEL

; X register contain the current scanline for the duration of the display kernal

; Y register contains number of SPRITE_LINES remaining
; note: we need to use Y register because we'll be performing a post-indexed indirect address 
; to set the sprite line

.display_loop
	; interlace sprite and obstacle drawing
	AND #%00000001						; 2
	BEQ .do_sprite						; 2/3

; -----------------------
	LDA OBSTACLE_0_DRAW				; 3
	STA ENAM0									; 3
	LDA OBSTACLE_1_DRAW				; 3
	STA ENAM1									; 3

	; maximum 76 cycles between STA WSYNC 
	; up to this point (including interlace test):
	;  19 cycles
	; (first iteration uses only 16 cycles)

	JMP .pre_calc
; -----------------------


; -----------------------
.do_sprite
	; if we're at scan line number BIRD_POS (ie. where the bird is) - turn on the sprite
	CPX BIRD_POS							; 2
	BCS .sprite_done					; 2/3

	TYA												; 2
	BMI .sprite_done					; 2/3
	BEQ	.sprite_off						; 2/3

	LDA (SPRITE_ADDRESS),Y		; 5
	; idle cycles to prevent writing to GRP0 mid-colour-cycle
	NOP												; 2
	STA GRP0									; 3
	DEY												; 2
	JMP .sprite_done					; 3

.sprite_off
	STY GRP0									; 3
	DEY												; 2

	; maximum 76 cycles between STA WSYNC 
	; up to this point (including interlace test):
	;		23 (19) - last sprite line drawn
	;		27 (24) - drawn sprite line
	;		15 (12) - scan line below BIRD_POS
	;		12 (9) - scan line above BIRD_POS
	; 49 cycles safely available

.sprite_done
	JMP .next_scanline				; 3
; -----------------------

.pre_calc
	; we dont' have time in the HBLANK to do all this comparing and branching
	; so we "precalc" the results now in time for the next scanline
	; we're using precious visible scanline cycles of course, but all the important
	; "drawing" is done during the hblank and the first part of the visible screen

.do_obstacle_0
	CPX OBSTACLE_0_T					; 3
	BCC .obstacle_0_on				; 2/3
	CPX OBSTACLE_0_B					; 3
	BCC .obstacle_0_off				; 2/3

.obstacle_0_on
	LDA #$2
	STA OBSTACLE_0_DRAW
	JMP .do_obstacle_1				; 3

.obstacle_0_off
	LDA #0										; 2
	STA OBSTACLE_0_DRAW

.do_obstacle_1
	CPX OBSTACLE_1_T					; 3
	BCC .obstacle_1_on				; 2/3
	CPX OBSTACLE_1_B					; 3
	BCC .obstacle_1_off				; 2/3

.obstacle_1_on
	LDA #$2
	STA OBSTACLE_1_DRAW
	JMP .obstacles_done				; 3

.obstacle_1_off
	LDA #0										; 2
	STA OBSTACLE_1_DRAW

.obstacles_done

.next_scanline
	; we need the number of scanlines in the accumulator at the beginning of the .display_loop
	; decrement and move transfer with TXA - saving precious cycles for the HBLANK
	DEX												
	TXA												

	STA WSYNC									; 3

	; status flags from DEX have survived so this branch is based on that
	BNE .display_loop					; 2/3

; ----------------------------------
; > OVERSCAN KERNEL

.overscan_kernel
	OVERSCAN_KERNEL_SETUP

	; update frame cycle
	LDX FRAME_CYCLE
	DEX
	BNE .store_frame_cycle
	LDX #FRAME_CYCLE_COUNT
.store_frame_cycle
	STX FRAME_CYCLE

	; limit OBSTACLE_TOP_POINTER to maximum value
	LDY OBSTACLE_TOP_POINTER
	CPY #OBSTACLE_TOP_MAX
	BCC .limit_obstacle_top
	LDY #$0
.limit_obstacle_top
	STY OBSTACLE_TOP_POINTER

	OVERSCAN_KERNEL_END

	JMP .vertical_loop

; END - GAME
; ----------------------------------


; ----------------------------------
; MACHINE INITIALISATION 

initialisation SUBROUTINE initialisation

	ORG $FFFA
	.word setup		; NMI
	.word setup		; RESET
	.word setup		;	IRQ

; END - MACHINE INITIALISATION 
; ----------------------------------
