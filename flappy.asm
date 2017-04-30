
	processor 6502
	include vcs.h
	include macro.h
	include vcs_extra.h

; program constants
BACKGROUND_COLOR		=		$85

	; how often (in frames) each vblank kernel should run
VBLANK_CYCLE_COUNT	=		$3
VBLANK_CYCLE_SPRITE =		$1
VBLANK_CYCLE_PFIELD	=		$2
VBLANK_CYCLE_SPARE	=		$3

	; screen boundries for bird sprite
BIRD_POS_HIGH				=		$C0
BIRD_POS_LOW				=		$10

	; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_CLIMB_START_FRAME		=		$0
FLY_DIVE_START_FRAME		=		$FF
FLY_UP_FRAMES						=		$4
FLY_GLIDE_FRAMES				=		FLY_UP_FRAMES + $3

	; speed of obstacles (speed of flight)
OBSTACLE_SPEED = $10

	; number of scanlines bird sprite should fly up per frame
CLIMB_RATE			=		$4

BIRD_POS_INIT		=		$BF
FLY_FRAME_INIT	=		FLY_DIVE_START_FRAME


; data  - variables
	SEG.U RAM 
	ORG $80			; start of 2600 RAM
VBLANK_CYCLE				ds 1
FIRE_HELD						ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS						ds 1	; between BIRD_POS_HIGH and BIRD_POS_LOW
FLY_FRAME						ds 1	; <0 = dive; <= FLY_UP_FRAMES = climb; <= FLY_GLIDE_FRAMES = glide
SPRITE_ADDRESS			ds 2	; which sprite to use in the display kernel

OBSTACLE_PAUSE			ds 1
OBSTACLE_1_GAP_T		ds 1
OBSTACLE_1_GAP_H		ds 1

; sprite data
	SEG
	ORG $F000		; start of cart ROM
SPRITES
SPRITE_WINGS_UP			HEX	FF 00 00 00 7F 72 60 40 
SPRITE_WINGS_FLAT		HEX	FF 00 00 00 7F 62 00 00
SPRITE_WINGS_DOWN		HEX	FF 40 60 70 7F 02 00 00
SPRITE_LINES				.byte	7
; note: first FF in each sprite is a boundry byte - value is unimportant


; ----------------------------------
; SETUP

setup SUBROUTINE setup
	CLEAN_START

; END - SETUP
; ----------------------------------


; ----------------------------------
; GAME INITIALISATION

game_init SUBROUTINE game_init
	; initialise variables
	LDA #VBLANK_CYCLE_COUNT
	STA VBLANK_CYCLE

	LDA INPT4
	STA FIRE_HELD

	LDA #BIRD_POS_INIT
	STA BIRD_POS

	LDA #FLY_FRAME_INIT
	STA FLY_FRAME

	; sprite to use in display kernel
	LDA #<SPRITES
	STA SPRITE_ADDRESS
	; all sprites are on the $F0 page so no need to change the following in the sprite-vblank kernel
	LDA #>SPRITES
	STA SPRITE_ADDRESS+1

	; obstacles
	LDA #32		; width 
	STA NUSIZ1
	LDA #$50
	STA OBSTACLE_1_GAP_T
	LDA #$20
	STA OBSTACLE_1_GAP_H

	; set background colour
	LDA #BACKGROUND_COLOR
	STA	COLUBK

.position_elements
	; we only need to position elements once. we use the very first frame of the game sequence to do this.

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

	; the ball horizontal movement was just for fine tuning the backstop
	; reset all horizontal before setting the speed of flight (which will persist
	; throughout the game)
	; note that we need to wait at least 24 machine cycles since HMOVE
	; or the motion will not have occurred
	STA HMCLR
	
	; speed of flight
	LDA #OBSTACLE_SPEED
	STA HMM1
	LDA #$0
	STA OBSTACLE_PAUSE

; END - GAME INITIALISATION
; ----------------------------------


; ----------------------------------
; GAME

game SUBROUTINE game

.vertical_loop
	VSYNC_KERNEL
	
; ----------------------------------
; > VBLANK KERNEL

	; frame triage - cycle through vblank kernels every VBLANK_CYCLE frames
	LDX VBLANK_CYCLE
	CPX #VBLANK_CYCLE_SPRITE
	BEQ .frame_player_sprite
	CPX #VBLANK_CYCLE_PFIELD
	BEQ .frame_obstacles
	; fall through 

	; -------------
	; FRAME - SPARE
	DEX
	STX VBLANK_CYCLE
	JMP .end_frame_triage
	; END - FRAME - SPARE
	; -------------

	; -------------
	; FRAME - OBSTACLES
.frame_obstacles
	DEX
	STX VBLANK_CYCLE

	; check collisions
	BIT CXM1FB
	BVS .reset_obstacle_1

	LDA #$0
	STA OBSTACLE_PAUSE

	BIT CXM1P
	BMI .bird_collision

	JMP .end_frame_triage

.reset_obstacle_1
	STA CXCLR
	LDA OBSTACLE_PAUSE
	BNE .obstacle_reset_done
	LDA OBSTACLE_1_GAP_T
	CLC
	ADC #$8
	STA OBSTACLE_1_GAP_T
	LDA #$1
	STA OBSTACLE_PAUSE

.obstacle_reset_done
	JMP .end_frame_triage

.bird_collision
	; TODO: better collision handling
	STA CXCLR
	BRK

	; END - FRAME - OBSTACLES
	; -------------


	; -------------
	; FRAME - PLAYER SPRITE
.frame_player_sprite
	; reset vblank cycle
	LDX #VBLANK_CYCLE_COUNT
	STX VBLANK_CYCLE

	; check fire button
	LDA INPT4
	BMI .continue_anim

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

	; have we been flying up for more than FLY_UP_FRAMES frames ...
	LDA FLY_FRAME
	CMP #FLY_UP_FRAMES
	BPL .glide_test

	; ... not yet, so fly up
	INC FLY_FRAME
	LDA BIRD_POS
	CLC
	ADC #CLIMB_RATE

	; check to see if we've reached top of flying area (ie. top of screen)
	CMP #BIRD_POS_HIGH
	BCS .fly_highest

	; done with flying up
	STA BIRD_POS
	JMP .fly_end

.fly_highest
	LDA #BIRD_POS_HIGH
	STA BIRD_POS
	JMP .fly_end

.glide_test
	; have we been flying/gliding for more than FLY_GLIDE_FRAMES frames ...
	CMP #FLY_GLIDE_FRAMES
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

	; ... actually, we don't want BIRD_POS to fall below BIRD_POS_LOW
	CMP #BIRD_POS_LOW
	BCC	.fly_lowest

	STA BIRD_POS
	DEC FLY_FRAME
	JMP .fly_end

.fly_lowest
	; TODO: test for game mode (easy/hard) and allow a "walking" bird sprite or cause death accordingly
	LDA #BIRD_POS_LOW
	STA BIRD_POS

.fly_end
	; fall through to end_frame_triage

	; END - FRAME - PLAYER SPRITE
	; -------------

.end_frame_triage
	; setup display kernel

	; X register will now contain the current scanline for the duration of the display kernal
	LDX	#VISIBLE_SCANLINES

	; preload Y register with number of sprite lines
	LDY SPRITE_LINES

	; set up horizontal movement
	STA WSYNC
	STA HMOVE

	; wait for end of vblank kernel 
	VBLANK_KERNEL_WAIT
	VBLANK_KERNEL_DONE

; ----------------------------------
; > DISPLAY KERNEL

; X register contain the current scanline for the duration of the display kernal

; Y register contains number of SPRITE_LINES remaining
; note: we need to use Y register because we'll be performing a post-indexed indirect address 
; to set the sprite line

.display_loop
	; interlace sprite and obstacle drawing
	TXA												; 2
	AND #%00000001						; 2
	BEQ .do_obstacle					; 2/3

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

.sprite_done
	JMP .next_scanline

; maximum 76 cycles between STA WSYNC 
; up to this point (including interlace test):
;		30 - last sprite line drawn
;		31 - drawn sprite line
;		15 - sprite has been completed
;		11 - scan line above BIRD_POS
 
; 40 cycles safely available
; (5 cycles used at end of display_loop)
; -----------------------

; -----------------------
.do_obstacle
	TXA
	CMP OBSTACLE_1_GAP_T
	BMI .obstacle_on
	SEC
	SBC OBSTACLE_1_GAP_T
	CMP OBSTACLE_1_GAP_H
	BMI .obstacle_off

.obstacle_on
	LDA #2
	STA ENAM1
	JMP .obstacle_done

.obstacle_off
	LDA #0
	STA ENAM1

.obstacle_done
; -----------------------

.next_scanline
	STA WSYNC
	DEX
	BNE .display_loop


; ----------------------------------
; > OVERSCAN KERNEL

	OVERSCAN_KERNEL_EMPTY
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
