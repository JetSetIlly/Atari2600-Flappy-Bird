
	processor 6502
	include vcs.h
	include macro.h
	include vcs_extra.h


; program tunables 
; TODO: alter these according to console difficulty setting

OBSTACLE_SPEED		= $10
OBSTACLE_WINDOW		= $1F
CLIMB_RATE				=	$4	 ; number of scanlines bird sprite should fly up per frame
CLIMB_FRAMES			=	$5
GLIDE_FRAMES			=	CLIMB_FRAMES + $2

; tunables - but not related to difficulty

; colours
FOREST_BACKGROUND				=	$D2
FOREST_COLOR						= $E0
BIRD_COLOR							= $00
FOLIAGE_COLOR						= $D0
SWAMP_BACKGROUND				= $B0
SWAMP_COLOR							= $B2
SCORING_BACKGROUND			= $00
SCORE_COLOR							= $0E
HISCORE_COLOR						= $F6

; program constants

; screen boundaries for bird sprite
BIRD_HIGH				=	VISIBLE_LINE_PLAYAREA
BIRD_LOW				=	$0C

; death rates
DEATH_SPIRAL_SPEED		= $E0 ; speed at which player sprite moves foward during death spiral
DEATH_DROWNING_LEN		= $0C ; should be the same BIRD_LOW

; NUSIZ0 and NUSIZ1 value - first four bits set obstacle width
; - last four bits set the player sprite size
OBSTACLE_WIDTH				= %00100000		; quad width
OBSTACLE_WIDTH_BRANCH	= %00110000		; octuple width
WINGS_SIZE						= %00000101   ; single instance, double width
HEAD_SIZE							= %00000000   ; single instance, single width
SCORE_DIGITS_SIZE			= %00000101   ; single instance, double width
WINGS_NUSIZ_VAL				= OBSTACLE_WIDTH | WINGS_SIZE
TREE_NUSIZ_VAL				= OBSTACLE_WIDTH | HEAD_SIZE
BRANCH_NUSIZ_VAL			= OBSTACLE_WIDTH_BRANCH | HEAD_SIZE

; score area of the screen doesn't have any obstacles but we'll set it anyway
; so that we don't have to reset at the beginning of the next frame
SCORE_NUSIZ_VAL				= OBSTACLE_WIDTH | SCORE_DIGITS_SIZE

; speed/width of obstacle when it's being reset
; prevents extra collisions (which cause extra scoring)
OBSTACLE_EXIT_SPEED		= $20

; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_CLIMB_START_FRAME	=	$0
FLY_DIVE_START_FRAME	=	$FF

BIRD_POS_INIT					=	BIRD_HIGH / 4 * 3
FLY_FRAME_INIT				=	FLY_DIVE_START_FRAME

; visible display area
VISIBLE_LINES_FOLIAGE			= $20
VISIBLE_LINES_PER_FOLIAGE	= VISIBLE_LINES_FOLIAGE / 8
VISIBLE_LINES_SWAMP				= $03
VISIBLE_LINE_PLAYAREA			= VISIBLE_SCANLINES - VISIBLE_LINES_FOLIAGE - VISIBLE_LINES_SWAMP - VISIBLE_LINES_SCOREAREA
VISIBLE_LINES_SCOREAREA		= DIGIT_LINES + $04
; the extra $04 scanlines in the score area are:
; * one at the start of the subroutine
; * two position resets
; * and another one because DIGIT_LINES breaks on -1 not 0 (BMI instead of BEQ)

; play state
PLAY_STATE_PLAY					= $00
PLAY_STATE_DEATH_SPIRAL	= $FF
PLAY_STATE_DEATH_DROWN	= $FE

; data - variables
	SEG.U RAM 
	ORG $80			; start of 2600 RAM
MULTI_COUNT_STATE				ds 1	; counts rounds of twos and threes
PLAY_STATE							ds 1	; state of play - zero is normal, negative is death
FIRE_HELD								ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS								ds 1	; between BIRD_HIGH and BIRD_LOW

; which bird/detail sprite to use in the display kernel
BIRD_SPRITE_ADDRESS			ds 2
DETAIL_SPRITE_ADDRESS			ds 2

; FLY_FRAME's meaning changes depending on PLAY_STATE
;
; if PLAY_STATE == PLAY_STATE_PLAY
;	then
;		<0 = dive; <= CLIMB_FRAMES = climb; <= GLIDE_FRAMES = glide
;
; if PLAY_STATE == PLAY_STATE_DEATH_SPIRAL
;	then
;		FLY_FRAME indexes DEATH_SPIRAL
;
; if PLAY_STATE == PLAY_STATE_DEATH_DROWN
;	then
;		FLY_FRAME counts number of frames to game reset
FLY_FRAME						ds 1

; value for next foliage (playfield) - points to FOLIAGE
NEXT_FOLIAGE				ds 1

; obstacles
OB_0_START					ds 1
OB_0_END						ds 1
OB_1_START					ds 1
OB_1_END						ds 1
OB_1_BRANCH					ds 1

; current speed of obstacle
CURRENT_OB_0_SPEED		ds 1
CURRENT_OB_1_SPEED		ds 1

; start value for next obstacle - points to OBSTACLES
NEXT_OBSTACLE					ds 1
NEXT_BRANCH						ds 1

; pre-calculated ENAM0 and ENAM1 - $0 for obstacle/missile "off" - $2 for "on"
OBSTACLE_0_DRAW				ds 1
OBSTACLE_1_DRAW				ds 1
OBSTACLE_1_NUSIZ			ds 1

; pre-calculated GRP0
BIRD_DRAW							ds 1
DETAIL_DRAW						ds 1

; background trees
FOREST_MID_0					ds 1
FOREST_MID_1					ds 1
FOREST_MID_2					ds 1

; player score
SCORE									ds 1
HISCORE								ds 1

; base address of current number being drawn
DIGIT_ADDRESS_0				ds 2
DIGIT_ADDRESS_1				ds 2


	SEG
	ORG $F000		; start of cart ROM

; sprite data
WINGS_SPRITE
WINGS_UP				HEX	00 00 00 00 30 70 60 40 
WINGS_FLAT			HEX	00 00 00 00 70 40 00 00
WINGS_DOWN			HEX	00 40 60 70 70 00 00 00
DETAIL_SPRITE		HEX 00 00 00 00 10 0E 0C 10
SPRITE_LINES		=	7

; NOTE: first 00 in each sprite is a boundry byte - used to turn off sprite

; table of obstacles (the lower the number, the lower the obstacle)
OBSTACLES				HEX 15 25 35 45 55 65
OBSTACLES_CT		= 6

; table of branches (the lower the number, the lower the obstacle)
; note: we test for branch on the odd scanlines of the play area so these values should be odd
BRANCHES				HEX 19 27 31 39 45 59 71
BRANCHES_CT			= 7

; foliage - playfield data
; (see ".display_foliage" subroutine for full and laboured explanation)
FOLIAGE
FOLIAGE_1	.byte %01100000, %10011010, %00111010, %10010000, %00110101, %11010001, %01010000, %01010110, %00110011, %10101000
FOLIAGE_2	.byte %10100110, %00010110, %10010110, %10011010, %00111010, %00101011, %01101101, %01100110, %01011001, %11001101
FOLIAGE_3	.byte %00101010, %01101100, %00100010, %10011010, %00111010, %00110011, %01101101, %01100110, %01011001, %10110101
FOLIAGE_CHAOS_CYCLE	= 7

; background forest - initial values
FOREST_MID_0_INIT	.byte %00100000
FOREST_MID_1_INIT	.byte %00011000
FOREST_MID_2_INIT	.byte %00001000
FOREST_STATIC_0		.byte %10000000
FOREST_STATIC_1		.byte %00100000	
FOREST_STATIC_2		.byte %10010000

; digits - used for scoring
DIGITS
DIGIT_0	HEX 3C 24 24 24 3C
DIGIT_1	HEX 08 08 08 18 08
DIGIT_2	HEX 3C 20 18 04 3C
DIGIT_3	HEX 3C 04 08 04 3C
DIGIT_4	HEX 08 3C 28 20 20
DIGIT_5	HEX 38 04 3C 20 3C
DIGIT_6	HEX 3C 24 3C 20 20
DIGIT_7	HEX 04 04 0C 04 3C
DIGIT_8	HEX 3C 24 3C 24 3C
DIGIT_9	HEX 04 04 3C 24 3C
DIGIT_LINES	= 4
DIGIT_TABLE	.byte <DIGIT_0, <DIGIT_1, <DIGIT_2, <DIGIT_3, <DIGIT_4, <DIGIT_5, <DIGIT_6, <DIGIT_7, <DIGIT_8, <DIGIT_9

DEATH_SPIRAL .byte 1, 2, 3, 4, 0, 0, 0, 0, -1, -2, -4, -6, -8, -10
DEATH_SPIRAL_LEN = 13


; ----------------------------------
; SETUP

setup SUBROUTINE setup
	CLEAN_START


; ----------------------------------
; TITLE SCREEN

; TODO: finish title screen

title_screen SUBROUTINE title_screen
.vsync
	VSYNC_KERNEL_BASIC

.vblank
	VBLANK_KERNEL_SETUP
	LDA INPT4
	BPL .end_title_screen
	LDX #VISIBLE_SCANLINES
	VBLANK_KERNEL_END

.visible_loop
	STA WSYNC
	DEX
	BNE .visible_loop

.overscan
	OVERSCAN_KERNEL_BASIC
	JMP .vsync

.end_title_screen
	VBLANK_KERNEL_END
	OVERSCAN_KERNEL_BASIC


; ----------------------------------
; GAME INITIALISATION

game_init SUBROUTINE game_init
	; all bird sprites are on the same page ($F0) so no need to change the following in the sprite-vblank kernel
	LDA #>WINGS_SPRITE
	STA BIRD_SPRITE_ADDRESS+1

	; all details sprites are on the same page ($F0) so no need to change the following in the sprite-vblank kernel
	LDA #<DETAIL_SPRITE
	STA DETAIL_SPRITE_ADDRESS
	LDA #>DETAIL_SPRITE
	STA DETAIL_SPRITE_ADDRESS+1

	; DIGIT_ADDRESS_0/1 refers to the current number being drawn
	LDA #<DIGITS
	STA DIGIT_ADDRESS_0
	STA DIGIT_ADDRESS_1
	; all bird sprites are on the $F0 page so no need to change the following in the sprite-vblank kernel
	LDA #>DIGITS
	STA DIGIT_ADDRESS_0+1
	STA DIGIT_ADDRESS_1+1

	; initialise obstacles
	LDY #$3
	LDA OBSTACLES,Y
	STA OB_0_START
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_0_END

	LDY #$5
	LDA OBSTACLES,Y
	STA OB_1_START
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_1_END
	STY NEXT_OBSTACLE

	LDY #$3
	LDA BRANCHES,Y
	STA OB_1_BRANCH
	STY NEXT_BRANCH

	; initalise background trees
	LDA FOREST_MID_0_INIT
	STA FOREST_MID_0
	LDA FOREST_MID_1_INIT
	STA FOREST_MID_1
	LDA FOREST_MID_2_INIT
	STA FOREST_MID_2

	; width/number of wings sprite & obstacle 0
	; doesn't change throughout game
	LDA #WINGS_NUSIZ_VAL
	STA NUSIZ0


; ----------------------------------
; GAME RESTART

	; registers that are reset in game_restart subroutine are altered during gameplay
	; and need to be reset on game restart
game_restart SUBROUTINE game_restart
	; clear any active collisions
	STA CXCLR

	MULTI_COUNT_SETUP

	LDA #$0
	STA SCORE
	STA PLAY_STATE	 ; 0 is the normal PLAY_STATE
	STA HMP0				 ; player speed

	LDA #BIRD_POS_INIT
	STA BIRD_POS
	LDA #FLY_FRAME_INIT
	STA FLY_FRAME

	; speed of flight
	LDA #OBSTACLE_SPEED
	STA CURRENT_OB_0_SPEED
	STA CURRENT_OB_1_SPEED
	STA HMM0
	STA HMM1


	; POSITION ELEMENTS
	
	; use the very first frame of the game sequence to position elements
	STA WSYNC

	; make sure beam is off
	LDA	#2
	STA VBLANK

	; note: RESP0 position is set at the end of the vblank

	; position obstacle trigger (ball) at right most screen edge
	; this will be hidden because of the aggresive triggering of HMOVE
	; we do at the beginning of every scanline
	POS_SCREEN_LEFT RESBL, 2

	; place obstacle 1 (missile 1) at right most screen edge
	POS_SCREEN_RIGHT RESM1, 0

	; place obstacle 0 (missile 1) at screen middle
	POS_SCREEN_MID RESM0, 0

	; signify end of fine tuning (must happen at least 24 machine cycles after ACTIVATE_FINE_TUNE)
	FINE_POS_END


; ----------------------------------
; GAME - VSYNC

game_vsync SUBROUTINE game_vsync
	VSYNC_KERNEL_BASIC

	
; ----------------------------------
; GAME - VBLANK

game_vblank SUBROUTINE game_vblank
	VBLANK_KERNEL_SETUP

	LDX PLAY_STATE
	BEQ game_vblank_play

; -------------
; GAME - VBLANK - DEATH

game_vblank_death SUBROUTINE game_vblank_death
	CPX #PLAY_STATE_DEATH_SPIRAL
	BEQ game_vblank_death_spiral


game_vblank_death_drown SUBROUTINE game_vblank_death_drown
	; slow down death animation
	MULTI_COUNT_THREE_CMP 0
	BMI .continue_drowning

	; end drowning after FLY_FRAME (initialised to DEATH_DROWNING_LEN) 
	LDX FLY_FRAME
	BEQ .drowning_end
	DEX 
	STX FLY_FRAME

	; decrease bird sprite position
	LDX BIRD_POS
	DEX
	STX BIRD_POS
.continue_drowning
	JMP game_vblank_end_more

.drowning_end
	JMP game_restart


game_vblank_death_spiral SUBROUTINE game_vblank_death_spiral
	; set death spriral rebound speed
	; note that we do this every frame because we trigger HMCLR every frame
	LDA #DEATH_SPIRAL_SPEED
	STA HMP0
	STA HMP1

	; alter position (FLY FRAME is a 2's complement negative so we can add)
	LDA BIRD_POS
	LDX FLY_FRAME
	CLC
	ADC DEATH_SPIRAL,X

	; we don't want BIRD_POS to fall below BIRD_LOW
	CMP #BIRD_LOW
	BCC	.death_fly_lowest

	STA BIRD_POS

	CPX #DEATH_SPIRAL_LEN
	BCS .cont_death_anim_save_activity
	INX
	JMP .cont_death_anim_save_activity

.death_fly_lowest
	JMP .end_death_anim

.cont_death_anim_save_activity
	STX FLY_FRAME

.cont_death_anim
	MULTI_COUNT_THREE_CMP 0
	BPL .cont_death_anim_with_foliage
	JMP game_vblank_end_more

.end_death_anim
	JMP game_restart

.cont_death_anim_with_foliage
	LDY NEXT_FOLIAGE
	INY
	CPY #FOLIAGE_CHAOS_CYCLE
	BCC .foliage_updated
	LDY #0
.foliage_updated
	STY NEXT_FOLIAGE
	JMP game_vblank_end_more


; ----------------------------------
; GAME - VBLANK - PLAY

game_vblank_play SUBROUTINE game_vblank_play
	; reset speed of flight
	LDA CURRENT_OB_0_SPEED
	STA HMM0
	LDA CURRENT_OB_1_SPEED
	STA HMM1

	MULTI_COUNT_THREE_CMP 0
	BEQ .far_jmp
	BMI game_vblank_collisions
	BPL game_vblank_foliage

.far_jmp
	JMP game_vblank_player_sprite

	; -------------
	; GAME - VBLANK - PLAY - FOLIAGE
game_vblank_foliage
	LDY NEXT_FOLIAGE
	INY
	CPY #FOLIAGE_CHAOS_CYCLE
	BCC .foliage_updated

	; rotate forest whenever foliage chaos cycle resets
.rotate_forest
	CLC
	ROR FOREST_MID_0
	LDA FOREST_MID_0
	AND #%00001000
	BNE .jump_tree
	JMP .cont_forest
.jump_tree
	LDA #%11110000
	AND FOREST_MID_0
	STA FOREST_MID_0
	SEC
.cont_forest
	ROR FOREST_MID_2
	ROL FOREST_MID_1
	BCS .carry_tree
	JMP .forest_done
.carry_tree
	LDA #%10000000
	ORA FOREST_MID_0
	STA FOREST_MID_0
.forest_done

	; reset chaos cycle
	LDY #0

.foliage_updated
	STY NEXT_FOLIAGE

	JMP game_vblank_end

	; -------------
	; GAME - VBLANK - PLAY - COLLISIONS
game_vblank_collisions
	; check bird collision
	BIT CXM0P
	BMI .bird_collision
	BVS .bird_collision
	BIT CXM1P
	BMI .bird_collision
	BVS .bird_collision

	; check for collision of obstacles with backstop
	BIT CXM0FB
	BVS .reset_obstacle_0
	BIT CXM1FB
	BVS .reset_obstacle_1

	; a frame has occurred without obstacle collision
	; reset speed and width of obstacles
	LDA #OBSTACLE_SPEED
	STA CURRENT_OB_0_SPEED
	STA CURRENT_OB_1_SPEED
	STA HMM0
	STA HMM1

	JMP game_vblank_end

.reset_obstacle_0
	; get new bottom for obstacle 0
	LDY NEXT_OBSTACLE
	LDA OBSTACLES,Y
	STA OB_0_START
	; calculate top of obstacle 0
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_0_END

	; NOTE: no branch for obstacle 0

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_EXIT_SPEED
	STA CURRENT_OB_0_SPEED
	STA HMM0

	; reset screen position
	POS_SCREEN_LEFT RESM0, 0

	; the combination of reset position and altered speed has the effect
	; of completely masking changes to the obstacles appearance when
	; it is reused

	JMP .obstacle_reset_done

.reset_obstacle_1
	; get new bottom for obstacle 1
	LDY NEXT_OBSTACLE
	LDA OBSTACLES,Y
	STA OB_1_START
	; calculate top for obstacle 1
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_1_END

	; get new branch for obstacle 1
	LDY NEXT_BRANCH
	LDA BRANCHES,Y
	STA OB_1_BRANCH

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_EXIT_SPEED
	STA CURRENT_OB_1_SPEED
	STA HMM1

	; reset screen position
	POS_SCREEN_LEFT RESM1, 0

	; the combination of reset position and altered speed has the effect
	; of completely masking changes to the obstacles appearance when
	; it is reused

.obstacle_reset_done
	; increase score -- using decimal addition
	SED
	LDA SCORE
	CLC
	ADC #$1
	STA SCORE
	CLD
	JMP game_vblank_end

.bird_collision
	; TODO: better collision handling
	
	; save hiscore
	LDA SCORE
	SED
	CMP HISCORE 
	BCC .bird_collision_reset
	STA HISCORE
.bird_collision_reset
	CLD

	; prepare death animation

	; note obstacle movement will effectively be stopped now because it will
	; be stopped at the end of the vblank (as usual) but won't be restarted 
	; in the game_vblank_death subroutine

	; use fly down sprite
	LDA #<WINGS_UP
	STA BIRD_SPRITE_ADDRESS

	; change play state
	LDA #PLAY_STATE_DEATH_SPIRAL
	STA PLAY_STATE

	; begin fly down anim
	LDA #0
	STA FLY_FRAME

	JMP game_vblank_end
	
	; -------------
	; GAME - VBLANK - PLAY - USER INPUT

game_vblank_player_sprite
	; check fire button
	LDA INPT4
	BMI .do_flight_anim

	; fire button is being pressed

	; change position of next obstacle - to save precious cycles
	; we've deferred limiting the pointer to the overscan kernel
	; NOTE: this is okay because we only reference NEXT_OBSTACLE
	; in the VBLANK - here and in "VBLANK - COLLISIONS". the overscan
	; kernel will have run at least once before we next reference the
	; variable
	INC NEXT_OBSTACLE

	; ditto for NEXT_BRANCH
	INC NEXT_BRANCH

	; do nothing if fire is being held from last frame
	LDX FIRE_HELD
	BPL .do_flight_anim

	; new fire button press - start new fly animation
	LDX #FLY_CLIMB_START_FRAME
	STX FLY_FRAME

	; flip sprite in response to fire button press
	LDX BIRD_SPRITE_ADDRESS
	CPX #<WINGS_DOWN
	BEQ .flip_sprite_use_flat

	LDX #<WINGS_DOWN
	STX BIRD_SPRITE_ADDRESS
	JMP .flip_sprite_end

.flip_sprite_use_flat
	LDX #<WINGS_FLAT
	STX BIRD_SPRITE_ADDRESS
.flip_sprite_end

.do_flight_anim
	; save fire button state for next frame
	; (accumulator should still reflect INPT4)
	STA FIRE_HELD

	; fly down animation
	LDA FLY_FRAME
	BMI .fly_down

	; have we been flying up for more than CLIMB_FRAMES frames ...
	LDA FLY_FRAME
	CMP #CLIMB_FRAMES
	BPL .glide_test

	; else, fly up
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
	LDA #<WINGS_FLAT
	STA BIRD_SPRITE_ADDRESS

	INC FLY_FRAME
	JMP .fly_end

.end_glide
	; we have been gliding for the maximum number of frames
	; begin fly down animation
	LDA #FLY_DIVE_START_FRAME
	STA FLY_FRAME

.fly_down
	; use fly down sprite
	LDA #<WINGS_UP
	STA BIRD_SPRITE_ADDRESS

	; alter position (FLY FRAME is a 2's complement negative so we can add)
	LDA BIRD_POS
	CLC
	ADC FLY_FRAME

	; we don't want BIRD_POS to fall below BIRD_LOW
	CMP #BIRD_LOW
	BCC	.fly_lowest

	STA BIRD_POS
	DEC FLY_FRAME
	JMP .fly_end

.fly_lowest
	LDA #BIRD_LOW
	STA BIRD_POS

	LDA #DEATH_DROWNING_LEN
	STA FLY_FRAME

	; change play state to death by drowning
	LDA #PLAY_STATE_DEATH_DROWN
	STA PLAY_STATE

	; use drown sprite
	;LDA #<WINGS_DROWN
	;STA BIRD_SPRITE_ADDRESS

.fly_end
	; fall through


; -------------
; GAME - VBLANK - END

game_vblank_end SUBROUTINE game_vblank_end

	; reset sprite objects to leftmost of the screen
	; note: we do this every frame because RESP0 is used and moved for 
	; the scoring subroutine
	POS_SCREEN_LEFT RESP0, 4
	POS_SCREEN_LEFT RESP1, 6

game_vblank_end_more SUBROUTINE game_vblank_end_more
	; setup display kernel

	; do horizontal movement
	; note that move registers need to be set up every frame before this
	; point because we're going to set them all to zero later on
	STA WSYNC
	STA HMOVE

	; reset collision flags every frame
	STA CXCLR

	; turn on trigger (ball) - turned off after foliage
	LDA #$2
	STA ENABL

	; obstacles already turned off - will be turned on again halfway through foliage subroutine

	; playfield priority - foliage in front of obstacles
	LDA #$4
	STA CTRLPF

	; foliage colours
	LDA #FOREST_BACKGROUND
	STA	COLUBK
	LDA #FOLIAGE_COLOR
	STA COLUPF

	; X register will contain the current scanline for the duration of the display kernal
	; starting with VISIBLE_LINES_FOLIAGE and then VISIBLE_LINE_PLAYAREA, loaded later
	LDX	#VISIBLE_LINES_FOLIAGE

	; Y register keeps pointer to next foliage data to stuff into playfield
	LDY NEXT_FOLIAGE

	; reset all movmement registers - we'll be triggering HMOVE every scanline and we
	; don't want objects flying all over the screen. move registers will be reset
	; accordingly at the beginning of each frame
	; note that we should be 24 machine cycles between these writes and
	; the HMOVE earlier
	STA HMCLR

	; wait for end of vblank kernel 
	VBLANK_KERNEL_END
	STA HMOVE


; ----------------------------------
; GAME - DISPLAY - FOLIAGE

foliage SUBROUTINE foliage

	; A should be zero after VBLANK_KERNEL_END

	; X register contains the number of VISIBLE_LINES_FOLIAGE remaining

	; Y register contains the NEXT_FOLIAGE value
	; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
	; into FOLIAGE space to set the playfield values
	;
	; we increase Y after every time we access it (after every change to the playfield - 3 per cycle)
	; in this subroutine: there are VISIBLE_LINES_FOLIAGE
	; scanlines in this part of the display; the main body of the routine is run every
	; VISIBLE_LINES_PER_FOLIAGE; so:
	;
	;			max foliage index = NEXT_FOLIAGE + (VISIBLE_LINES_FOLIAGE / VISIBLE_LINES_PER_FOLIAGE * 3) - 1
	;
	; as is, the routine would draw the same foliage every frame. to introduce some randomness,
	; NEXT_FOLIAGE is increased by one every THREE_CYCLE frames in the vblank kernel. the maximum
	; vaulue of NEXT_FOLIAGE at the start of the .display_foliage subroutine is therefore:
	;
	;			FOLIGE_CHAOS_CYCLE = sizeof FOLIAGE memory space - mex_foliage_index

.next_foliage
	; A = VISIBLE_LINES_PER_FOLIAGE 
	CMP #$0
	BNE .cont_foliage

	; we're going to clobber the accumulator but that's okay, we're
	; going to reset it after setting the playfield

	LDA FOLIAGE,Y
	STA PF0
	INY
	LDA FOLIAGE,Y
	STA PF1
	INY
	LDA FOLIAGE,Y
	STA PF2
	INY

	; start drawing obstacles if we're halfway through the foliage area
	CPX #VISIBLE_LINES_FOLIAGE / 2
	BNE .reset_foliage_block_count
	LDA #$2
	STA ENAM0
	STA ENAM1

.reset_foliage_block_count
	LDA #VISIBLE_LINES_PER_FOLIAGE

.cont_foliage
	; A = VISIBLE_LINES_PER_FOLIAGE 
	SEC
	SBC #$1

	DEX												; 2
	BEQ game_play_area				; 2/3

	STA WSYNC
	STA HMOVE
	JMP .next_foliage			; 3


; ----------------------------------
; GAME - DISPLAY - PLAY AREA

game_play_area SUBROUTINE game_play_area
	; set up play area
	LDA #$0

	; turn off trigger (ball) - otherwise, it'll be visible because we won't be doing any HMOVEs 
	STA ENABL

	; playfield priority - background trees behind obstacles
	STA CTRLPF

	; we want to stuff the playfield with new data as quickly as possible
	; prepare playfield data according to which frame (odd/even) and
	; push results onto stack
	MULTI_COUNT_TWO_CMP
	BEQ .precalc_forest_static
	LDA FOREST_MID_2
	PHA
	LDA FOREST_MID_1
	PHA
	LDA FOREST_MID_0
	PHA
	JMP .end_forest_precalc
.precalc_forest_static
	LDA FOREST_STATIC_2
	ORA FOREST_MID_2
	PHA
	LDA FOREST_STATIC_1
	ORA FOREST_MID_1
	PHA
	LDA FOREST_STATIC_0
	ORA FOREST_MID_0
	PHA
.end_forest_precalc

	STA WSYNC
	STA HMOVE

	; set forest for entire play area
	LDA #FOREST_COLOR
	STA COLUPF
	PLA
	STA PF0
	PLA
	STA PF1
	PLA
	STA PF2

	; prepare for loop

	LDX #VISIBLE_LINE_PLAYAREA
	LDY #SPRITE_LINES

	; loop alternates between .display_bird and .display_obstacle
	; starting with .display_bird - branching at end of loop

	; X register contains the number of VISIBLE_LINE_PLAYAREA remaining

	; Y register contains number of SPRITE_LINES remaining
	; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
	; to set the sprite line

.display_bird
	STA WSYNC									; 3
	STA HMOVE									; 3

	LDA BIRD_DRAW							; 3
	STA GRP0									; 3

	LDA DETAIL_DRAW						; 3
	STA GRP1									; 3

	; maximum 22 cycles in HBLANK
	;		15 cycles used
	;		7 cycles until end of HBLANK

.precalc_sprite
	; if we're at scan line number BIRD_POS (ie. where the bird is) - turn on the sprite
	CPX BIRD_POS										; 2
	BCS .precalc_sprite_done				; 2/3
	TYA															; 2
	BMI .precalc_sprite_done				; 2/3
	LDA (BIRD_SPRITE_ADDRESS),Y			; 5
	STA BIRD_DRAW										; 3 
	LDA (DETAIL_SPRITE_ADDRESS),Y		; 5
	STA DETAIL_DRAW									; 3 
	DEY															; 2
.precalc_sprite_done

	; precalculate branch placement in time for next .display_obstacle cycle
.precalc_branch
	LDA #TREE_NUSIZ_VAL				; 3
	CPX OB_1_BRANCH						; 3
	BNE .precalc_branch_done	; 2/3
	LDA #BRANCH_NUSIZ_VAL			; 3
.precalc_branch_done
	STA OBSTACLE_1_NUSIZ			; 3

	JMP .next_scanline				; 3

	; maximum 76 cycles between WSYNC
	; longest path
	;   58 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 66
	; 2 cycles remaining


.display_obstacle
	; maximum 76 cycles between WSYNC
	STA WSYNC									; 3
	STA HMOVE									; 3


	LDA OBSTACLE_1_NUSIZ			; 3
	STA NUSIZ1								; 3

	LDA OBSTACLE_0_DRAW				; 3
	STA ENAM0									; 3
	LDA OBSTACLE_1_DRAW				; 3
	STA ENAM1									; 3

	; maximum 22 cycles in HBLANK
	;		21 cycles used
	;		1 cycles until end of HBLANK

.precalc_obstacles
	; we don't have time in the HBLANK to do all this comparing and branching
	; so we "precalc" the results now in time for the next scanline
	; we're using precious visible scanline cycles of course, but all the important
	; "drawing" is done during the hblank and the first part of the visible screen

.precalc_obstacle_0
	LDA #$2										; 2
	CPX OB_0_START						; 3
	BCC .obstacle_0_done			; 2/3
	CPX OB_0_END							; 3
	BCS .obstacle_0_done			; 2/3
	LDA #$0										; 2
.obstacle_0_done
	STA OBSTACLE_0_DRAW				; 3

.precalc_obstacle_1
	LDA #$2										; 2
	CPX OB_1_START						; 3
	BCC .obstacle_1_done			; 2/3
	CPX OB_1_END							; 3
	BCS .obstacle_1_done			; 2/3	
	LDA #$0										; 2
.obstacle_1_done
	STA OBSTACLE_1_DRAW				; 3

	; longest path
	;		55 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 71
	; 5 cycles remaining

	STA VDELP1

.next_scanline
	; decrement current scanline - go to overscan kernel if we have reached zero
	DEX												; 2
	BEQ swamp									; 2/3

	; interlace sprite and obstacle drawing
	TXA												; 2
	AND #%00000001						; 2
	BEQ .display_obstacle	  	; 2/3
	JMP .display_bird					; 3


; ----------------------------------
; GAME - DISPLAY - SWAMP 

swamp SUBROUTINE swamp
	; we've arrived here via the BEQ swamp call above in the game_play_area.next_scanline routine above
	; the last loop in the game_play_area loop before the successful branching would have been the .display_bird
	; loop. so, the available number of cycles left before the end of the scanline is:

	; (note the fewer cycles used for ".next_scanline" - caused by the branching to this swamp subroutine)

	; maximum 76 cycles between WSYNC
	; longest path
	;   58 cycles
	; + 5 for ".next_scanline"
	; + 3 for WSYNC
	; = 66
	; 10 cycles remaining

	; (also note that this is the number of cycles left after the longest path, which occurs when the bird
	; is at it's lowest point. ie. using extra cycle might not be noticable depending on the bird's position)

	LDY NEXT_FOLIAGE			; 3
	LDX FOLIAGE,Y					; 4

	; 3 cycles remaining until end of scanline

	STA WSYNC
	STA HMOVE

	; disable obstacles - we don't want the "trees" to extend into the forest swamp
	STA ENAM0
	STA ENAM1

	; define the forest swamp playfield once per frame
	LDA #SWAMP_BACKGROUND
	STA COLUBK

	; change colour of playfield to simulate movement
	; we'll change background colour in the next HBLANK
	LDA #SWAMP_COLOR
	STA COLUPF

	; draw swap the same as the last line of the foliage
	STX PF0
	STX PF1
	STX PF2

	; turn off sprites
	LDA #$00
	STA GRP0
	STA GRP1

	; make sure we don't draw sprite at the top of next frame by accident
	STA BIRD_DRAW
	STA DETAIL_DRAW

	; wait for end of the swamp ...
	LDX #VISIBLE_LINES_SWAMP
.next_scanline
	DEX													; 2
	BEQ scoring									; 2/3
	STA WSYNC
	STA HMOVE
	JMP .next_scanline					; 3


; ----------------------------------
; GAME - DISPLAY - SCORING 
scoring SUBROUTINE scoring
	LDA PLAY_STATE
	BEQ .display_score
	JMP game_overscan

.display_score
	STA WSYNC
	LDA #0
	STA PF0
	STA PF0
	STA PF1
	STA PF2
	LDA #SCORING_BACKGROUND
	STA	COLUBK

	LDA #SCORE_NUSIZ_VAL
	STA NUSIZ1
	; NUSIZ0 is already a suitable size for displaying a scoring digit

	MULTI_COUNT_TWO_CMP
	BEQ .prep_hiscore

.prep_score
	LDA #SCORE_COLOR
	STA COLUP0
	STA COLUP1

	POS_SCREEN_RIGHT RESP0, 8
	POS_SCREEN_RIGHT RESP1, 4

	; get address of unit digit
	LDA SCORE
	AND #$0F
	TAY
	LDA DIGIT_TABLE,Y
	STA DIGIT_ADDRESS_1

	; get address of tens digit
	LDA SCORE
	JMP .do_score

.prep_hiscore
	LDA #HISCORE_COLOR
	STA COLUP0
	STA COLUP1

	POS_SCREEN_RIGHT RESP0, 20
	POS_SCREEN_RIGHT RESP1, 16

	; get address of unit digit
	LDA HISCORE
	AND #$0F
	TAY
	LDA DIGIT_TABLE,Y
	STA DIGIT_ADDRESS_1

	; get address of tens digit
	LDA HISCORE

.do_score
	ROR
	ROR
	ROR
	ROR
	AND #$0F
	TAY
	LDA DIGIT_TABLE,Y
	STA DIGIT_ADDRESS_0

	LDY #DIGIT_LINES

	; Y register contains number of DIGIT_LINES remaining
	; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
	; to set the sprite line

.scoring_loop
	STA WSYNC
	LDA (DIGIT_ADDRESS_1),Y
	STA GRP1										; 3
	LDA (DIGIT_ADDRESS_0),Y
	STA GRP0										; 3

.next_scanline
	DEY													; 2
	BMI game_overscan						; 2/3
	JMP .scoring_loop						; 3


; ----------------------------------
; GAME - OVERSCAN 

game_overscan SUBROUTINE game_overscan
	OVERSCAN_KERNEL_SETUP

	; reset sprite image
	LDA #0
	STA GRP0
	STA GRP1

	; reset colours
	LDA #BIRD_COLOR
	STA COLUP0
	STA COLUP1

	; limit NEXT_OBSTACLE to maximum value
	LDY NEXT_OBSTACLE
	CPY #OBSTACLES_CT
	BCC .next_obstacle
	LDY #$0
	STY NEXT_OBSTACLE
.next_obstacle

	; limit NEXT_BRANCH to maximum value
	LDY NEXT_BRANCH
	CPY #BRANCHES_CT
	BCC .next_branch
	LDY #$0
	STY NEXT_BRANCH
.next_branch

	MULTI_COUNT_UPDATE

	OVERSCAN_KERNEL_END

	JMP game_vsync


; ----------------------------------
; MACHINE INITIALISATION 

initialisation SUBROUTINE initialisation

	ORG $FFFA
	.word setup		; NMI
	.word setup		; RESET
	.word setup		;	IRQ

