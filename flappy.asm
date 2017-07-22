
	processor 6502
	include vcs.h
	include macro.h
	include vcs_extra.h

; ----------------------------------
; DATA - COLOURS

; colours
BIRD_COLOR							= $00

FOLIAGE_BACKGROUND			=	$D2
FOLIAGE_COLOR						= $D0

FOREST_BACKGROUND				=	$D2
FOREST_COLOR						= $E0

SWAMP_BACKGROUND				= $B0
SWAMP_COLOR							= $B2

SCORING_BACKGROUND			= $00
SCORE_COLOR							= $0E
HISCORE_COLOR						= $F6

; ----------------------------------
; DATA - CONSTANTS

; screen boundaries for bird sprite
BIRD_HIGH				=	VISIBLE_LINES_PLAYAREA
BIRD_LOW				=	$0C

; death rates
DEATH_COLLISION_SPEED	= $02 ; speed at which player sprite moves foward during death collision
DEATH_DROWNING_SPEEED	= $01
DEATH_DROWNING_LEN		= $0C ; should be the same BIRD_LOW

; NUSIZ0 and NUSIZ1 values - first four bits set obstacle width
; - last four bits set the player sprite size
; - the two nibbles will be OR'd to create the NUSIZ value
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

; start position at start of game 
BIRD_VPOS_INIT					=	BIRD_HIGH / 4 * 3
BIRD_HPOS_INIT					=	$00
BIRD_HPOS_PLAY_POS			=	$0C


; play state -- death states are all negative
PLAY_STATE_PLAY					= $00
PLAY_STATE_READY				= $01
PLAY_STATE_COLLISION		= $FF
PLAY_STATE_DEATH_DROWN	= $FE

; visible scan line usage
; =======================
; note the extra $04 scanlines in the score area are:
; * one at the start of the subroutine
; * two position resets
; * and another one because DIGIT_LINES breaks on -1 not 0 (BMI instead of BEQ)
;
; note that we don't loop the swamp WSYNCs because there are so few and we'll
; be doing something useful during them
; care should be taken to update this value, if we increase the number of WSYNCs
VISIBLE_LINES_FOLIAGE			= $20
VISIBLE_LINES_PLAYAREA			= DISPLAY_SCANLINES - VISIBLE_LINES_FOLIAGE - VISIBLE_LINES_SWAMP - VISIBLE_LINES_SCOREAREA
VISIBLE_LINES_SWAMP				= $03
VISIBLE_LINES_SCOREAREA		= DIGIT_LINES + $04
VISIBLE_LINES_PER_FOLIAGE	= VISIBLE_LINES_FOLIAGE / 8

; ----------------------------------
; DATA - RAM
	SEG.U RAM 
	ORG $80			; start of 2600 RAM

; access _scratch* variables through local "shadow" names only
; that way, you can better monitor unfortunate memory reuse
_scratchA								ds 1

; variables beginning with _ are required by routines in vcs_extra.h
_SLEEP_TABLE_JMP				ds 2
_MULTI_COUNT_STATE			ds 1
_STATE_INPT4						ds 1
STATE_SWCHB							ds 1

PLAY_STATE							ds 1	; state of play - zero is normal, negative is death
BIRD_VPOS								ds 1	; between BIRD_HIGH and BIRD_LOW
BIRD_HPOS								ds 1	; current horizontal pixel position of bird
SELECTED_HEAD						ds 1	;	index into HEADS_TABLE

; selected flight pattern - see FLIGHT_PATTERN macros
FLIGHT_PATTERN					ds 2

; which bird/detail sprite to use in the display kernel
SPRITE_WINGS_ADDRESS		ds 2
SPRITE_HEAD_ADDRESS			ds 2

; PATTERN_INDEX's meaning changes depending on PLAY_STATE
;
; if PLAY_STATE == PLAY_STATE_PLAY
;	then
;		PATTERN_INDEX indexes FLIGHT_PATTERN
;
; if PLAY_STATE == PLAY_STATE_COLLISION
;	then
;		PATTERN_INDEX indexes COLLISION_PATTERN
;
; if PLAY_STATE == PLAY_STATE_DEATH_DROWN
;	then
;		PATTERN_INDEX counts number of frames to game reset
PATTERN_INDEX				ds 1

; value for next foliage (playfield) - points to FOLIAGE
FOLIAGE_SEED				ds 1
OBSTACLE_SEED				ds 1
BRANCH_SEED					ds 1

; obstacles
OB_0								ds 2
OB_1								ds 2
OB_1_BRANCH					ds 1
OB_0_HPOS						ds 1
OB_1_HPOS						ds 1

; pre-calculated ENAM0 and ENAM1 - $0 for obstacle/missile "off" - $2 for "on"
OB_0_DRAW						ds 1
OB_1_DRAW						ds 1
OB_1_NUSIZ					ds 1

; pre-calculated GRP0
WINGS_DRAW					ds 1
HEAD_DRAW						ds 1

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

	; DASM directive - echo number of bytes left in RAM
	ECHO "----",($100 - *) , "bytes of RAM left"

; ----------------------------------
; DATA - DATA / OBSTACLE DATA, ETC.
	SEG
	ORG $F000		; start of cart ROM
	
DATA_SEGMENT

; sprite data
; NOTE: first 00 in each sprite is a boundry byte - used to turn off sprite
WINGS
WINGS_UP				HEX	00 00 00 00 30 70 60 40 
WINGS_FLAT			HEX	00 00 00 00 70 40 00 00
WINGS_DOWN			HEX	00 40 60 70 70 00 00 00

HEADS
HEAD_GIRL_A			HEX 00 00 00 00 20 1C 18 20
HEAD_BOY_A			HEX 00 00 00 00 20 1C 18 00
HEAD_GIRL_B			HEX 00 00 00 00 20 1C 58 20
HEAD_BOY_B			HEX 00 00 00 00 20 1C 18 04
HEADS_TABLE			.byte <HEAD_GIRL_A, <HEAD_BOY_A, <HEAD_GIRL_B, <HEAD_BOY_B
NUM_HEADS				= 4

SPRITE_LINES		=	7

; foliage - playfield data
; (see "foliage" subroutine for full and laboured explanation)
FOLIAGE .byte %01100000, %10011010, %00111010, %10010000, %00110101, %11010001, %01010000, %01010110, %00110011, %10101000, %10100110, %00010110, %10010110, %10011010, %00111010, %00101011, %01101101, %01100110, %01011001, %11001101, %00101010, %01101100, %00100010, %10011010, %00111010, %00110011, %01101101, %01100110, %01011001, %10110101
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

; obstacles tables
; ===============
;
; * obstacle enable precalc table based on play ara of 153 lines
;		- may need tuning if VISIBLE_LINES_PLAYAREA changes
; * works as a barrel shifter
;		- 153 lines before AND after window
;		- there's room for optimisation
; * the window (the zeroes in the table) is 32 bytes(lines)

	; make sure we start on a page boundary - we don't want to cross a page boundary
	ORG $F100

OBSTACLE_ENABLE_PRECALC 
. HEX 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02 02 02 02 02 02 02 02
. HEX 02 02 02

; list of obstacles to use
OBSTACLES			HEX 25 35 45 55 65 75
OBSTACLES_LEN	= 6

; list of branches
; note that we test for branch on the even scanlines of the play area so these values should be even
BRANCHES			HEX 18 26 30 38 44 58 70
BRANCHES_LEN	= 7

; flight patterns
; ===============
; first byte is the length of the flight pattern proper (not including last byte or the first
; byte in the array).
;
; last byte is the index to initialse the pattern index to (usually start of the glide period)
;
;	intervening bytes are the flight pattern proper
;		note: negative pattern values less than -BIRD_LOW (ie. -12) may result in undefined behaviour
;		as this may cause the bird to descend lower than the screen (ie. a negative value)
;
; see FLIGHT_PATTERN macros
EASY_FLIGHT_PATTERN .byte 20, 4, 4, 4, 4, 4, 0, 0, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, 6

	; we'll be using fine positioning from vcs_extra.h so include sleep table definitions
	DEF_POS_SLEEP_TABLE

; ----------------------------------
; MACROS

	MAC NEW_OBSTACLE
		; {1} == 0 or 1
		; clobbers A and X
		; alters OB_0 or OB_1

		IF {1} != 0 && {1} != 1
			ECHO "MACRO ERROR: 'NEW_OBSTACLE': {1} must be 0 OR 1"
			ERR
		ENDIF

		LDX OBSTACLE_SEED
		LDA #<OBSTACLE_ENABLE_PRECALC
		CLC
		ADC OBSTACLES,X

		IF {1} == 0
			STA OB_0
		ENDIF

		IF {1} == 1
			STA OB_1
			LDX BRANCH_SEED
			LDA BRANCHES,X
			STA OB_1_BRANCH
		ENDIF
	ENDM


	MAC APPLY_FLIGHT_PATTERN
		; clobbers A and Y
		LDY PATTERN_INDEX
		LDA (FLIGHT_PATTERN),Y
		CLC
		ADC BIRD_VPOS
	ENDM

DEATH_SPIRAL = 1
ALWAYS = 0
	MAC RESET_FLIGHT_PATTERN
		; initialise PATTERN_INDEX for selected FLIGHT_PATTERN
		; see FLIGHT_PATTERN definitions for memory layout explanation

		; {1} == DEATH_SPIRAL || ALWAYS
		; clobbers A and Y
		; alters PATTERN_INDEX

		; first byte in (FLIGHT_PATTERN) is the pattern's length 
		LDY #$0
		LDA (FLIGHT_PATTERN),Y
		; last byte of (FLIGHT_PATTERN) is the pattern's index initialisation value
		TAY
		INY
		LDA (FLIGHT_PATTERN),Y

		IF {1} == DEATH_SPIRAL
			CMP PATTERN_INDEX
			; when called with the DEATH_SPIRAL argument AND the current PATTERN_INDEX is less than
			; the initialisation value, then don't initialise the index
			BCS .no_store_index
		ENDIF
		STA PATTERN_INDEX
.no_store_index
	ENDM

	MAC UPDATE_FLIGHT_PATTERN
		; requires PATTERN_INDEX in Y
		; clobbers A
		; alters Y and PATTERN_INDEX
		STA BIRD_VPOS
		INY
		TYA
		LDX #$0
		CMP (FLIGHT_PATTERN),X
		BCC .store_index
		LDA (FLIGHT_PATTERN),X
.store_index
		STA PATTERN_INDEX
	ENDM

	MAC DROWNING_PLAY_STATE
		; change play state to death by drowning

		; clobbers A
		; alters BIRD_VPOS, PATTERN_INDEX, PLAY_STATE

		LDA #BIRD_LOW
		STA BIRD_VPOS
		LDA #DEATH_DROWNING_LEN
		STA PATTERN_INDEX
		LDA #PLAY_STATE_DEATH_DROWN
		STA PLAY_STATE

		; use wings up sprite for drowning sprite
		LDA #<WINGS_UP
		STA SPRITE_WINGS_ADDRESS
	ENDM

	MAC FOLIAGE_ANIMATION
		; {1} == 0 -> do NOT the forest background
		; {1} == n -> do animate the forest background (at the speed of FOLIAGE CHAOS CYCLE)

		; clobbers Y
		; alters FOREST_MID_0, FOREST_MID_1, FOREST_MID_2, FOLIAGE_SEED

		; cycle playfield data used to illustrate foliage, and by
		; association, the playfield used for the water/swamp
		LDY FOLIAGE_SEED
		INY
		CPY #FOLIAGE_CHAOS_CYCLE
		BCC .foliage_updated
		LDY #0

		; rotate forest whenever foliage chaos cycle resets
		IF {1} == 1
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
	ENDIF

.foliage_updated
		STY FOLIAGE_SEED
	ENDM


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
	DISCREET_TRIGGER_PLAYER0
	BPL .end_title_screen
	LDX #DISPLAY_SCANLINES
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
; GAME - INITIALISATION

game_state_init SUBROUTINE game_state_init
	; initialise most significant byte of indirect addresses
	LDA #>DATA_SEGMENT
	STA SPRITE_WINGS_ADDRESS+1
	STA SPRITE_HEAD_ADDRESS+1
	STA DIGIT_ADDRESS_0+1
	STA DIGIT_ADDRESS_1+1
	LDA #>OBSTACLE_ENABLE_PRECALC
	STA OB_0+1
	STA OB_1+1

	; initialise head
	LDY #$0
	STY SELECTED_HEAD
	LDA HEADS_TABLE,Y
	STA SPRITE_HEAD_ADDRESS

	; initialise obstacles
	LDA #$2
	STA OBSTACLE_SEED
	STA BRANCH_SEED
	NEW_OBSTACLE 0
	INC OBSTACLE_SEED
	NEW_OBSTACLE 1

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
; GAME - RESTART

	; registers that are reset in game_restart subroutine are altered during gameplay
	; and need to be reset on game restart
game_restart SUBROUTINE game_restart
	; clear any active collisions
	STA CXCLR

	DISCREET_TRIGGER_PLAYER0

	MULTI_COUNT_SETUP
	; TODO: use flight pattern depending on switch
	LDA #<EASY_FLIGHT_PATTERN
	STA FLIGHT_PATTERN
	LDA #>EASY_FLIGHT_PATTERN
	STA FLIGHT_PATTERN+1

	RESET_FLIGHT_PATTERN 0

	LDA #$0
	STA SCORE

	; not resetting high score (!)

	LDA #PLAY_STATE_READY
	STA PLAY_STATE

	LDA #BIRD_VPOS_INIT
	STA BIRD_VPOS

	LDA #BIRD_HPOS_INIT
	STA BIRD_HPOS

	; use flat wings sprite at start
	LDA #<WINGS_FLAT
	STA SPRITE_WINGS_ADDRESS

	; POSITION ELEMENTS

	; make sure movement registers are zero
	STA HMCLR
	
	; use the very first frame of the game sequence to position elements
	VERTICAL_SYNC

	; position obstacle trigger (ball) at right most screen edge
	; this will be hidden because of the aggresive triggering of HMOVE
	; we do at the beginning of every scanline
	FINE_POS_SCREEN_LEFT RESM0, OB_0_HPOS, 4, 74

	; place obstacle 1 (missile 1) at right most screen edge
	FINE_POS_SCREEN_RIGHT RESM1, OB_1_HPOS, 4, 5

	; place obstacle 0 (missile 1) at screen middle


	
; ----------------------------------
; GAME - VSYNC / VBLANK

game_vsync
game_vblank SUBROUTINE game_vblank
	VSYNC_KERNEL_BASIC
	VBLANK_KERNEL_SETUP

	LDX PLAY_STATE
	BEQ .far_jmp_game_vblank_play

	CPX #PLAY_STATE_COLLISION
	BEQ game_vblank_death_collision
	CPX #PLAY_STATE_DEATH_DROWN
	BEQ game_vblank_death_drown
	JMP game_vblank_ready

.far_jmp_game_vblank_play
	JMP game_vblank_play

	
; ----------------------------------
; GAME - VBLANK - READY STATE

game_vblank_ready SUBROUTINE game_vblank_ready
	DISCREET_TRIGGER_PLAYER0
	BMI .continue_ready_state

.end_ready_state
	LDA #PLAY_STATE_PLAY
	STA PLAY_STATE

.continue_ready_state
	JMP game_vblank_end


; ----------------------------------
; GAME - VBLANK - DEATH - DROWN

game_vblank_death_drown SUBROUTINE game_vblank_death_drown
	; slow down death animation
	MULTI_COUNT_THREE_CMP 0
	BNE .continue_drowning

	LDA BIRD_HPOS
	CLC
	ADC #DEATH_DROWNING_SPEEED
	STA BIRD_HPOS

	; end drowning after PATTERN_INDEX (initialised to DEATH_DROWNING_LEN) 
	LDX PATTERN_INDEX
	BEQ .drowning_end
	DEX 
	STX PATTERN_INDEX

	; decrease bird sprite position
	LDX BIRD_VPOS
	DEX
	STX BIRD_VPOS

	FOLIAGE_ANIMATION 0

.continue_drowning
	JMP game_vblank_end

.drowning_end
	JMP game_restart


; ----------------------------------
; GAME - VBLANK - DEATH - COLLISION

game_vblank_death_collision SUBROUTINE game_vblank_death_collision
	; update display elements every three frames
	; note that we do all updating on the same frame, unlike during PLAY_STATE_PLAY
	; where we update different elements (sprite, foliage) on different frames
	MULTI_COUNT_THREE_CMP 0
	BEQ .update_display_elements
	JMP game_vblank_end

.update_display_elements
	; alter wing position - simulates furious flapping
	LDA SPRITE_WINGS_ADDRESS
	CMP #<WINGS_UP
	BEQ .use_wings_flat
	CMP #<WINGS_FLAT
	BEQ .use_wings_down
.use_wings_up
	LDA #<WINGS_UP
	STA SPRITE_WINGS_ADDRESS
	JMP .wings_updated
.use_wings_flat
	LDA #<WINGS_FLAT
	STA SPRITE_WINGS_ADDRESS
	JMP .wings_updated
.use_wings_down
	LDA #<WINGS_DOWN
	STA SPRITE_WINGS_ADDRESS
	FOLIAGE_ANIMATION 0
.wings_updated

	; propel bird forward
	LDA BIRD_HPOS
	CLC
	ADC #DEATH_COLLISION_SPEED
	STA BIRD_HPOS

	; apply flight pattern and check for drowning state
	APPLY_FLIGHT_PATTERN
	CMP #BIRD_LOW
	BCC	.end_death_collision
	STA BIRD_VPOS
	UPDATE_FLIGHT_PATTERN

	JMP game_vblank_end

.end_death_collision
	DROWNING_PLAY_STATE
	JMP game_vblank_end


; ----------------------------------
; GAME - VBLANK - PLAY

game_vblank_play SUBROUTINE game_vblank_play
	MULTI_COUNT_THREE_CMP 0
	BEQ .far_jmp
	BMI game_vblank_collisions
	BPL game_vblank_foliage

.far_jmp
	JMP game_vblank_player_sprite

; -------------
; GAME - VBLANK - PLAY - UPDATE FOLIAGE
game_vblank_foliage
	FOLIAGE_ANIMATION 1
	JMP game_vblank_end

; -------------
; GAME - VBLANK - PLAY - COLLISIONS / OBSTACLE RESET
game_vblank_collisions
	; check bird collision
	BIT CXM0P
	BMI .bird_collision
	BVS .bird_collision
	BIT CXM1P
	BMI .bird_collision
	BVS .bird_collision

	; reset obstacle gap / branch in "activision border"
	; we're comparing against three and less because we only run
	; this "collision" vblank subroutine every three frames
	LDA OB_0_HPOS
	CMP #03
	BCC .reset_obstacle_0
	LDA OB_1_HPOS
	CMP #03
	BCC .reset_obstacle_1

	JMP game_vblank_end

.reset_obstacle_0
	; NOTE: no branch ornamentation for obstacle 0
	NEW_OBSTACLE 0
	JMP .score_obstacle

.reset_obstacle_1
	NEW_OBSTACLE 1

.score_obstacle
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

	; start collision animation with wings up sprite
	LDA #<WINGS_UP
	STA SPRITE_WINGS_ADDRESS

	; change play state
	LDA #PLAY_STATE_COLLISION
	STA PLAY_STATE

	RESET_FLIGHT_PATTERN DEATH_SPIRAL

	JMP game_vblank_end

	
; -------------
; GAME - VBLANK - PLAY - UPDATE BIRD SPRITE

game_vblank_player_sprite
	; move bird towards play position if it's not there already
	LDA BIRD_HPOS
	CMP #BIRD_HPOS_PLAY_POS
	BEQ .hpos_done
	CLC
	ADC #1		; not bothering to use FINE_POS_MOVE_RIGHT
	STA BIRD_HPOS
.hpos_done

	DISCREET_TRIGGER_PLAYER0
	BMI .process_flight_pattern

	; trigger is being pressed

	; use joystick input to alter next random seeds

	; change position of next obstacle - to save precious cycles
	; we've deferred limiting the pointer to the overscan kernel
	; NOTE: this is okay because we only reference OBSTACLE_SEED
	; in the VBLANK - here and in "VBLANK - COLLISIONS". the overscan
	; kernel will have run at least once before we next reference it
	INC OBSTACLE_SEED

	; ... similarly for BRANCH_SEED
	INC BRANCH_SEED

	; start new fly animation
	LDX #1
	STX PATTERN_INDEX

	; flip sprite in response to trigger press
	LDX SPRITE_WINGS_ADDRESS
	CPX #<WINGS_DOWN
	BEQ .flip_sprite_use_flat

	LDX #<WINGS_DOWN
	STX SPRITE_WINGS_ADDRESS
	JMP .flip_sprite_end

.flip_sprite_use_flat
	LDX #<WINGS_FLAT
	STX SPRITE_WINGS_ADDRESS
.flip_sprite_end

.process_flight_pattern
	APPLY_FLIGHT_PATTERN

	; compare new BIRD_VPOS with old BIRD_VPOS and change sprite accordingly
	CMP BIRD_VPOS
	BEQ .use_glide_sprite
	BCC .use_wings_up_sprite
	JMP .sprite_set
.use_wings_up_sprite
	LDX #<WINGS_UP
	STX SPRITE_WINGS_ADDRESS
	JMP .sprite_set
.use_glide_sprite
	LDX #<WINGS_FLAT
	STX SPRITE_WINGS_ADDRESS
.sprite_set

	; check for ground collision
	CMP #BIRD_LOW
	BCC	.begin_drowning

	; check to see if we've reached top of flying area (ie. top of screen)
	CMP #BIRD_HIGH
	BCS .limit_height
	JMP .update_flight_pattern_index

.begin_drowning
	DROWNING_PLAY_STATE
	JMP .fly_end

.limit_height
	LDA #BIRD_HIGH

.update_flight_pattern_index
	UPDATE_FLIGHT_PATTERN

.fly_end
	; fall through


; ----------------------------------
; GAME - VBLANK - END (PREPARE DISPLAY KERNEL)

game_vblank_end SUBROUTINE game_vblank_end
	; position sprites
	LDA BIRD_HPOS
	FINE_POS_SCREEN RESP0
	LDA BIRD_HPOS
	CLC
	ADC #$07
	FINE_POS_SCREEN RESP1

	; progress & position obstacles
	; (note that we're performing the progression every frame)
	LDA PLAY_STATE
	CMP #PLAY_STATE_PLAY
	BNE .end_obstacle_progression
	FINE_POS_MOVE_LEFT OB_0_HPOS, #1
	FINE_POS_MOVE_LEFT OB_1_HPOS, #1
.end_obstacle_progression
	LDA OB_0_HPOS
	FINE_POS_SCREEN RESM0
	LDA OB_1_HPOS
	FINE_POS_SCREEN RESM1

	; setup display kernel (for foliage area)

	; do horizontal movement
	; note that move registers need to be set up every frame before this
	; point because we're going to set them all to zero later on
	STA WSYNC
	STA HMOVE

	; reset collision flags every frame
	STA CXCLR

	; obstacles already turned off - will be turned on again halfway through foliage subroutine

	; playfield priority - foliage in front of obstacles
	LDA #%00000100
	STA CTRLPF

	; foliage colours
	LDA #FOLIAGE_BACKGROUND
	STA	COLUBK
	LDA #FOLIAGE_COLOR
	STA COLUPF

	; prepare index registers
	; Y register will count the number of lines in the foliage area of the display
	; X register keeps pointer to next foliage data to stuff into playfield
	LDY	#VISIBLE_LINES_FOLIAGE
	LDX FOLIAGE_SEED

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
; GAME - DISPLAY KERNEL - FOLIAGE

foliage SUBROUTINE foliage

	; A should be zero after VBLANK_KERNEL_END

	; Y register contains the number of VISIBLE_LINES_FOLIAGE remaining

	; X register contains the FOLIAGE_SEED value
	;
	; we increase X after every time we access it (after every change to the playfield - 3 per cycle)
	; in this subroutine: there are VISIBLE_LINES_FOLIAGE
	; scanlines in this part of the display; the main body of the routine is run every
	; VISIBLE_LINES_PER_FOLIAGE; so:
	;
	;			max_foliage_index = FOLIAGE_SEED + (VISIBLE_LINES_FOLIAGE / VISIBLE_LINES_PER_FOLIAGE * 3) - 1
	;
	; as is, the routine would draw the same foliage every frame. to introduce some randomness,
	; FOLIAGE_SEED is increased by one every THREE_CYCLE frames in the vblank kernel. the maximum
	; vaulue of FOLIAGE_SEED at the start of the foliage subroutine is therefore:
	;
	;			FOLIGE_CHAOS_CYCLE = sizeof FOLIAGE memory space - max_foliage_index

.next_foliage
	; A = VISIBLE_LINES_PER_FOLIAGE 
	CMP #$0
	BNE .cont_foliage

	; we're going to clobber the accumulator but that's okay, we're
	; going to reset it after setting the playfield

	LDA FOLIAGE,X
	STA PF0
	INX
	LDA FOLIAGE,X
	STA PF1
	INX
	LDA FOLIAGE,X
	STA PF2
	INX

	; start drawing obstacles if we're halfway through the foliage area
	CPY #VISIBLE_LINES_FOLIAGE / 2
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

	DEY													; 2
	BEQ game_play_area_prepare	; 2/3

	STA WSYNC
	STA HMOVE
	JMP .next_foliage			; 3


; ----------------------------------
; GAME - DISPLAY KERNEL - PLAY AREA

game_play_area_prepare SUBROUTINE game_play_area_prepare
	; playfield priority - background trees behind obstacles
	LDA #%00000000
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
	LDA #FOREST_BACKGROUND
	STA	COLUBK
	LDA #FOREST_COLOR
	STA COLUPF
	PLA
	STA PF0
	PLA
	STA PF1
	PLA
	STA PF2

	; prepare for loop

	LDY #VISIBLE_LINES_PLAYAREA
	LDX #SPRITE_LINES

game_play_area SUBROUTINE game_play_area
.YSTATE = _scratchA

	; loop alternates between .display_bird and .display_obstacle starting with .display_bird
	; Y register contains the number of VISIBLE_LINES_PLAYAREA remaining
	; X register contains number of SPRITE_LINES remaining

	; ODD NUMBERED SCANLINES
.display_bird
	STA WSYNC									; 3
	STA HMOVE									; 3

	LDA WINGS_DRAW						; 3
	STA GRP0									; 3

	LDA HEAD_DRAW							; 3
	STA GRP1									; 3

	; maximum 22 cycles in HBLANK
	;		15 cycles used
	;		7 cycles until end of HBLANK

.precalc_sprite
	; if we're at scan line number BIRD_VPOS (ie. where the bird is) - turn on the sprite
	CPY BIRD_VPOS									; 2
	BCS .precalc_sprite_done			; 2/3
	TXA														; 2
	BMI .precalc_sprite_done			; 2/3
	STY .YSTATE										; 3
	TAY														; 2
	LDA (SPRITE_WINGS_ADDRESS),Y	; 5
	STA WINGS_DRAW								; 3 
	LDA (SPRITE_HEAD_ADDRESS),Y		; 5
	STA HEAD_DRAW									; 3 
	DEX														; 2
	LDY .YSTATE										; 3
.precalc_sprite_done

	JMP .next_scanline				; 3

	; maximum 76 cycles between WSYNC
	; longest path
	;   52 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 68
	; 8 cycles remaining

	; EVEN NUMBERED SCANLINES
.display_obstacle
	; maximum 76 cycles between WSYNC
	STA WSYNC									; 3
	STA HMOVE									; 3

	LDA OB_1_NUSIZ						; 3
	STA NUSIZ1								; 3

	LDA OB_0_DRAW							; 3
	STA ENAM0									; 3
	LDA OB_1_DRAW							; 3
	STA ENAM1									; 3

	; maximum 22 cycles in HBLANK
	;		21 cycles used
	;		1 cycles until end of HBLANK

.precalc_obstacles
	LDA (OB_0),Y							; 5
	STA OB_0_DRAW							; 3
	LDA (OB_1),Y							; 5
	STA OB_1_DRAW							; 3

	; precalculate branch placement in time for next .display_obstacle cycle
.precalc_branch
	LDA #TREE_NUSIZ_VAL				; 3
	CPY OB_1_BRANCH						; 3
	BNE .precalc_branch_done	; 2/3
	LDA #BRANCH_NUSIZ_VAL			; 3
.precalc_branch_done
	STA OB_1_NUSIZ						; 3

	; longest path
	;		61 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 61
	; 9 cycles remaining

.next_scanline
	; decrement current scanline - go to overscan kernel if we have reached zero
	DEY												; 2
	BEQ swamp									; 2/3

	; interlace sprite and obstacle drawing
	TYA												; 2
	AND #%00000001						; 2
	BEQ .display_obstacle	  	; 2/3
	JMP .display_bird					; 3


; ----------------------------------
; GAME - DISPLAY KERNEL - SWAMP

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

	LDY FOLIAGE_SEED			; 3
	LDX FOLIAGE,Y					; 4

	; 3 cycles remaining until end of scanline

	STA WSYNC
	STA HMOVE

	; disable obstacles - we don't want the "trees" to extend into the forest swamp
	LDA #0
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

	; an extra line of swamp - so when we turn off the sprites below
	; we do so in the HBLANK
	STA WSYNC
	STA HMOVE

	; turn off sprites
	LDA #$00
	STA GRP0
	STA GRP1

	; make sure we don't draw the sprites at the top of next frame by accident
	STA WINGS_DRAW
	STA HEAD_DRAW

	; wait another line before display - scoring
	STA WSYNC
	STA HMOVE


; ----------------------------------
; GAME - DISPLAY KERNEL - SCORING
scoring SUBROUTINE scoring
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

	POS_SCREEN_RIGHT RESP0, 10
	POS_SCREEN_RIGHT RESP1, 6

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

	POS_SCREEN_RIGHT RESP0, 22
	POS_SCREEN_RIGHT RESP1, 18

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

	; read select switch
	LDA SWCHB
	AND #%00000010
	BNE .heads_swapped
.cycle_heads
	; do nothing if select button is being held from last frame
	LDA STATE_SWCHB
	AND #%00000010
	BEQ .heads_swapped

	LDY SELECTED_HEAD
	INY
	CPY #NUM_HEADS
	BNE .swap_heads
	LDY #0
.swap_heads
	STY SELECTED_HEAD
	LDA HEADS_TABLE,Y
	STA SPRITE_HEAD_ADDRESS
.heads_swapped
	LDA SWCHB
	STA STATE_SWCHB

	; reset sprite image
	LDA #0
	STA GRP0
	STA GRP1

	; reset colours
	LDA #BIRD_COLOR
	STA COLUP0
	STA COLUP1

	; limit OBSTACLE_SEED to maximum value
	LDY OBSTACLE_SEED
	CPY #OBSTACLES_LEN
	BCC .next_obstacle
	LDY #$0
	STY OBSTACLE_SEED
.next_obstacle

	; limit BRANCH_SEED to maximum value
	LDY BRANCH_SEED
	CPY #BRANCHES_LEN
	BCC .next_branch
	LDY #$0
	STY BRANCH_SEED
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

