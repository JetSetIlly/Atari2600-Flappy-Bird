
	processor 6502
	include vcs.h
	include macro.h
	include vcs_extra.h

; program tunables 
; TODO: alter these according to console difficulty setting

OBSTACLE_SPEED		= $10
OBSTACLE_WINDOW		= $20
CLIMB_RATE				=	$4	 ; number of scanlines bird sprite should fly up per frame
CLIMB_FRAMES			=	$5
GLIDE_FRAMES			=	CLIMB_FRAMES + $2


; program constants

; screen boundaries for bird sprite
BIRD_HIGH	=	PLAY_AREA_SCANLINES
BIRD_LOW	=	$10

; NUSIZ0 and NUSIZ1 value - first four bits set obstacle width
; - last four bits set the player sprite size
OBSTACLE_WIDTH					= %00110101

; speed/width of obstacle when it's being reset
; prevents ugly graphical glitch
OBSTACLE_EXIT_SPEED			= $20
OBSTACLE_EXIT_WIDTH			= %00100101

; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_CLIMB_START_FRAME	=	$0
FLY_DIVE_START_FRAME	=	$FF

BIRD_POS_INIT					=	BIRD_HIGH
FLY_FRAME_INIT				=	FLY_DIVE_START_FRAME

; how often (in frames) each vblank kernel should run
VBLANK_CYCLE_COUNT			=	$3
VBLANK_CYCLE_SPRITE			=	$1
VBLANK_CYCLE_OBSTACLES	=	$2
VBLANK_CYCLE_FOLIAGE		=	$3

; visible display area
FOLIAGE_SCANLINES				= $20
FOREST_FLOOR_SCANLINES	= $03
PLAY_AREA_SCANLINES			= VISIBLE_SCANLINES - FOLIAGE_SCANLINES

; colours
BACKGROUND_COLOR		=	$D2
OBSTACLE_COLOR			= $00
FOLIAGE_COLOR				= $D0
FOREST_FLOOR_COLOR	= $20
FOREST_FLOOR_LEAVES = $22

; data - variables
	SEG.U RAM 
	ORG $80			; start of 2600 RAM
VBLANK_CYCLE				ds 1
PAUSE_COLLISIONS	  ds 1	; flag: $00 = no pause; $FF = paused
FIRE_HELD						ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS						ds 1	; between BIRD_HIGH and BIRD_LOW
FLY_FRAME						ds 1	; <0 = dive; <= CLIMB_FRAMES = climb; <= GLIDE_FRAMES = glide
SPRITE_ADDRESS			ds 2	; which sprite to use in the display kernel

; value for next foliage (playfield) - points to FOLIAGE
NEXT_FOLIAGE				ds 1

; obstacles
OB_0_START					ds 1
OB_0_END						ds 1
OB_1_START					ds 1
OB_1_END						ds 1

; start value for next obstacle - points to OBSTACLES
NEXT_OBSTACLE				ds 1

; pre-calculated ENAM0 and ENAM1 - $0 for obstacle/missile "off" - $2 for "on"
OBSTACLE_0_DRAW				ds 1
OBSTACLE_1_DRAW				ds 1

; pre-calculated GRP0
BIRD_DRAW							ds 1

; player score
SCORE									ds 1
HISCORE								ds 1


; start of cart ROM
	SEG
	ORG $F000	

; sprite data
SPRITES
SPRITE_WINGS_UP			HEX	FF 00 00 00 7F 72 60 40 
SPRITE_WINGS_FLAT		HEX	FF 00 00 00 7F 62 00 00
SPRITE_WINGS_DOWN		HEX	FF 40 60 70 7F 02 00 00
SPRITE_LINES				=	7
; NOTE: first FF in each sprite is a boundry byte - value is unimportant

; table of obstacles (the lower the number, the lower the obstacle)
OBSTACLES				HEX 20 30 40 50 60 70 80
OBSTACLES_CT		= 7		; counting from 0

; foliage - playfield data
FOLIAGE
FOLIAGE_1	.byte %01100000, %10011010, %0011010, %10010000, %00110101, %1101001, %01010000, %01010110, %001101011, %10110000
FOLIAGE_2	.byte %10100110, %00010110, %1000110, %10011010, %00111010, %0010011, %01101101, %01100110, %010110001, %10110101
FOLIAGE_CT	= 5
SCANLINES_PER_FOLIAGE = FOLIAGE_SCANLINES / 7

; ----------------------------------
; SETUP

setup SUBROUTINE setup
	CLEAN_START


; ----------------------------------
; GAME INITIALISATION

game_init SUBROUTINE game_init
	LDA #VBLANK_CYCLE_COUNT
	STA VBLANK_CYCLE

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

	; colours
	LDA #OBSTACLE_COLOR
	STA COLUP0
	STA COLUP1

	; width of obstacles
	LDA #OBSTACLE_WIDTH
	STA NUSIZ0
	STA NUSIZ1

	; playfield priority - foliage in front of obstacles
	LDA #$4
	STA CTRLPF

	; kickstart foliage
	LDA #0
	STA NEXT_FOLIAGE

	; kickstart obstacle
	LDY #$3
	LDA OBSTACLES,Y
	STA OB_0_START
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_0_END
	LDY #$6
	LDA OBSTACLES,Y
	STA OB_1_START
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_1_END
	STY NEXT_OBSTACLE

	; reset score - there's a false point awarded at the beginning because of the initial collision of obstacle 1
	LDA #$FF
	STA SCORE

.position_elements
	; we only need to position elements once. we use the very first frame of the game sequence to do this.
	STA WSYNC

	; make sure beam is off
	LDA	#2
	STA VBLANK

	; reset sprite objects to leftmost of the screen
	; we could use the NEXT_3_CYCLES_IS_SCREEN_LEFT macro
	; but resettting anywhere in the HBLANK will have the
	; same effect
	STA RESP0
	STA RESP1

	; position obstacle trigger (ball) at right most screen edge
	POSITION_RESET_SCREEN_RIGHT RESBL

	; place obstacle 1 (missile 1) at right most screen edge
	POSITION_RESET_SCREEN_RIGHT RESM1

	; place obstacle 0 (missile 1) at screen middle
	POSITION_RESET_SCREEN_MIDDLE RESM0

	; speed of flight
	LDA #OBSTACLE_SPEED
	STA HMM0
	STA HMM1

	; load frame cycle into X ready for beginning of vblank kernel
	; -- we implicitly do the same at the end of the overscan kernel as a
	; side effect of the frame cycle update
	LDX VBLANK_CYCLE


; ----------------------------------
; GAME - VSYNC

game_vsync SUBROUTINE game_vsync
	VSYNC_KERNEL_BASIC

	
; ----------------------------------
; GAME - VBLANK

game_vblank SUBROUTINE game_vblank
	VBLANK_KERNEL_SETUP

	; vblank triage - cycle through vblank kernels every VBLANK_CYCLE frames
	CPX #VBLANK_CYCLE_SPRITE
	BEQ .vblank_player_sprite
	CPX #VBLANK_CYCLE_OBSTACLES
	BEQ .vblank_collisions
	; CPX #VBLANK_CYCLE_FOLIAGE is implied

	; -------------
	; GAME - VBLANK - FOLIAGE

	LDY NEXT_FOLIAGE
	INY
	CPY #FOLIAGE_CT
	BCC .foliage_updated
	LDY #0
.foliage_updated
	STY NEXT_FOLIAGE

	JMP .end_vblank_triage

	; -------------
	; GAME - VBLANK - COLLISIONS

.vblank_collisions

	; check bird collision
	BIT CXM1P
	BMI .bird_collision
	BIT CXM0P
	BVS .bird_collision

	; check for collision of obstacles with backstop
	BIT CXM0FB
	BVS .reset_obstacle_0
	BIT CXM1FB
	BVS .reset_obstacle_1

	; a frame has occurred without obstacle collision
	; reset speed and width of obstacles
	LDA #OBSTACLE_SPEED
	STA HMM0
	STA HMM1
	LDA #OBSTACLE_WIDTH
	STA NUSIZ0
	STA NUSIZ1

	JMP .end_vblank_triage

; TODO: make reset_obstacle_0 and reset_obstacle_1 more efficient
.reset_obstacle_0
	; get new bottom for obstacle 0
	LDY NEXT_OBSTACLE
	LDA OBSTACLES,Y
	STA OB_0_START
	; calculate top of obstacle 0
	CLC
	ADC #OBSTACLE_WINDOW
	STA OB_0_END

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_EXIT_SPEED
	STA HMM0
	LDA #OBSTACLE_EXIT_WIDTH
	STA NUSIZ0

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

	; increase speed / decrease size of obstacle temporarily
	LDA #OBSTACLE_EXIT_SPEED
	STA HMM1
	LDA #OBSTACLE_EXIT_WIDTH
	STA NUSIZ1

.obstacle_reset_done
	INC SCORE
	JMP .end_vblank_triage

.bird_collision
	; TODO: better collision handling
	BRK


	; -------------
	; GAME - VBLANK - USER INPUT

.vblank_player_sprite
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

	; do nothing if fire is being held from last frame
	LDX FIRE_HELD
	BPL .do_flight_anim

	; new fire button press - start new fly animation
	LDX #FLY_CLIMB_START_FRAME
	STX FLY_FRAME

	; flip sprite in response to fire button press
	LDX SPRITE_ADDRESS
	CPX #<SPRITE_WINGS_DOWN
	BEQ .flip_sprite_use_flat

	LDX #<SPRITE_WINGS_DOWN
	STX SPRITE_ADDRESS
	JMP .flip_sprite_end

.flip_sprite_use_flat
	LDX #<SPRITE_WINGS_FLAT
	STX SPRITE_ADDRESS
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
	LDA #<SPRITE_WINGS_FLAT
	STA SPRITE_ADDRESS

	INC FLY_FRAME
	JMP .fly_end

.end_glide
	; we have been gliding for the maximum number of frames
	; begin fly down animation
	LDA #FLY_DIVE_START_FRAME
	STA FLY_FRAME

.fly_down
	; use fly down sprite
	LDA #<SPRITE_WINGS_UP
	STA SPRITE_ADDRESS

	; alter position (FLY FRAME is a 2's complement negative so we can add)
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


	; -------------
	; GAME - VBLANK - PREPARE DISPLAY

.end_vblank_triage
	; reset collision flags every frame
	STA CXCLR

	; setup display kernel

	; turn off obstacles
	LDY #$0
	STY ENAM0
	STY ENAM1

	; ball is enabled until game_play_area subroutine
	LDA #$2
	STA ENABL

	; we have the time so set up foliage subroutine
	LDA #BACKGROUND_COLOR
	STA	COLUBK
	LDA #FOLIAGE_COLOR
	STA COLUPF
	LDY NEXT_FOLIAGE

	; X register will contain the current scanline for the duration of the display kernal
	; starting with FOLIAGE_SCANLINES and then PLAY_AREA_SCANLINES, loaded later
	LDX	#FOLIAGE_SCANLINES

	; set up horizontal movement
	STA WSYNC
	STA HMOVE

	; wait for end of vblank kernel 
	VBLANK_KERNEL_END


; ----------------------------------
; GAME - DISPLAY - FOLIAGE

foliage SUBROUTINE foliage

; X register contains the number of FOLIAGE_SCANLINES remaining

; Y register contains the NEXT_FOLIAGE value
; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
; into FOLIAGE space to set the playfield values

; A should be zero after VBLANK_KERNEL_END

.display_foliage

	; test accumulator (SCANLINES_PER_FOLIAGE)
	CMP #$0
	BNE .next_scanline

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

	; start drawing obstacles if we're halfway through the foliage area
	CPX #FOLIAGE_SCANLINES / 2
	BNE .reset_accumulator
	LDA #$2
	STA ENAM0
	STA ENAM1

.reset_accumulator
	LDA #SCANLINES_PER_FOLIAGE

.next_scanline

	; decrement accumulator (SCANLINES_PER_FOLIAGE)
	SEC
	SBC #$1

	DEX												; 2
	BEQ game_play_area				; 2/3
	STA WSYNC
	JMP .display_foliage			; 3


; ----------------------------------
; GAME - DISPLAY - PLAY AREA

game_play_area SUBROUTINE game_play_area
	; set up play area
	; NOTE: there has not been a WSYNC since the beginning of the last ".display_foliage" loop
	; there's another one at the beginning of ".display_bird". none of the following should
	; cause any visual artefacts

	; turn off ball
	LDA #0
	STA ENABL

	; no playfield in the play area
	STA PF0
	STA PF1
	STA PF2

	; prepare for loop
	LDX #PLAY_AREA_SCANLINES
	LDY #SPRITE_LINES

; X register contains the number of PLAY_AREA_SCANLINES remaining

; Y register contains number of SPRITE_LINES remaining
; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
; to set the sprite line

; -----------------------
.display_bird
	; maximum 76 cycles between WSYNC
	STA WSYNC									; 3

	LDA BIRD_DRAW							; 3
	STA GRP0									; 3

	; maximum 22 cycles in HBLANK
	;	 6 cycles used
	; 
	; 16 cycles until end of HBLANK

.precalc_sprite
	; if we're at scan line number BIRD_POS (ie. where the bird is) - turn on the sprite
	CPX BIRD_POS							; 2
	BCS .precalc_sprite_done	; 2/3

	TYA												; 2
	BMI .precalc_sprite_done	; 2/3
	BEQ	.precalc_sprite_off		; 2/3

	LDA (SPRITE_ADDRESS),Y		; 5
	STA BIRD_DRAW							; 3 
	DEY												; 2
	JMP .precalc_sprite_done	; 3

.precalc_sprite_off
	STY BIRD_DRAW							; 3

.precalc_sprite_done

	; NOTE: save the JMP cycles by making ".next_scanline" a macro
	JMP .next_scanline				; 3

	; longest path
	;   31 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 47
	; 29 cycles remaining
; -----------------------

; -----------------------
.display_obstacle
	; maximum 76 cycles between WSYNC
	STA WSYNC									; 3

	LDA OBSTACLE_0_DRAW				; 3
	STA ENAM0									; 3
	LDA OBSTACLE_1_DRAW				; 3
	STA ENAM1									; 3

	; maximum 22 cycles in HBLANK
	;	 12 cycles used
	; 
	; 10 cycles until end of HBLANK

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
	;		46 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 62
	; 14 cycles remaining
; -----------------------

; -----------------------
.next_scanline
	; decrement current scanline - go to overscan kernel if we have reached zero
	DEX												; 2
	BEQ forest_floor					; 2/3

	; interlace sprite and obstacle drawing
	TXA												; 2
	AND #%00000001						; 2
	BEQ .display_bird	  			; 2/3
	JMP .display_obstacle			; 3
; -----------------------

; ----------------------------------
; GAME - DISPLAY - FOREST_FLOOR 

forest_floor SUBROUTINE forest_floor
	STA WSYNC

	; change colour of playfield to simulate leaves
	; we'll change background colour in the next HBLANK
	LDA #FOREST_FLOOR_LEAVES
	STA COLUPF

	; set up forest floor subroutine
	LDX #FOREST_FLOOR_SCANLINES
	LDY NEXT_FOLIAGE

	; preload accumulator with 0 in time to disable obstacles in the next HBLANK
	LDA #$0

; X register contains the number of FOREST_FLOOR_SCANLINES remaining

; Y register contains the NEXT_FOLIAGE value
; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
; into FOLIAGE space to set the playfield values

.display_forest_floor
	; NOTE: this only runs once

	STA WSYNC
	STA ENAM0
	STA ENAM1
	LDA #FOREST_FLOOR_COLOR
	STA COLUBK
	LDA FOLIAGE,Y
	STA PF0
	INY
	LDA FOLIAGE,Y
	STA PF1
	INY
	LDA FOLIAGE,Y
	STA PF2

.next_scanline
	DEX													; 2
	BEQ game_overscan						; 2/3
	STA WSYNC
	JMP .next_scanline					; 3


; ----------------------------------
; GAME - OVERSCAN 

game_overscan SUBROUTINE game_overscan
	STA WSYNC									; 3

	OVERSCAN_KERNEL_SETUP

	; update frame cycle
	LDX VBLANK_CYCLE
	DEX
	BNE .store_frame_cycle
	LDX #VBLANK_CYCLE_COUNT
.store_frame_cycle
	STX VBLANK_CYCLE

	; limit NEXT_OBSTACLE to maximum value
	LDY NEXT_OBSTACLE
	CPY #OBSTACLES_CT
	BCC .limit_obstacle_bott
	LDY #$0
	STY NEXT_OBSTACLE
.limit_obstacle_bott

	OVERSCAN_KERNEL_END

	JMP game_vsync


; ----------------------------------
; MACHINE INITIALISATION 

initialisation SUBROUTINE initialisation

	ORG $FFFA
	.word setup		; NMI
	.word setup		; RESET
	.word setup		;	IRQ

