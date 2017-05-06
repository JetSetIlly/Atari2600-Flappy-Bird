
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

BASE_BACKGROUND_COLOR		=	$80

; how often (in frames) each vblank kernel should run
VBLANK_CYCLE_COUNT			=	$3
VBLANK_CYCLE_SPRITE			=	$1
VBLANK_CYCLE_OBSTACLES	=	$2
VBLANK_CYCLE_SPARE			=	$3

; screen boundaries for bird sprite
BIRD_HIGH	=		$C0
BIRD_LOW	=		$10

; number of frames (since fire button was last pressed) for the bird to fly up, and then glide
FLY_CLIMB_START_FRAME	=		$0
FLY_DIVE_START_FRAME	=		$FF

BIRD_INIT				=	$BF
FLY_FRAME_INIT	=	FLY_DIVE_START_FRAME


; data - variables
	SEG.U RAM 
	ORG $80			; start of 2600 RAM
FRAME_CYCLE					ds 1
FIRE_HELD						ds 1	; reflects INPT4 - positive if held from prev frame, negative if not
BIRD_POS						ds 1	; between BIRD_HIGH and BIRD_LOW
FLY_FRAME						ds 1	; <0 = dive; <= CLIMB_FRAMES = climb; <= GLIDE_FRAMES = glide
SPRITE_ADDRESS			ds 2	; which sprite to use in the display kernel

; existing obstacles
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

; current background colour
CURRENT_BG_COL				ds 1


; start of cart ROM
	SEG
	ORG $F000	

; sprite data
SPRITES
SPRITE_WINGS_UP			HEX	FF 00 00 00 7F 72 60 40 
SPRITE_WINGS_FLAT		HEX	FF 00 00 00 7F 62 00 00
SPRITE_WINGS_DOWN		HEX	FF 40 60 70 7F 02 00 00
SPRITE_LINES				.byte	7
; NOTE: first FF in each sprite is a boundry byte - value is unimportant

; table of obstacles (the lower the number, the lower the obstacle)
OBSTACLES				HEX 20 30 40 50 60 70 80
OBSTACLES_CT		= 7		; counting from 0


; ----------------------------------
; SETUP

setup SUBROUTINE setup
	CLEAN_START


; ----------------------------------
; GAME INITIALISATION

game_init SUBROUTINE game_init
	LDA #VBLANK_CYCLE_COUNT
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
	LDY #$0
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

.position_elements
	; we only need to position elements once. we use the very first frame of the game sequence to do this.
	STA WSYNC

	; make sure beam is off
	LDA	#2
	STA VBLANK

	; reset sprite objects to leftmost of the screen
	; we could use the NEXT_3_CYCLES_IS_SCREEN_LEFT macro
	; but resettting anywhere in the horizontal blank will have the
	; same effect
	STA RESP0
	STA RESP1
	STA RESBL

.draw_ball
	; the ball will be enabled throughout the game
	STA ENABL
	; fine tune positioning of the ball
	LDA #$21
	STA HMBL
	STA WSYNC
	STA HMOVE

	; place obstacle 1 (missile 1) at right most screen edge
	POSITION_RESET_SCREEN_RIGHT RESM1

	; place obstacle 0 (missile 1) at screen middle
	POSITION_RESET_SCREEN_MIDDLE RESM0

	; reset all horizontal movement before setting the speed of flight (which will
	; persist throughout the game)
	; NOTE: we need to wait at least 24 machine cycles since the ball HMOVE above
	; or the motion will not have occurred
	STA HMCLR
	
	; speed of flight
	LDA #OBSTACLE_SPEED
	STA HMM0
	STA HMM1

	; load frame cycle into X ready for beginning of vblank kernel
	; -- we implicitly do the same at the end of the overscan kernel as a
	; side effect of the frame cycle update
	LDX FRAME_CYCLE

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

	; vblank triage - cycle through vblank kernels every FRAME_CYCLE frames
	CPX #VBLANK_CYCLE_SPRITE
	BEQ .vblank_player_sprite
	CPX #VBLANK_CYCLE_OBSTACLES
	BEQ .vblank_collisions

	; -------------
	; VBLANK - SPARE
	JMP .end_vblank_triage
	; END - VBLANK - SPARE
	; -------------

	; -------------
	; VBLANK - COLLISIONS
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

	; a frame has occurred without obstacle collision - reset speed and size of both obstacles (missiles)
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
	; prevents ugly graphical glitch
	LDA #OBSTACLE_DISAPPEAR_SPEED
	STA HMM0
	LDA #0
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
	; prevents ugly graphical glitch
	LDA #OBSTACLE_DISAPPEAR_SPEED
	STA HMM1
	LDA #0
	STA NUSIZ1

.obstacle_reset_done
	JMP .end_vblank_triage

.bird_collision
	; TODO: better collision handling
	BRK

	; END - VBLANK - COLLISIONS
	; -------------


	; -------------
	; VBLANK - PLAYER SPRITE
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

	; END - VBLANK - PLAYER SPRITE
	; -------------

.end_vblank_triage
	; reset collision flags every frame
	STA CXCLR

	; setup display kernel

	; set background colour
	LDA #BASE_BACKGROUND_COLOR
	STA CURRENT_BG_COL
	STA	COLUBK

	; first sprite line should be empty ie. 
	;		LDA #%0
	;		STA BIRD_DRAW
	; is implied

	; preload Y register with number of sprite lines
	LDY SPRITE_LINES

	; X register will now contain the current scanline for the duration of the display kernal
	LDX	#VISIBLE_SCANLINES

	; set up horizontal movement
	STA WSYNC
	STA HMOVE

	; wait for end of vblank kernel 
	VBLANK_KERNEL_END


; ----------------------------------
; > DISPLAY KERNEL

; X register contain the current scanline for the duration of the display kernal

; Y register contains number of SPRITE_LINES remaining
; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
; to set the sprite line

.display_loop

; -----------------------
.display_sprite
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

.precalc_background
	TXA												; 2
	AND #%00011111						; 2
	BNE .next_scanline				; 2/3
	LDA CURRENT_BG_COL				; 3
	CLC												; 2
	ADC #$2										; 2
	STA CURRENT_BG_COL				; 3

	; NOTE: save the JMP cycles by making ".next_scanline" a macro
	JMP .next_scanline				; 3

	; longest path
	;   51 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 67
	; 9 cycles remaining
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

.change_background
	LDA CURRENT_BG_COL				; 3
	STA	COLUBK								; 3

	; 4 cycles until end of HBLANK

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
	;		51 cycles
	; + 13 for ".next_scanline"
	; + 3 for WSYNC
	; = 67
	; 9 cycles remaining
; -----------------------

; -----------------------
.next_scanline
	; decrement current scanline - go to overscan kernel if we have reached zero
	DEX												; 2
	BEQ .overscan_kernel			; 2/3

	; interlace sprite and obstacle drawing
	TXA												; 2
	AND #%00000001						; 2
	BEQ .display_sprite				; 2/3
	JMP .display_obstacle			; 3
; -----------------------


; ----------------------------------
; > OVERSCAN KERNEL

.overscan_kernel
	STA WSYNC									; 3

	OVERSCAN_KERNEL_SETUP

	; update frame cycle
	LDX FRAME_CYCLE
	DEX
	BNE .store_frame_cycle
	LDX #VBLANK_CYCLE_COUNT
.store_frame_cycle
	STX FRAME_CYCLE

	; limit NEXT_OBSTACLE to maximum value
	LDY NEXT_OBSTACLE
	CPY #OBSTACLES_CT
	BCC .limit_obstacle_bott
	LDY #$0
	STY NEXT_OBSTACLE
.limit_obstacle_bott

	OVERSCAN_KERNEL_END

	JMP .vertical_loop


; ----------------------------------
; MACHINE INITIALISATION 

initialisation SUBROUTINE initialisation

	ORG $FFFA
	.word setup		; NMI
	.word setup		; RESET
	.word setup		;	IRQ

