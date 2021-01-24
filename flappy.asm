; Copyright (c) 2017-2021, Stephen Illingworth
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    processor 6502 
    include vcs.h
    include macro.h
    include vcs_extra.h
    include vcs_positioning.h
    include vcs_sfx.h
    include dasm_extra.h

PLUSROM = TRUE
PAL60   = FALSE

MDL_TYPE_CHECKING = TRUE
MDL_RANGE_CHECKING = TRUE

SHOW_READY_SCREEN = TRUE
RANDOM_TRUNK = FALSE
HARD_DIFFICULTY = FALSE

; ----------------------------------
; PlusROM Hotspots
   IF PLUSROM
WriteToBuffer     = $1ff0
WriteSendBuffer   = $1ff1
ReceiveBuffer     = $1ff2
ReceiveBufferSize = $1ff3
HIGHSCORE_ID      = 34
   ENDIF



; ----------------------------------
; * DATA - COLOURS

; colours
    IF PAL60
BIRD_COLOR                    = $06

FOLIAGE_BACKGROUND            = $32
FOLIAGE_COLOR                 = $30

FOREST_BACKGROUND             = $32
FOREST_COLOR                  = $20

SWAMP_BACKGROUND              = $72
SWAMP_COLOR                   = $70

SCORING_BACKGROUND            = $00
SCORE_COLOR                   = $0E
HISCORE_COLOR                 = $26

OKAY_COLOR                    = $2E
    ELSE
BIRD_COLOR                    = $06

FOLIAGE_BACKGROUND            = $D2
FOLIAGE_COLOR                 = $D0

FOREST_BACKGROUND             = $D2
FOREST_COLOR                  = $E0

SWAMP_BACKGROUND              = $B2
SWAMP_COLOR                   = $B0

SCORING_BACKGROUND            = $00
SCORE_COLOR                   = $0E
HISCORE_COLOR                 = $F6

OKAY_COLOR                    = $2E
    ENDIF
; ----------------------------------
; * DATA - CONSTANTS

; values for CTRLPF
CTRLPF_FOLIAGE  = %00100100
CTRLPF_PLAYAREA = %00100000
CTRLPF_SWAMP    = %00100100

; death rates
DEATH_COLLISION_SPEED    = $02 ; speed at which player sprite moves foward during death collision
DEATH_DROWNING_LEN       = $0D ; should be the same BIRD_LOW + 1

; NUSIZ0 and NUSIZ1 values - first four bits set obstacle width
; - last four bits set the player sprite size and duplication number
; - the two nibbles will be OR'd to create the NUSIZ value
;
; size/width of obstacles and player sprites changes frequently throughout the game.
;
; summary of changes
; ==================
;        o during game initialisation
;        o during score routine
;        o during overscan (depending on the play state)
;        o during game play area, to simulate a solitary branch (NUSIZ1) only
;
OBSTACLE_WIDTH               = %00100000        ; quad width
BRANCH_WIDTH                 = %00110000        ; octuple width
BALL_WIDTH                   = 001100000        ; octuple width
SCORE_DIGITS_SIZE            = %00000101   ; single instance, double width

; bird sprite "coords"
BIRD_VPOS_INIT                = BIRD_HIGH / 4 * 3
BIRD_HPOS_PLAY_POS            = $0C

    IF HARD_DIFFICULTY == TRUE
WING_WIDTH                   = %00000101   ; single instance, double width
HEAD_OFFSET_FROM_WINGS       = $07
HEAD_WIDTH                   = %00000000   ; single instance, single width
BIRD_HPOS_INIT                = $00
    ELSE
WING_WIDTH                   = %00000000    ; single instance, single width
HEAD_OFFSET_FROM_WINGS       = $02
HEAD_WIDTH                   = %00000000   ; single instance, single width
BIRD_HPOS_INIT                = $05
    ENDIF

; ok sign width
OK_SIGN_WIDTH                   = %00000101     ; single instance, double width
QMARK_SIGN_WIDTH                = %00000000     ; single instance, single width

; play state -- death states are all negative
PLAY_STATE_PLAY             = $00
PLAY_STATE_APPROACH         = $01
PLAY_STATE_READY            = $02
PLAY_STATE_COLLISION        = $FF
PLAY_STATE_DROWN            = $FE

; visible scan line usage
; =======================
; note the extra $04 scanlines in the score area are:
; * one at the start of the subroutine
; * two position resets
; * and another one because DIGIT_LINES breaks on -1 not 0 (BMI instead of BEQ)
;
; note that we don't loop the swamp WSYNCs because there are so few and we'll
; be doing something useful and different on each line
; care should be taken to update this value, if we increase the number of WSYNCs
SCANLINES_FOLIAGE         = $20
SCANLINES_PLAYAREA        = DISPLAY_SCANLINES - SCANLINES_FOLIAGE - SCANLINES_SWAMP - SCANLINES_SCOREAREA
SCANLINES_SWAMP           = $04
SCANLINES_SCOREAREA       = DIGIT_LINES + $04
SCANLINES_PER_FOLIAGE     = SCANLINES_FOLIAGE / 8

; point at which to change the sprite color - to enable effective swamp colouring
; the value should be odd because it is checked for on the odd scanlines
SPLASH_LINE = $09

; screen boundaries for bird sprite
BIRD_HIGH               = SCANLINES_PLAYAREA
BIRD_LOW                = SCANLINES_SWAMP + SCANLINES_SCOREAREA

    ; DASM directives to output number of scanlines used
    DASM_MESSAGE "Scanline Layout"
    DASM_MESSAGE "---------------"
    DASM_MESSAGE "FOLIAGE = ", SCANLINES_FOLIAGE
    DASM_MESSAGE "PLAYAREA = ", SCANLINES_PLAYAREA
    DASM_MESSAGE "SWAMP = ", SCANLINES_SWAMP
    DASM_MESSAGE "SCORE AREA = ", SCANLINES_SCOREAREA
    DASM_MESSAGE "TOTAL = ", SCANLINES_FOLIAGE + SCANLINES_PLAYAREA + SCANLINES_SWAMP + SCANLINES_SCOREAREA, "(", DISPLAY_SCANLINES, ")"


; ----------------------------------
; * DATA - RAM
    SEG.U RAM
    ORG $80            ; start of 2600 RAM


; VCS_EXTRA.H SCOPE
; - variables beginning with __ are required by routines in vcs_*.h
__MULTI_COUNT_STATE            ds 1
__STATE_INPT4                  ds 1
__STATE_SWCHB                  ds 1

; VCS_AUDIO.H SCOPE
__SFX_NEW_EVENT                ds 1
__SFX_QUEUE_EVENT              ds 1
__SFX_SUB_FRAMES               ds 1


; LOCAL SCOPE
; - can be resused between subroutines
; - don't access these locations accept through aliases defined
; in the subroutine that uses them
_localA                        ds 1
_localB                        ds 1
_localC                        ds 1
_localD                        ds 1
_localE                        ds 1
_localF                        ds 1
_localG                        ds 1


; GLOBAL SCOPE 
; - data that persists for a long time, say, frame to frame

PLAY_STATE                       ds 1    ; state of play - zero is normal, negative is death
SELECTED_HEAD                    ds 1    ;    index into HEADS_TABLE

; selected flight pattern - see FLIGHT_PATTERN macros
FLIGHT_PATTERN                   ds 2

; which bird/detail sprite to use in the display kernel
ADDRESS_SPRITE_0                ds 2
ADDRESS_SPRITE_1                ds 2

BIRD_VPOS                       ds 1    ; between BIRD_HIGH and BIRD_LOW
BIRD_HPOS                       ds 1    ; current horizontal position of bird (in pixels)
BIRD_HEAD_OFFSET                ds 1    ; number of pixels the head is offset from BIRD_HPOS

; PATTERN_INDEX's meaning changes depending on PLAY_STATE
;
; if PLAY_STATE == PLAY_STATE_PLAY || PLAY_STATE_APPROACH || PLAY_STATE_COLLISION
;    then
;        PATTERN_INDEX indexes FLIGHT_PATTERN
;
; if PLAY_STATE == PLAY_STATE_DROWN
;    then
;        PATTERN_INDEX counts number of frames to game reset
;
PATTERN_INDEX               ds 1

; seed values for /next/ foliage, obstacle and branch
FOLIAGE_SEED                ds 1
OBSTACLE_SEED               ds 1
BRANCH_SEED                 ds 1

; obstacle defintions
OB_0                        ds 2
OB_1                        ds 2
OB_0_BRANCH                 ds 1
OB_1_BRANCH                 ds 1
OB_0_HPOS                   ds 1
OB_1_HPOS                   ds 1
OB_0_SPEED                  ds 1
OB_1_SPEED                  ds 1

; current state of background trees (playfield data)
FOREST_MID_0                ds 1
FOREST_MID_1                ds 1
FOREST_MID_2                ds 1

; colour of player/missile 0 below the splash line
; o BIRD_COLOR in most instances
; o SWAMP_COLOR during PLAY_STATE_DROWN
SPLASH_COLOR                ds 1

; player score
SCORE                       ds 1
HISCORE                     ds 1

    ; DASM directive - echo number of bytes left in RAM
    DASM_MESSAGE "",($100 - *) , "bytes of RAM left"


; ----------------------------------
; * DATA - GFX

    SEG
    ORG $F000        ; start of cart ROM
    
DATA_SEGMENT 

; graphics data
; =============

; number of lines in each sprite - same for everything, except digits
SPRITE_LINES        =    7

; number of line each digit
DIGIT_LINES    = 4

EMPTY                 HEX 00 00 00 00 00 00 00 00

; ready state text
TEXT_OK               HEX 00 EA AA AA AC EA 00 00
TEXT_QMARK            HEX 00 18 00 1E 42 7E 00 00 

; bird sprite - heads
WINGS
WINGS_UP              HEX    00 00 00 00 30 70 60 40 
WINGS_FLAT            HEX    00 00 00 00 30 60 00 00
WINGS_DOWN            HEX    00 40 60 70 30 00 00 00

; bird sprite - wings
    PAGE_CHECK
HEADS
HEAD_GIRL_A           HEX 00 00 00 00 20 1C 18 20
HEAD_BOY_A            HEX 00 00 00 00 20 1C 18 00
HEAD_GIRL_B           HEX 00 00 00 00 20 1C 58 20
HEAD_BOY_B            HEX 00 00 00 00 20 1C 18 04
    PAGE_CHECK_END "HEADS"

NUM_HEADS              = 4
HEADS_TABLE           .byte <HEAD_GIRL_A, <HEAD_BOY_A, <HEAD_GIRL_B, <HEAD_BOY_B

; splash sprite 
_SPLASH   HEX 18 18 
SPLASH    HEX 00 00 24 42 24 00 18 00

; digits - used for scoring
    PAGE_CHECK
DIGITS
DIGIT_0    HEX 3C 24 24 24 3C
DIGIT_1    HEX 08 08 08 18 08
DIGIT_2    HEX 3C 20 18 04 3C
DIGIT_3    HEX 3C 04 08 04 3C
DIGIT_4    HEX 08 3C 28 20 20
DIGIT_5    HEX 38 04 3C 20 3C
DIGIT_6    HEX 3C 24 3C 20 20
DIGIT_7    HEX 04 04 0C 04 3C
DIGIT_8    HEX 3C 24 3C 24 3C
DIGIT_9    HEX 04 04 3C 24 3C
    PAGE_CHECK_END "DIGITS"
DIGIT_TABLE    .byte <DIGIT_0, <DIGIT_1, <DIGIT_2, <DIGIT_3, <DIGIT_4, <DIGIT_5, <DIGIT_6, <DIGIT_7, <DIGIT_8, <DIGIT_9

; foliage - playfield data
; (see "foliage" subroutine for full and laboured explanation)
FOLIAGE .byte %01100000, %10011010, %00111010, %10010000, %00110101, %11010001, %01010000, %01010110, %00110011, %10101000, %10100110, %00010110, %10010110, %10011010, %00111010, %00101011, %01101101, %01100110, %01011001, %11001101, %00101010, %01101100, %00100010, %10011010, %00111010, %00110011, %01101101, %01100110, %01011001, %10110101
MAX_FOLIAGE_SEED    = 7

; background forest - initial values
FOREST_MID_0_INIT    .byte %00100000
FOREST_MID_1_INIT    .byte %00011000
FOREST_MID_2_INIT    .byte %00001000
FOREST_STATIC_0      .byte %10000000
FOREST_STATIC_1      .byte %00100000
FOREST_STATIC_2      .byte %10010000


; ----------------------------------
; * DATA - OBSTACLES

; obstacles tables
; ===============
;
; * obstacle enable precalc table based on play ara of 153 lines
;        - may need tuning if SCANLINES_PLAYAREA changes
; * works as a barrel shifter
;        - 153 lines before AND after window
;        - there's room for optimisation
; * the window (the zeroes in the table) is 32 bytes(lines)

    ; make sure we start on a page boundary - we don't want to cross a page boundary
    ORG $F100

SET_OBSTACLE_TABLE 
. HEX 02 02 02 02
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
; 32 scanlines (pixels) of 00 (this is the size of the gap)
. HEX 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
. HEX 02 02 02 02
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

; list of obstacles to use
OBSTACLES        HEX 05 15 25 35 45 55 65
OBSTACLES_LEN    = 7

; list of branches
; note that we test for branch on the even scanlines of the play area so these values should be even
BRANCHES        HEX 18 24 30 38 44 58 70
BRANCHES_LEN    = 7


; ----------------------------------
; * DATA - FLIGHT PATTERNS

; flight patterns
; ===============
; first byte is the length of the flight pattern proper (not including last byte or the first
; byte in the array).
;
; last byte is the index to initialse the pattern index to (usually start of the glide period)
;
;    intervening bytes are the flight pattern proper
;        note: negative pattern values less than -BIRD_LOW (ie. -12) may result in undefined behaviour
;        as this may cause the bird to descend lower than the screen (ie. a negative value)
;
; see FLIGHT_PATTERN macros
FLIGHT_PATTERNS
EASY_FLIGHT_PATTERN .byte 20, 4, 4, 4, 4, 4, 0, 0, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, 6

    DEF_FINE_POS_TABLE


; ----------------------------------
; * DATA - SFX

    PAGE_CHECK
SFX_TABLE        HEX FF 00 00 00 00 00
SFX_FLAP         HEX FF 01 F8 03 0F 79
SFX_COLLISION    HEX 00 06 87 31 01 49
.                HEX 00 06 06 30 00 07
.                HEX FF 06 06 31 00 04
SFX_SPLASH       HEX 00 04 08 04 00 09
.                HEX    00 04 08 03 00 09
.                HEX    00 05 08 05 00 09
.                HEX    00 06 08 06 00 06
.                HEX    00 06 08 08 00 03
.                HEX    00 06 08 09 02 01
.                HEX    FF 02 06 09 03 01
    PAGE_CHECK_END "SFX_TABLE"


; ----------------------------------
; * MACROS - FLIGHT PATTERN

    MAC APPLY_FLIGHT_PATTERN
        ; < S = PATTERN_INDEX
        ; ! AYCZVN

        LDY PATTERN_INDEX
        LDA (FLIGHT_PATTERN),Y
        CLC
        ADC BIRD_VPOS
    ENDM

    MAC RESET_FLIGHT_PATTERN
        ; > {death spiral} [boolean
        ; ! AYCZVN
        ; + PATTERN_INDEX

        ; initialise PATTERN_INDEX for selected FLIGHT_PATTERN
        ; (see FLIGHT_PATTERN definitions for memory layout explanation)
        ; first byte in (FLIGHT_PATTERN) is the pattern's length 
        LDY #$0
        LDA (FLIGHT_PATTERN),Y
        ; last byte of (FLIGHT_PATTERN) is the pattern's index initialisation value
        TAY
        INY
        LDA (FLIGHT_PATTERN),Y

        IF {1} == TRUE
            CMP PATTERN_INDEX
            ; if current PATTERN_INDEX is less than the initialisation value, then don't initialise the index
            BCS .no_store_index
        ENDIF
        STA PATTERN_INDEX
.no_store_index
    ENDM

    MAC UPDATE_FLIGHT_PATTERN 
        ; > {cycle pattern} [B}
        ; > Y = PATTERN_INDEX
        ; ! AYCZVN
        ; + PATTERN_INDEX

        INY
        TYA
        LDX #$0         ; X = 0 -> length byte of FLIGHT_PATTERN
        CMP (FLIGHT_PATTERN,X)
        BCC .store_index

        IF {1} == TRUE
            LDA #$01
        ELSE
            DEY
            TYA
        ENDIF
.store_index
        STA PATTERN_INDEX
    ENDM


; ----------------------------------
; * MACROS - OTHER

    MAC POSITION_BIRD_SPRITE
        FINE_POS RESP0, BIRD_HPOS
        LDA BIRD_HPOS
        CLC
        ADC BIRD_HEAD_OFFSET
        FINE_POS_A RESP1
    ENDM

    MAC NEW_OBSTACLE
        ; > {obstacle number} [0,1]
        ; ! AXCZVN
        ; + OB_0, OB_1

        IF {1} != 0 && {1} != 1
            DASM_MACRO_ERROR "'NEW_OBSTACLE': {1} must be 0 or 1"
        ENDIF

        LDX OBSTACLE_SEED
        LDA #<SET_OBSTACLE_TABLE
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


    MAC DROWNING_PLAY_STATE
        ; ! ACZVN
        ; + BIRD_VPOS, PATTERN_INDEX, PLAY_STATE

        ; put game into drowning state
        ;        o make sure bird is at the lowest position
        ;        o use pattern index to measure length of drowning animation
        ;        o point ADDRESS_SPRITE_0 to SPLASH
        ;        o alter BIRD_HEAD_OFFSET and BIRD_HPOS
        ;        o SPLASH_COLOR = SWAMP_COLOR
        LDA #BIRD_LOW
        STA BIRD_VPOS
        ; --
        LDA #DEATH_DROWNING_LEN
        STA PATTERN_INDEX
        ; --
        LDA #<SPLASH
        STA ADDRESS_SPRITE_0
        ; --
        LDA #PLAY_STATE_DROWN
        STA PLAY_STATE
      IF PLUSROM
        JSR SendPlusROMScore
      ENDIF
        ; --
        DEC BIRD_HEAD_OFFSET
        DEC BIRD_HEAD_OFFSET
        INC BIRD_HPOS
        INC BIRD_HPOS
        ; --
        LDA #SWAMP_COLOR
        STA SPLASH_COLOR

        ; obstacle flicker phase correction done in the overscan, otherwise
        ; there will be a frame where the wrong obstacle definitions are used

        SFX_LOAD SFX_SPLASH

    ENDM

    MAC FOLIAGE_ANIMATION
        ; > {animate forest background} [B]
        ; ! YCZVN
        ; + FOREST_MID_0, FOREST_MID_1, FOREST_MID_2, FOLIAGE_SEED

        ; cycle playfield data used to illustrate foliage, and by
        ; association, the playfield used for the water/swamp

        LDY FOLIAGE_SEED
        INY
        CPY #MAX_FOLIAGE_SEED
        BCC .foliage_updated
        LDY #0

        ; rotate forest whenever foliage chaos cycle resets
        IF {1} == TRUE
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
   JMP game_state_init

; ----------------------------------
; TITLE SCREEN

; TODO: finish title screen

title_screen SUBROUTINE title_screen
.vsync
    VSYNC_KERNEL_BASIC

.vblank
    VBLANK_KERNEL_SETUP
    NEW_TRIGGER_CHECK 0
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
    STA ADDRESS_SPRITE_0+1
    STA ADDRESS_SPRITE_1+1
    LDA #>FLIGHT_PATTERNS
    STA FLIGHT_PATTERN+1

    LDA #>SET_OBSTACLE_TABLE
    STA OB_0+1
    STA OB_1+1

    ; initialise head
    LDY #$0
    STY SELECTED_HEAD
    LDA HEADS_TABLE,Y
    STA ADDRESS_SPRITE_1

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

    ; initialise NUSIZ values
    LDA #(OBSTACLE_WIDTH | WING_WIDTH)
    STA NUSIZ0
    LDA #(OBSTACLE_WIDTH | HEAD_WIDTH)
    STA NUSIZ1

	JSR game_restart
	JMP game_overscan

; ----------------------------------
; GAME - RESTART

    ; registers that are reset in game_restart subroutine are altered during gameplay
    ; and need to be reset on game restart
game_restart SUBROUTINE game_restart
    ; clear any active collisions
    STA CXCLR

    NEW_TRIGGER_CHECK 0

    MULTI_COUNT_SETUP

    ; save hiscore
    LDA SCORE
    SED
    CMP HISCORE 
    BCC .done_hiscore
    STA HISCORE
.done_hiscore
    CLD

    ; position elements

    ; make sure movement registers are zero
    STA HMCLR

    ; position both obstacles inside the activision-border
    FINE_POS_LEFT RESM0, OB_0_HPOS, 0, 4
    FINE_POS_LEFT RESM1, OB_1_HPOS, 0, 8

    ; obstacle 0 starts moving immediately - obstacle 1 will begin moving later
    LDA #1
    STA OB_0_SPEED
    LDA #0
    STA OB_1_SPEED 

    ; reset OB_0_BRANCH to zero - it should always be zero except when it is being
    ; used to swap the contents of OB_1_BRANCH during PLAY_STATE_DROWN.
    ; we're forcing the resetting of it's value here becaus PLAY_STATE_DROWN may have
    ; occurred out of phase and left the real value of OB_1_BRANCH in OB_0_BRANCH.
    ; the consequence of this is that OB_1_BRANCH may lose its real value for the
    ; next play ; but it doesn't really matter
    STA OB_0_BRANCH

    ; put game into ready state
    ;        o reset score
    ;        o initialise vertical and horizontal position
    ;        o use flat wings sprite at start
    ;        o reset BIRD_HEAD_OFFSET
    ;        o SPLASH_COLOR = BIRD_COLOUR
    LDA #PLAY_STATE_READY
    STA PLAY_STATE
    ; --
    LDA #$0
    STA SCORE
    ; --
    LDA #BIRD_VPOS_INIT
    STA BIRD_VPOS
    LDA #BIRD_HPOS_INIT
    STA BIRD_HPOS
    ; --
    LDA #<WINGS_FLAT
    STA ADDRESS_SPRITE_0
    ; --
    LDA #HEAD_OFFSET_FROM_WINGS
    STA BIRD_HEAD_OFFSET
    ; --
    LDA #BIRD_COLOR
    STA SPLASH_COLOR

    SFX_ENGINE_INIT
	RTS

; ----------------------------------
; GAME - VSYNC / VBLANK

game_vsync SUBROUTINE game_vsync
    VSYNC_KERNEL_BASIC

game_vblank SUBROUTINE game_vblank
    VBLANK_KERNEL_SETUP

    LDX PLAY_STATE
    BEQ .far_jmp_play

    CPX #PLAY_STATE_APPROACH
    BEQ .far_jmp_approach

    CPX #PLAY_STATE_COLLISION
    BEQ .far_jmp_collision

    CPX #PLAY_STATE_DROWN
    BEQ .far_jmp_drown

    JMP game_vblank_ready

.far_jmp_collision
    JMP game_vblank_death_collision

.far_jmp_drown
    JMP game_vblank_death_drown

.far_jmp_approach
    JMP game_vblank_approach

.far_jmp_play
    JMP game_vblank_main_triage


; ----------------------------------
; GAME - VBLANK - READY STATE

game_vblank_ready SUBROUTINE game_vblank_ready
    IF SHOW_READY_SCREEN == TRUE
      ; check for user input
      NEW_TRIGGER_CHECK 0
      BMI .ready_state_triage
    ENDIF

    ; put into approach state
    ;        o set correct flight pattern
    ;        o change player sprites to point to wings/head data
    ;        o put bird sprite at correct vertical positions
    ;        o jump to vblank approach
    LDA #PLAY_STATE_APPROACH
    STA PLAY_STATE

    ; TODO: use flight pattern depending on switch
    LDA #<EASY_FLIGHT_PATTERN
    STA FLIGHT_PATTERN
    RESET_FLIGHT_PATTERN FALSE

    LDA #<WINGS_FLAT
    STA ADDRESS_SPRITE_0
    LDY SELECTED_HEAD
    LDA HEADS_TABLE,Y
    STA ADDRESS_SPRITE_1

    LDA #BIRD_VPOS_INIT
    STA BIRD_VPOS

    JMP game_vblank_approach

    ; update every three frames
    ; - same sequence as main play state
.ready_state_triage
    MULTI_COUNT_THREE_CMP 1
    BEQ .prepare_display
    BPL .update_foliage
    JMP .prepare_display

.update_foliage
    FOLIAGE_ANIMATION TRUE

.prepare_display
    MULTI_COUNT_TWO_CMP
    BEQ .display_ready_logo

.display_empty
    LDA #<EMPTY
    STA ADDRESS_SPRITE_0
    LDA #<EMPTY
    STA ADDRESS_SPRITE_1
    JMP game_vblank_end

.display_ready_logo
    LDA #90
    STA BIRD_VPOS
    LDA #OKAY_COLOR
    STA COLUP0
    STA COLUP1
    LDA #<TEXT_OK
    STA ADDRESS_SPRITE_0
    LDA #<TEXT_QMARK
    STA ADDRESS_SPRITE_1
    FINE_POS RESP0, #72
    FINE_POS RESP1, #88
    JMP game_vblank_end

; ----------------------------------
; GAME - VBLANK - DEATH - COLLISION

game_vblank_death_collision SUBROUTINE game_vblank_death_collision
    ; update display elements every three frames
    ; note that we do all updating on the same frame, unlike during PLAY_STATE_PLAY
    ; where we update different elements (sprite, foliage) on different frames
    MULTI_COUNT_THREE_CMP 0
    BEQ .update_bird
    BPL .update_foliage
    JMP .prepare_display

.update_foliage
    FOLIAGE_ANIMATION FALSE
    JMP .prepare_display

.update_bird
    ; alter wing position - simulates furious flapping
    LDA ADDRESS_SPRITE_0
    CMP #<WINGS_UP
    BEQ .use_wings_flat
    CMP #<WINGS_FLAT
    BEQ .use_wings_down
.use_wings_up
    LDA #<WINGS_UP
    STA ADDRESS_SPRITE_0
    JMP .wings_updated
.use_wings_flat
    LDA #<WINGS_FLAT
    STA ADDRESS_SPRITE_0
    JMP .wings_updated
.use_wings_down
    LDA #<WINGS_DOWN
    STA ADDRESS_SPRITE_0
.wings_updated

    ; apply flight pattern and check for drowning state
    APPLY_FLIGHT_PATTERN
    CMP #BIRD_LOW
    BCC    .enter_drowning_state

    ; limit bird height
    CMP #BIRD_HIGH
    BCC .update_pattern_idx
    LDA #BIRD_HIGH

.update_pattern_idx 
    STA BIRD_VPOS
    UPDATE_FLIGHT_PATTERN FALSE

    ; propel bird forward - after drowning check
    LDA BIRD_HPOS
    CLC
    ADC #DEATH_COLLISION_SPEED
    STA BIRD_HPOS

    JMP .prepare_display

.enter_drowning_state
    DROWNING_PLAY_STATE

.prepare_display
    POSITION_BIRD_SPRITE
    JMP game_vblank_end


; ----------------------------------
; GAME - VBLANK - DEATH - DROWN

game_vblank_death_drown SUBROUTINE game_vblank_death_drown
    ; slow down death animation
    MULTI_COUNT_THREE_CMP 0
    BEQ .update_bird
    BPL .update_foliage
    JMP .prepare_display

.update_foliage
    FOLIAGE_ANIMATION FALSE
    JMP .prepare_display

.update_bird
    ; end drowning after PATTERN_INDEX (initialised to DEATH_DROWNING_LEN) 
    DEC PATTERN_INDEX
    BEQ .drowning_end

    ; decrease bird sprite position
    DEC BIRD_VPOS
    DEC ADDRESS_SPRITE_0

    JMP .prepare_display

.drowning_end
	JSR game_restart
	JMP game_vblank_ready

.prepare_display
    POSITION_BIRD_SPRITE

    ; flicker obstacle 0 and 1 positions and display both using only obstacle 1
    ; o hiding obstacle 0 in the activision border
    ; o we do this because we don't want the setting of COLUP0 to SPLASH_COLOR to be visible
    FINE_POS_LEFT RESM0, NULL, 0, 4
    MULTI_COUNT_TWO_CMP
    BEQ .show_obstacle_1
    FINE_POS RESM1, OB_0_HPOS
    JMP .flipped_obstacles
.show_obstacle_1
    FINE_POS RESM1, OB_1_HPOS
.flipped_obstacles

    SWAP OB_0, OB_1
    SWAP OB_0_BRANCH, OB_1_BRANCH

    JMP game_vblank_end


; ----------------------------------
; GAME - VBLANK - APPROACH

game_vblank_approach SUBROUTINE game_vblank_approach
    MULTI_COUNT_THREE_CMP 1
    BNE .test_approach_completion

    ; move bird towards play position if it's not there already
    LDA BIRD_HPOS
    CMP #BIRD_HPOS_PLAY_POS
    BEQ .hpos_done
    CLC
    ADC #1
    STA BIRD_HPOS

    ; note that because game_vblank_approach falls through game_vblank_main_triage
    ; we'll also be performing the usual bird sprite update in addition to the
    ; above horizontal movement
.hpos_done

.test_approach_completion
    ; end approach state if obstacle 0 has reached middle of the screen
    LDA OB_0_HPOS
    CMP #78
    BNE .done_completion_test

    ; note there is an assumption that the bird has reached BIRD_HPOS_PLAY_POS
    ; before obstacle 0 reaches the middle of the screen

    ; put into main play state
    ;        o start obstacle 1 moving
    LDA #PLAY_STATE_PLAY
    STA PLAY_STATE
    LDA #1
    STA OB_1_SPEED
.done_completion_test

    ; intentionally fall through to game_vblank_main_triage

; ----------------------------------
; GAME - VBLANK - MAIN (TRIAGE)

game_vblank_main_triage SUBROUTINE game_vblank_main_triage
    MULTI_COUNT_THREE_CMP 1
    BEQ .far_jmp_sprite
    BPL game_vblank_foliage
    BMI game_vblank_collisions

.far_jmp_sprite
    JMP game_vblank_sprite
	

; -------------
; GAME - VBLANK - MAIN - FOLIAGE
game_vblank_foliage
    FOLIAGE_ANIMATION TRUE
    JMP game_vblank_position_sprites

; -------------
; GAME - VBLANK - MAIN - COLLISIONS / OBSTACLE RESET
game_vblank_collisions SUBROUTINE game_vblank_collisions
    ; skip the collisions frame if we're not in PLAY_STATE_PLAY
    LDA PLAY_STATE
    BNE .done_vblank_collisions

    ; check for bird collision
    BIT CXM0P
    BMI .bird_collision
    BVS .bird_collision
    BIT CXM1P
    BMI .bird_collision
    BVS .bird_collision

    ; reset obstacle gap / branch - comparing against three and less
    ; because we only make this check once every three frames and
    ; we don't want to miss the event
    LDA #03
    CMP OB_0_HPOS
    BCS .reset_obstacle_0
    CMP OB_1_HPOS
    BCS .reset_obstacle_1
    JMP game_vblank_position_sprites

.reset_obstacle_0
    ; NOTE: no branch ornamentation for obstacle 0
    NEW_OBSTACLE 0
    JMP game_vblank_position_sprites

.reset_obstacle_1
    NEW_OBSTACLE 1
    JMP game_vblank_position_sprites

.bird_collision
    ; put into collision play state
    ;        o reset flight pattern
    ;        o start collision animation with wings up sprite
    ;        o note obstacle movement will effectively be stopped now because it will
    ;            be stopped at the end of the vblank (as usual) but won't be restarted 
    ;            in the game_vblank_death subroutine
    RESET_FLIGHT_PATTERN TRUE
    LDA #<WINGS_UP
    STA ADDRESS_SPRITE_0
    LDA #PLAY_STATE_COLLISION
    STA PLAY_STATE
    SFX_LOAD SFX_COLLISION

.done_vblank_collisions
    JMP game_vblank_position_sprites


; -------------
; GAME - VBLANK - MAIN - UPDATE BIRD SPRITE

game_vblank_sprite SUBROUTINE game_vblank_sprite
    NEW_TRIGGER_CHECK 0
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

    ; new flap sound effect
    SFX_LOAD SFX_FLAP

    ; start new fly animation
    LDX #1
    STX PATTERN_INDEX

    ; flip sprite in response to trigger press
    LDX ADDRESS_SPRITE_0
    CPX #<WINGS_DOWN
    BEQ .flip_sprite_use_flat

    LDX #<WINGS_DOWN
    STX ADDRESS_SPRITE_0
    JMP .flip_sprite_end

.flip_sprite_use_flat
    LDX #<WINGS_FLAT
    STX ADDRESS_SPRITE_0
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
    STX ADDRESS_SPRITE_0
    JMP .sprite_set
.use_glide_sprite
    LDX #<WINGS_FLAT
    STX ADDRESS_SPRITE_0
.sprite_set

    ; check for ground collision
    CMP #BIRD_LOW
    BCC    .begin_drowning

    ; check to see if we've reached top of flying area (ie. top of screen)
    CMP #BIRD_HIGH
    BCS .limit_height
    JMP .update_pattern_idx

.begin_drowning
    DROWNING_PLAY_STATE
    JMP .fly_end

.limit_height
    LDA #BIRD_HIGH

.update_pattern_idx
    STA BIRD_VPOS 
    UPDATE_FLIGHT_PATTERN FALSE

.fly_end
    ; fall through


; ----------------------------------
; GAME - VBLANK - POSITION SPRITES / SCORING

game_vblank_position_sprites SUBROUTINE game_vblank_position_sprites
    POSITION_BIRD_SPRITE

    ; progress and then position osbtacles
    FINE_MOVE_LEFT OB_0_HPOS, OB_0_SPEED, TRUE
    FINE_MOVE_LEFT OB_1_HPOS, OB_1_SPEED, TRUE
    FINE_POS RESM0, OB_0_HPOS
    NOP
    FINE_POS RESM1, OB_1_HPOS

.scoring_check
    LDA PLAY_STATE
    BNE .end_scoring
    LDA BIRD_HPOS
    CMP OB_0_HPOS
    BEQ .score_obstacle
    CMP OB_1_HPOS
    BEQ .score_obstacle
    JMP game_vblank_end

.score_obstacle
    ; increase score -- using decimal addition
    SED
    LDA SCORE
    CLC
    ADC #$1
    STA SCORE
    CLD
    JMP game_vblank_end

.end_scoring


; ----------------------------------
; GAME - VBLANK - END (PREPARE DISPLAY KERNEL)

game_vblank_end SUBROUTINE game_vblank_end
    ; setup display kernel (for foliage area)

    ; added for exit function of Multicarts like PlusCart, UnoCart or Harmony.
    ; reading the missing SWCHA or SWCHB (whatever is not read by the game),
    ; once per frame is enough so the Multicart can peek the missing value(s).
    LDA SWCHA

    ; do horizontal movement
    ; note that movement registers need to be reset every frame
    STA WSYNC
    STA HMOVE

    ; reset collision flags every frame
    STA CXCLR

    ; obstacles already turned off - will be turned on again halfway through foliage subroutine

    ; playfield priority - foliage in front of obstacles
    LDA #CTRLPF_FOLIAGE
    STA CTRLPF

    ; foliage colours
    LDA #FOLIAGE_BACKGROUND
    STA    COLUBK
    LDA #FOLIAGE_COLOR
    STA COLUPF

    ; prepare index registers
    ; Y register will count the number of lines in the foliage area of the display
    ; X register keeps pointer to next foliage data to stuff into playfield
    LDY    #SCANLINES_FOLIAGE
    LDX FOLIAGE_SEED

    ; reset all movmement registers - we'll be triggering HMOVE every scanline and we
    ; don't want objects flying all over the screen.
    ; note 24 machine cycles required between HMOVE and HMCLR 
    STA HMCLR

    ; wait for end of vblank kernel 
    VBLANK_KERNEL_END
    STA HMOVE


; ----------------------------------
; GAME - DISPLAY KERNEL - FOLIAGE

foliage SUBROUTINE foliage

    ; A = SCANLINES_PER_FOLIAGE (starts at zero) (clobbered temporarily every SCANLINES_PER_FOLIAGE scanlines)
    ; Y register contains the number of SCANLINES_FOLIAGE remaining
    ; X register contains the FOLIAGE_SEED value

    ; laboured description of FOLIAGE and MAX_FOLIAGE_SEED
    ; ===================================================
    ;
    ; we change the playfield every SCANLINES_PER_FOLIAGE scanlines 
    ; we increase X (FOLIAGE_SEED) after every time we access it (twice per line)
    ; there are SCANLINES_FOLIAGE scanlines in the foliage are of of the display
    ; therefore the final value of X at the end of the foliage subroutine is:
    ;
    ;            max_foliage_index = FOLIAGE_SEED + (SCANLINES_FOLIAGE / SCANLINES_PER_FOLIAGE * 2) - 1
    ;
    ; this would mean that the same foliage is drawn every frame. to introduce some randomness,
    ; FOLIAGE_SEED is increased by one every THREE_CYCLE frames (in the vblank kernel).
    ; the maximum vaulue of FOLIAGE_SEED at the start of the foliage subroutine is therefore:
    ;
    ;            MAX_FOLIAGE_SEED = sizeof(FOLIAGE memory space) - max_foliage_index

.next_foliage
    CMP #$0                          ; 2
    BNE .cont_foliage                ; 2/3

    ; stuff new values into playfield
    ; we're going to clobber the accumulator but that's okay, we're
    ; going to reset it after setting the playfield

    LDA FOLIAGE,X                    ; 4
    STA PF0                          ; 3
    STA PF2                          ; 3
    INX                              ; 2
    LDA FOLIAGE,X                    ; 4

    ; end of hblank

    STA PF1                          ; 3
    INX                              ; 2
    INX                              ; 2

    ; start drawing obstacles if we're halfway through the foliage area
    CPY #SCANLINES_FOLIAGE / 2       ; 2
    BCS .new_foliage

.set_trunk
    LDA #$2                          ; 2
    IF RANDOM_TRUNK == TRUE
        AND $00                      ; 2
    ENDIF
    STA ENAM0                        ; 3
    STA ENAM1                        ; 3

.new_foliage
    ; re-initialise accumulator
    LDA #SCANLINES_PER_FOLIAGE       ; 2

.cont_foliage
    ; decrease SCANLINE_FOLIAGE count
    SEC                              ; 2
    SBC #$1                          ; 2

    DEY                              ; 2
    BEQ game_play_area_prepare       ; 2/3

    STA WSYNC                        ; 3
    STA HMOVE                        ; 3
    JMP .next_foliage                ; 3


; ----------------------------------
; GAME - DISPLAY KERNEL - PLAY AREA

game_play_area_prepare SUBROUTINE game_play_area_prepare
.PF0 = _localA
.PF1 = _localB
.PF2 = _localC

    ; playfield priority - background trees behind obstacles
    LDA #CTRLPF_PLAYAREA
    STA CTRLPF

    ; we want to stuff the playfield with new data as quickly as possible
    ; prepare playfield data according to which frame (odd/even)
    MULTI_COUNT_TWO_CMP
    BEQ .precalc_forest_static
    LDA FOREST_MID_2
    STA .PF2
    LDA FOREST_MID_1
    STA .PF1
    LDA FOREST_MID_0
    STA .PF0
    JMP .end_forest_precalc
.precalc_forest_static
    LDA FOREST_STATIC_2
    ORA FOREST_MID_2
    STA .PF2
    LDA FOREST_STATIC_1
    ORA FOREST_MID_1
    STA .PF1
    LDA FOREST_STATIC_0
    ORA FOREST_MID_0
    STA .PF0
.end_forest_precalc

    STA WSYNC
    STA HMOVE

    ; set forest for entire play area
    LDA #FOREST_BACKGROUND
    STA COLUBK
    LDA #FOREST_COLOR
    STA COLUPF
    LDA .PF0
    STA PF0
    LDA .PF1
    STA PF1
    LDA .PF2
    STA PF2

    ; prepare for loop

    LDY #SCANLINES_PLAYAREA
    LDX #SPRITE_LINES

game_play_area SUBROUTINE game_play_area
.YSTATE           = _localA
.MISSILE_0_SET    = _localB
.MISSILE_1_SET    = _localC
.MISSILE_1_NUSIZ  = _localD
.PLAYER_0_SPRITE  = _localE
.PLAYER_1_SPRITE  = _localF

    ; (re)initialise _local variables
    LDA #$00
    STA .PLAYER_0_SPRITE
    STA .PLAYER_1_SPRITE
    LDA #$02
    STA .MISSILE_0_SET
    STA .MISSILE_1_SET
    LDA #(OBSTACLE_WIDTH | HEAD_WIDTH)
    STA .MISSILE_1_NUSIZ

    ; loop alternates between .set_player_sprites and .set_missile_sprites starting with .set_player_sprites
    ; Y register contains the number of SCANLINES_PLAYAREA remaining
    ; X register contains number of SPRITE_LINES remaining

    ; EVEN NUMBERED SCANLINES
.set_missile_sprites
    STA WSYNC                       ; 3
    STA HMOVE                       ; 3

    LDA .MISSILE_1_NUSIZ            ; 3
    STA NUSIZ1                      ; 3

    LDA .MISSILE_0_SET              ; 3
    STA ENAM0                       ; 3
    LDA .MISSILE_1_SET              ; 3
    STA ENAM1                       ; 3

    ; maximum 22 cycles in HBLANK
    ;        21 cycles used
    ;        1 cycles until end of HBLANK

    ; precalculate missile state (on/off) before next .set_missile_sprites cycle
    ; precalculate branch placement in time for next .set_missile_sprites cycle
.precalc_missile_size
    LDA #(BRANCH_WIDTH | HEAD_WIDTH)                  ; 3
    CPY OB_1_BRANCH                                   ; 3
    BEQ .done_precalc_missile_size                    ; 2/3
    LDA #(OBSTACLE_WIDTH | HEAD_WIDTH)                ; 3
.done_precalc_missile_size
    STA .MISSILE_1_NUSIZ                              ; 3

.precalc_missile_sprites
    ; the AND $00 instruction introduces some randomness to whether the missile
    ; should be on during the upcoming scanline
    LDA (OB_0),Y                                      ; 5
    IF RANDOM_TRUNK == TRUE
        AND $00                                       ; 2
    ENDIF
    STA .MISSILE_0_SET                                ; 3
    LDA (OB_1),Y                                      ; 5
    IF RANDOM_TRUNK == TRUE
        AND $00                                       ; 2
    ENDIF
    STA .MISSILE_1_SET                                ; 3

    JMP .next_scanline                                ; 3

    ; maximum 76 cycles between WSYNC
    ; longest path
    ;        58 cycles
    ; + 11 for ".next_scanline"
    ; + 3 for WSYNC
    ; = 72
    ; 4 cycles remaining

    ; ODD NUMBERED SCANLINES
.set_player_sprites
    STA WSYNC                                   ; 3
    STA HMOVE                                   ; 3

    LDA .PLAYER_0_SPRITE                        ; 3
    STA GRP0                                    ; 3

    LDA .PLAYER_1_SPRITE                        ; 3
    STA GRP1                                    ; 3

    ; maximum 22 cycles in HBLANK
    ;        15 cycles used
    ;        7 cycles until end of HBLANK

.precalc_players_sprites
    ; o if scanline (Y) is equal or less than BIRD_VPOS
    ; o and if X is not negative
    ;    o then stuff .PLAYER_0_SPRITE and .PLAYER_1_SPRITE with next line of sprite data
    CPY BIRD_VPOS                               ; 3
    BCS .done_precalc_players                   ; 2/3
    TXA                                         ; 2
    BMI .done_precalc_players                   ; 2/3
    STY .YSTATE                                 ; 3
    TAY                                         ; 2
    LDA (ADDRESS_SPRITE_0),Y                    ; 5
    STA .PLAYER_0_SPRITE                        ; 3
    LDA (ADDRESS_SPRITE_1),Y                    ; 5
    STA .PLAYER_1_SPRITE                        ; 3
    DEX                                         ; 2
    LDY .YSTATE                                 ; 3
.done_precalc_players

    ; change color of player/missile 0 for last few lines
    ; o we use this to animate a water splash
    CPY #SPLASH_LINE                            ; 2
    BNE .next_scanline                          ; 2/3
    LDA SPLASH_COLOR                            ; 3
    STA COLUP0                                  ; 3

    ; maximum 76 cycles between WSYNC
    ;
    ; scanline above bird position
    ;        26 cycles
    ;
    ; scanline below bird position
    ;        30 cycles    - above or below virtual swamp line
    ;        34 cycles    - at virtual swamp line
    ; 
    ; scanline at bird position
    ;        55 cycles - above or below virtual swamp line
    ;        59 cycles - at virtual swamp line
    ;
    ; longest path
    ;   59 cycles
    ; + 13 for ".next_scanline"
    ; + 3 for WSYNC
    ; = 75
    ; 1 cycle remaining

.next_scanline
    ; decrement current scanline and jump to end of subroutine if we've reached zero
    DEY                                   ; 2
    BEQ .end_game_play_area               ; 2/3

    ; alternate setting of player and missile sprites
    TYA                                   ; 2
    AND #%00000001                        ; 2
    BEQ .set_missile_sprites              ; 2/3
    JMP .set_player_sprites               ; 3

.end_game_play_area


; ----------------------------------
; GAME - DISPLAY KERNEL - SWAMP

swamp SUBROUTINE swamp
    ; we've arrived here via the "BEQ swamp" call above in the .next_scanline of the game_play_area routine above.
    ; the last loop in the game_play_area loop before the successful branching would have been the .set_player_sprites
    ; loop.

    ; the scanline at this point is, by defintion, below the virtual swamp line (if SPLASH_LINE > 0)
    ; therefore, if the bird is being drawn at the bottom of the play area the longest path is as follows:
    ;
    ; 55 cycles
    ; + 5 for ".next_scanline" (fewer than normal because of the succesful "BEQ swamp")
    ; + 3 for WSYNC
    ; = 63
    ; 13 cycles remaining
    ;
    ; if the bird is above the scanline position then:
    ;
    ; 30 cycles
    ; + 5 for ".next_scanline" (fewer than normal because of the succesful "BEQ swamp")
    ; + 3 for WSYNC
    ; = 38
    ; 38 cycles remaining

.draw_swamp
    ; preload registers so that we're not wasting cycles in the HBLANK
    LDY FOLIAGE_SEED                 ; 3
    LDX FOLIAGE,Y                    ; 4
    LDA #0                           ; 2
    LDY #SWAMP_BACKGROUND            ; 3

    ; 1 cycle remaining (worst case)

    STA WSYNC
    STA HMOVE

    ; disable obstacles - we don't want the "trees" to extend into the forest swamp
    ; (A register preloaded)
    STA ENAM0
    STA ENAM1

    ; define the forest swamp playfield once per frame
    ; (Y register preloaded)
    STY COLUBK

    ; change colour of playfield to simulate movement
    ; we'll change background colour in the next HBLANK
    LDA #SWAMP_COLOR
    STA COLUPF

    ; draw swap the same as the last line of the foliage
    ; (X register preloaded)
    STX PF0
    STX PF1
    STX PF2

    STA WSYNC
    STA HMOVE

    ; turn off sprites - delaying a scanline
    LDA #$00
    STA GRP0
    STA GRP1

    ; wait another line before display - scoring
    STA WSYNC
    STA HMOVE


; ----------------------------------
; GAME - DISPLAY KERNEL - DISPLAY SCORE
display_score SUBROUTINE display_score
.DIGIT_TENS = _localA
.DIGIT_UNITS = _localC

    ; prepare data segment for digit addressing
    LDA #>DATA_SEGMENT
    STA .DIGIT_TENS+1
    STA .DIGIT_UNITS+1

    STA WSYNC
    LDA #0
    STA PF0
    STA PF0
    STA PF1
    STA PF2
    LDA #SCORING_BACKGROUND
    STA COLUBK

    LDA #SCORE_DIGITS_SIZE
    STA NUSIZ0
    STA NUSIZ1

    MULTI_COUNT_TWO_CMP
    BEQ .prep_hiscore

.prep_score
    LDA #SCORE_COLOR
    STA COLUP0
    STA COLUP1

    SIMPLE_POS_RIGHT RESP0, 8
    SIMPLE_POS_RIGHT RESP1, 4

    ; get address of unit digit
    LDA SCORE
    AND #$0F
    TAY
    LDA DIGIT_TABLE,Y
    STA .DIGIT_UNITS

    ; get address of tens digit
    LDA SCORE
    JMP .tens_digits

.prep_hiscore
    LDA #HISCORE_COLOR
    STA COLUP0
    STA COLUP1

    SIMPLE_POS_RIGHT RESP0, 19
    SIMPLE_POS_RIGHT RESP1, 15

    ; get address of unit digit
    LDA HISCORE
    AND #$0F
    TAY
    LDA DIGIT_TABLE,Y
    STA .DIGIT_UNITS

    ; get address of tens digit
    LDA HISCORE

.tens_digits
    ; tail end of both prep_score and prep_hiscore branches
    ROR
    ROR
    ROR
    ROR
    AND #$0F
    TAY
    LDA DIGIT_TABLE,Y
    STA .DIGIT_TENS

    LDY #DIGIT_LINES

    ; Y register contains number of DIGIT_LINES remaining
    ; NOTE: we need to use Y register because we'll be performing a post-indexed indirect address 
    ; to set the sprite line

.scoring_loop
    STA WSYNC
    LDA (.DIGIT_UNITS),Y
    STA GRP1                                        ; 3
    LDA (.DIGIT_TENS),Y
    STA GRP0                                        ; 3

.next_scanline
    DEY                                             ; 2
    BMI game_overscan                               ; 2/3
    JMP .scoring_loop                               ; 3


; ----------------------------------
; GAME - OVERSCAN

game_overscan SUBROUTINE game_overscan
    OVERSCAN_KERNEL_SETUP

    NEW_SWITCH_CHECK SELECT_SWITCH
    BNE .done_head_check

    LDY SELECTED_HEAD
    INY
    CPY #NUM_HEADS
    BNE .swap_heads
    LDY #0
.swap_heads
    STY SELECTED_HEAD
    LDA HEADS_TABLE,Y
    STA ADDRESS_SPRITE_1

.done_head_check
    STORE_SWITCH_STATE

    ; reset player sprites, color and NUSIZ after scoring subroutine
    ; o color may change again in the ready state for the "OK?" text
    ; o NUSIZ may change in drowning play state
    LDA #0
    STA GRP0
    STA GRP1
    LDA #BIRD_COLOR
    STA COLUP0
    STA COLUP1

    LDA PLAY_STATE
    CMP #PLAY_STATE_READY
    BEQ .set_width_for_ready
    LDA #(OBSTACLE_WIDTH | WING_WIDTH)
    STA NUSIZ0
    LDA #(OBSTACLE_WIDTH | HEAD_WIDTH)
    STA NUSIZ1
    JMP .done_set_width
.set_width_for_ready
    LDA #(OBSTACLE_WIDTH | OK_SIGN_WIDTH)
    STA NUSIZ0
    LDA #(OBSTACLE_WIDTH | QMARK_SIGN_WIDTH)
    STA NUSIZ1
.done_set_width

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

    ; special case for when we've /just/ entered PLAY_STATE_DROWN
    LDA PLAY_STATE
    CMP #PLAY_STATE_DROWN
    BNE .done_drowning_compensation
    LDA PATTERN_INDEX
    CMP #DEATH_DROWNING_LEN
    BNE .done_drowning_compensation

    ; decrease PATTERN_INDEX straight away otherwise we'll reach this point
    ; again next frame, because PATTERN_INDEX is only updated every three frames
    DEC PATTERN_INDEX

    ; phase correct the obstacle flicker if we're currently on the "odd" frame
    MULTI_COUNT_TWO_CMP
    BEQ .done_drowning_compensation
    SWAP OB_0, OB_1
    SWAP OB_0_BRANCH, OB_1_BRANCH
.done_drowning_compensation

    SFX_ENGINE

    MULTI_COUNT_UPDATE

    OVERSCAN_KERNEL_END

    JMP game_vsync


  IF PLUSROM
PlusROM_API:
    .byte "a", 0, "h.firmaplus.de", 0

SendPlusROMScore:
   lda SCORE
   sta WriteToBuffer
   lda #HIGHSCORE_ID          	    ; game id in Highscore DB
   sta WriteSendBuffer              ; send request to backend..
   rts

    ; keep PlusROM hotspots clear!
    ORG $FFF0
    .byte $FF       ; WriteToBuffer
    .byte $FF       ; WriteSendBuffer
    .byte $FF       ; ReceiveBuffer
    .byte $FF       ; ReceiveBufferSize
  ENDIF


; ----------------------------------
; * MACHINE INITIALISATION

initialisation SUBROUTINE initialisation

    ORG $FFFA
  IF PLUSROM
    .word ((PlusROM_API & $0FFF) + $1000)
  ELSE
    .word setup        ; NMI
  ENDIF
    .word setup        ; RESET
    .word setup        ; IRQ

