; boot.asm
; Author: Caleb Leak
; Created: 10/18/2014
; Compile with "dasm boot.asm -oboot.bin -f3"

    PROCESSOR 6502    
    INCLUDE "vcs.h"
    ORG $F000

; RAM locations
PLAYER_Y_POS_LO = $80
PLAYER_Y_POS_HI = $81

PLAYER_Y_SPD_LO = $82
PLAYER_Y_SPD_HI = $83

PLAYER_ROT_LO     = $84
PLAYER_ROT_HI     = $85

; Pipe 0 stuff
PIPE_0_MILESTONE    = $86
PIPE_0_SEG_INDEX    = $87
PIPE_0_SEG_1        = $88
PIPE_0_SEG_2        = $89
PIPE_0_SEG_3        = $8A
PIPE_0_SEG_4        = $8B
PIPE_0_SEG_5        = $8C

; Pipe 1 stuff
PIPE_1_MILESTONE    = $8D
PIPE_1_SEG_INDEX    = $8E
PIPE_1_SEG_1        = $8F
PIPE_1_SEG_2        = $90
PIPE_1_SEG_3        = $91
PIPE_1_SEG_4        = $92
PIPE_1_SEG_5        = $93

TEMP_STORE            = $94

; LSB of screen shift accumulator
SCROLL_ACCUM        = $95

; Offset of the screen horizontal scroll
SCROLL_POS            = $96

PIPE_0_RESET_TIMER    = $97
PIPE_1_RESET_TIMER    = $98

RAND_HOLDER            = $99

BIRD_SPRITE_COUNTER = $9A

; Last input state, for detecting button edges
LAST_INPUT            = $9B
JUMP_PRESSED        = $9C ; Set to 1 for a newly pressed jump

; States of the game 0=Start, 1=Running, 2=Game Over
GAME_STATE            = $9D

PIPE_0_MASK_1        = $9E
PIPE_0_MASK_2        = $9F
PIPE_0_MASK_3        = $A0
PIPE_0_MASK_4        = $A1
PIPE_0_MASK_5        = $A2

PIPE_1_MASK_1        = $A3
PIPE_1_MASK_2        = $A4
PIPE_1_MASK_3        = $A5
PIPE_1_MASK_4        = $A6
PIPE_1_MASK_5        = $A7

LINE_COUNTER        = $A8

SCORE_DIGIT_1        = $A9
SCORE_DIGIT_2        = $AA
SCORE_DIGIT_3        = $AB

RAND_100            = $AC

SOUND_0_TIMER        = $AE
SOUND_0_DIVIDER        = $AF
SOUND_1_TIMER        = $B0
SOUND_1_DIVIDER        = $B1

; Indicates if enough time has passed for the game to be reset from the game over screen
RESET_READY            = $B2

SCORE_COLOR            = $B3
BOOT_COLOR            = $B4

FRAME_COUNTER_LO    = $B5
FRAME_COUNTER_HI    = $B6
LAST_INPUT_FIRE        = $B7

Main:
    cld ; Disable decimal mode
    lda #$40
    sta PLAYER_Y_POS_LO
    sta PLAYER_Y_POS_HI

    ldx #$80
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta ENABL       ; Turn off ball, missiles and players
    sta ENAM0
    sta ENAM1
    sta GRP0
    sta GRP1
    sta VDELBL

    sta GAME_STATE
    sta LAST_INPUT
    sta PIPE_0_MASK_3
    sta PIPE_1_MASK_3

    sta SCORE_DIGIT_1
    sta SCORE_DIGIT_2
    sta SCORE_DIGIT_3
    sta SOUND_0_DIVIDER
    sta SOUND_0_TIMER
    sta SOUND_1_DIVIDER
    sta SOUND_1_TIMER

    sta RESP0
    lda #$00
    sta WSYNC
    sta RESP0

    lda #$80
    sta HMP0
    sta WSYNC
    sta HMOVE

    ; Player 0 color
    lda #$ff
    sta COLUP0

    ; Player 1 color (Pipe)
    lda #$C4
    sta COLUP1

    ; Ball color (player)
    lda #$F0
    sta COLUPF

    lda #$00
    sta PLAYER_Y_POS_LO
    sta PLAYER_Y_POS_HI
    
    sta PLAYER_Y_SPD_LO
    sta PLAYER_Y_SPD_HI

    sta PLAYER_ROT_LO
    sta PLAYER_ROT_HI

    lda #$05
    sta NUSIZ0
    sta NUSIZ1

    lda #0
    sta FRAME_COUNTER_LO
    sta FRAME_COUNTER_HI

ResetLevel:
    lda #111
    sta RAND_HOLDER

    lda #0
    sta SCROLL_ACCUM
    sta SCROLL_POS

    ; About to reset
    lda #255
    sta PIPE_0_RESET_TIMER

    ; Second pipe is halfway down the screen
    lda #168
    sta PIPE_1_RESET_TIMER

    lda #255
    sta PIPE_0_SEG_1
    sta PIPE_0_SEG_2
    sta PIPE_0_SEG_3
    sta PIPE_0_SEG_4
    sta PIPE_1_SEG_1
    sta PIPE_1_SEG_2
    sta PIPE_1_SEG_3
    sta PIPE_1_SEG_4

StartFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of 3 line vsync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    lda #$77
    sta COLUBK

    ; Vsync
    lda #%00000010
    sta VSYNC
    REPEAT 3
        sta WSYNC
    REPEND

    sta HMOVE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of 37 line vsync + vblank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Prepare vblank (line 1)
    lda #25
    sta LINE_COUNTER
    lda #0
    sta VSYNC
    lda #%00000010
    sta VBLANK
    sta WSYNC

    ; Setup ball position (line 2)
    REPEAT 17
        nop
    REPEND
    sta RESBL
    lda #$F0
    sta COLUPF
    sta WSYNC

    ; Setup initial pipe values (line 3)
    ; Prepare pipes
    lda #0
    sta    PIPE_0_SEG_INDEX
    sta    PIPE_1_SEG_INDEX

    lda PIPE_0_SEG_1
    sta PIPE_0_MILESTONE
    lda #$FF
    sta PIPE_0_SEG_5

    lda PIPE_1_SEG_1
    sta PIPE_1_MILESTONE
    lda #$FF
    sta PIPE_1_SEG_5
    sta WSYNC

    ; Reset line counter and prepare pipe masks (line 4)
    lda #%01111110
    sta GRP0
    sta GRP1
    sta HMCLR
    ldy #0
    lda #0
    sta WSYNC

    ; Reset bird line counter (line 5)
    lda PLAYER_Y_POS_HI
    clc
    lsr
    sta BIRD_SPRITE_COUNTER
    lda #0
    clc
    sbc BIRD_SPRITE_COUNTER
    clc
    sta BIRD_SPRITE_COUNTER
    sta WSYNC

    ; Prepare pipe 0 mask (line 6)
    lda #%01111110
    sta PIPE_0_MASK_1
    sta PIPE_0_MASK_5
    lda #%11111111
    sta PIPE_0_MASK_2
    sta PIPE_0_MASK_4
    sta WSYNC

    ; Prepare pipe 1 mask (line 7+)    
    lda #$ff
    ldy PIPE_0_RESET_TIMER
PreparePipe0MasksLeft:
    cpy #244
    bcc PreparePipe0MasksRight
    clc
    dey 
    dey 
    lsr
    dec LINE_COUNTER
    sta WSYNC
    jmp PreparePipe0MasksLeft

PreparePipe0MasksRight:
    cpy #99
    bcs SetPipe0Masks
    clc
    iny 
    iny 
    asl
    beq SetPipe0Masks
    dec LINE_COUNTER
    sta WSYNC
    jmp PreparePipe0MasksRight

SetPipe0Masks:
    sta PIPE_0_MASK_2
    sta PIPE_0_MASK_4
    and #%01111110
    sta PIPE_0_MASK_1
    sta PIPE_0_MASK_5
    sta GRP0
    sta WSYNC

    ; Prepare pipe 1 mask    
    lda #$ff
    ldy PIPE_1_RESET_TIMER
PreparePipe1MasksLeft:
    cpy #244
    bcc PreparePipe1MasksRight
    clc
    dey 
    dey 
    lsr
    dec LINE_COUNTER
    sta WSYNC
    jmp PreparePipe1MasksLeft

PreparePipe1MasksRight:
    cpy #99
    bcs SetPipe1Masks
    clc
    iny 
    iny 
    asl
    beq SetPipe1Masks
    dec LINE_COUNTER
    sta WSYNC
    jmp PreparePipe1MasksRight

SetPipe1Masks:
    sta PIPE_1_MASK_2
    sta PIPE_1_MASK_4
    and #%01111110
    sta PIPE_1_MASK_1
    sta PIPE_1_MASK_5
    sta GRP1
    lda #$C4
    sta COLUP0
    sta COLUP1
    sta WSYNC

    ; Special case for pipes that aren't enabled yet
    lda PIPE_0_SEG_1
    cmp #255
    bne CheckNoPipe1
    lda #0
    sta GRP0
    lda #2
    sta PIPE_0_SEG_INDEX
CheckNoPipe1:
    lda PIPE_1_SEG_1
    cmp #255
    bne FinishPipeSkips
    lda #0
    sta GRP1
    lda #2
    sta PIPE_1_SEG_INDEX
FinishPipeSkips:
    sta WSYNC

    ; Wsync for remaining lines
VblankFiller:
    ; Handle clearing of inputs
    lda SWCHA
    ora LAST_INPUT
    sta LAST_INPUT
    lda INPT4

    ora LAST_INPUT_FIRE
    sta LAST_INPUT_FIRE
    sta WSYNC

    dec LINE_COUNTER
    bne VblankFiller

    ; Clear vblank and vsync    
    ldy #0
    lda #%00000000
    sta VBLANK
    sta VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of visible 192 lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Draw:
    inc BIRD_SPRITE_COUNTER
    lda #10
    cmp BIRD_SPRITE_COUNTER
    bcc SkipBirdSprite
    ldx BIRD_SPRITE_COUNTER
    lda BirdSprite,X
    sta HMBL
    sta ENABL
    rol
    rol
    sta CTRLPF

SkipBirdSprite:
    ; Handle pipe 0 drawing
    cpy PIPE_1_MILESTONE
    bne SkipUpdatePipe0
    inc PIPE_1_SEG_INDEX
SkipUpdatePipe0:
    ldx PIPE_1_SEG_INDEX
    lda PIPE_1_SEG_1,X
    sta PIPE_1_MILESTONE
    lda PIPE_1_MASK_1,X

    iny
    sta WSYNC
    sta GRP1

    ; Handle clearing of inputs
    lda SWCHA
    ora LAST_INPUT
    sta LAST_INPUT
    lda INPT4
    ora LAST_INPUT_FIRE
    sta LAST_INPUT_FIRE

    ; Handle pipe 1 drawing
    cpy PIPE_0_MILESTONE
    bne SkipUpdatePipe1
    inc PIPE_0_SEG_INDEX
SkipUpdatePipe1:
    ldx PIPE_0_SEG_INDEX
    lda PIPE_0_SEG_1,X
    sta PIPE_0_MILESTONE
    lda PIPE_0_MASK_1,X

    ; End of visible line
    iny
    sta WSYNC
    sta GRP0
    cpy #168
    bne Draw

    lda #$CA
    sta COLUBK
    sta COLUPF
    lda #0
    sta GRP0
    sta GRP1
    sta ENABL
    sta WSYNC
    
    lda #0
    sta CTRLPF
    lda #%10101010
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC

    lda #$F4
    sta COLUBK
    sta WSYNC
    sta WSYNC
    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2

    lda #$F4
    sta COLUP0
    sta COLUPF
    lda SCORE_COLOR
    sta COLUP1
    lda #$03
    sta CTRLPF
    ldy #0
    sta WSYNC
DrawScore:
    tya
    clc
    adc SCORE_DIGIT_3
    tax
    lda Digit0,X
    lsr
    lsr
    lsr
    lsr
    sta PF2

    tya
    clc
    adc SCORE_DIGIT_2
    tax
    lda Digit0R,X
    sta PF1

    tya
    clc
    adc SCORE_DIGIT_1
    tax
    lda Digit0,X
    sta PF0
    iny
    sta WSYNC
    sta WSYNC
    sta WSYNC
    sta WSYNC
    cpy #5
    bne DrawScore

    lda #0
    sta PF2
    sta PF1
    sta PF0

    REPEAT 2
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of 30 line overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Ssetup vblank
    sta WSYNC

    ; Update joystick input state
    ldy #0
    lda SWCHA
    and #$10             ; Check if P0 is pressing up
    bne FinishInputCheck
    lda LAST_INPUT
    and #$10             ; Check if P0 up was previously not pressed
    beq FinishInputCheck
    ldy #1    
FinishInputCheck:
    sty JUMP_PRESSED
    lda SWCHA
    sta LAST_INPUT
    sta WSYNC

    ; Update fire button input state
    ldy JUMP_PRESSED
    lda INPT4
    and #$80             ; Check if P0 is pressing fire
    bne FinishFireInputCheck
    lda LAST_INPUT_FIRE
    and #$80             ; Check if P0 fire was previously not pressed
    beq FinishFireInputCheck
    ldy #1    
FinishFireInputCheck:
    sty JUMP_PRESSED
    lda INPT4
    sta LAST_INPUT_FIRE
    sta WSYNC

    ; Generate a random number to be used each frame
    lda RAND_HOLDER
    clc
    rol
    bit RandTap0
    bne SkipTap0
    eor #$01
SkipTap0:
    bit RandTap1
    bne SkipTap1
    eor #$01
SkipTap1:
    bit RandTap2
    bne SkipTap2
    eor #$01
SkipTap2:
    sta RAND_HOLDER
    ; Create a random number in 0 - 100
    and #$7F
    cmp #100
    bcc NoRandMod
    clc
    sbc #80
NoRandMod:
    sta RAND_100
    sta WSYNC

    ; Branch based on game state
    lda GAME_STATE
    cmp #0
    beq GameStart
    cmp #1
    beq GameRunning
    cmp #2
    beq GameOver

GameStart:

    lda #0
    sta SCORE_DIGIT_1
    sta SCORE_DIGIT_2
    sta SCORE_DIGIT_3
    sta RESET_READY

    lda #50
    sta PLAYER_Y_POS_HI

    lda #253
    sta PLAYER_Y_SPD_HI

    lda #$FF
    sta PIPE_0_SEG_1
    sta PIPE_0_SEG_3
    sta PIPE_1_SEG_1
    sta PIPE_1_SEG_3

    ; Ball color (player)
    lda #$F0
    sta COLUPF

    lda #$0E
    sta SCORE_COLOR

    sta WSYNC

    ; Check if the game should now start
    lda JUMP_PRESSED
    bne GenerateLevel
    lda #25
    jmp FinishOverscan

GenerateLevel:
    sta WSYNC
    lda #1
    sta GAME_STATE
    lda #96
    sta PIPE_0_RESET_TIMER
    lda #16
    sta PIPE_1_RESET_TIMER
    lda #24
    jmp FinishOverscan

GameOver:
    clc
    inc FRAME_COUNTER_LO
    bne SkipIncFrameHi
    lda FRAME_COUNTER_HI
    adc #1 ; Add carry from low byte
    sta FRAME_COUNTER_HI
SkipIncFrameHi:
    lda FRAME_COUNTER_LO

    ; Score color
    lsr
    lsr
    and #31
    tax
    lda CosTable,X
    ora #$B0
    sta SCORE_COLOR

    lda FRAME_COUNTER_LO

    ; Check if enough time has passed for the game to be reset
    cmp #100
    bne FinishReadyResetCheck
    lda #$ff
    sta RESET_READY
FinishReadyResetCheck:
    sta WSYNC

    ; Check if the game is being reset
    lda JUMP_PRESSED
    and RESET_READY
    beq FinishReset
    lda #0
    sta GAME_STATE
FinishReset:
    sta WSYNC

    lda #25
    jmp FinishOverscan

GameRunning:
    ; Increment position w/ velocity
    clc
    lda PLAYER_Y_POS_LO        ; Velocity (low byte)
    adc PLAYER_Y_SPD_LO
    sta PLAYER_Y_POS_LO

    lda PLAYER_Y_POS_HI        ; Velocity (high byte)
    adc PLAYER_Y_SPD_HI
    sta PLAYER_Y_POS_HI

    sta WSYNC

    ; Increment position w/ acceleration
    ; TODO: This could probably be baked into the initial velocity value
    clc
    lda PLAYER_Y_POS_LO
    adc #20 ; 1/2 acceleration
    sta PLAYER_Y_POS_LO

    lda PLAYER_Y_POS_HI
    adc #0
    cmp #240
    bcc DonePlayerPosClamp
    lda #0
DonePlayerPosClamp:
    sta PLAYER_Y_POS_HI

    sta WSYNC

    ; Increment velocity w/ acceleration
    clc
    lda PLAYER_Y_SPD_LO
    adc #40 ; Full acceleration
    sta PLAYER_Y_SPD_LO

    lda PLAYER_Y_SPD_HI
    adc #0
    sta PLAYER_Y_SPD_HI
    sta WSYNC

    ; Handle jumping
    lda JUMP_PRESSED
    beq FinishJump

    ; Set player velocity to -2 for a jump
    lda #0
    sbc #2
    sta PLAYER_Y_SPD_HI
    lda #$AF
    sta PLAYER_Y_SPD_LO

    ; Jump sound
    ldy #$FB
    sty SOUND_1_TIMER
    ldy #1
    sty SOUND_1_DIVIDER
    ldy #6
    sty AUDC1

FinishJump:
    sta WSYNC

    ; Check if it's time to generate a pipe
    ; Make sure there's no other movement
    lda #0
    sta HMCLR

    ; Determine how much to scroll the screen
    clc
    lda SCROLL_ACCUM
    adc #0
    sta SCROLL_ACCUM
    lda #1
    adc #0 ; Add the carry
    sta SCROLL_POS ; Save the scroll value for later
    asl ; Get the scroll value in the upper 4 bits
    asl
    asl
    asl
    sta HMP0
    sta HMP1
    clc
    sta WSYNC

    ; Reset pipe 0 if it's time
    lda PIPE_1_RESET_TIMER
    adc SCROLL_POS
    sta WSYNC

    bcc NoResetPipe1
    sta RESP1
    lda RAND_100
    and #%01111110
    clc
    adc #4
    sta PIPE_1_SEG_1
    adc #06
    sta PIPE_1_SEG_2
    ;;;;;adc #30
    adc #38
    sta PIPE_1_SEG_3
    adc #06
    sta PIPE_1_SEG_4
    lda #80
NoResetPipe1:
    sta PIPE_1_RESET_TIMER 
    clc
    sta WSYNC

    ; Reset pipe 1 if it's time
    lda PIPE_0_RESET_TIMER
    adc SCROLL_POS
    sta WSYNC

    bcc NoResetPipe0
    sta RESP0
    lda RAND_100
    and #%01111110
    clc
    adc #5 ; Offset by 1 so milestones between pipes 0 and 1 are never on the same line
    sta PIPE_0_SEG_1
    adc #06
    sta PIPE_0_SEG_2
    adc #38
    sta PIPE_0_SEG_3
    adc #06
    sta PIPE_0_SEG_4
    lda #80
NoResetPipe0:
    sta PIPE_0_RESET_TIMER 
    sta WSYNC

    ; Do scoring
    lda #0
    ldx PIPE_0_SEG_1
    cpx #255
    beq NoPipe0Score ; Special case, pipe isn't enabled
    ldx PIPE_0_RESET_TIMER
    cpx #212
    bne NoPipe0Score
    lda #8
NoPipe0Score:
    ldx PIPE_1_SEG_1
    cpx #255
    beq NoPipe1Score ; Special case, pipe isn't enabled
    ldx PIPE_1_RESET_TIMER
    cpx #212
    bne NoPipe1Score
    lda #8
NoPipe1Score:
    cmp #8

    ; Play score sound if appropriate
    bne AddToScore
    ldy #$F0
    sty SOUND_0_TIMER
    ldy #3
    sty SOUND_0_DIVIDER
    ldy #13
    sty AUDC0

AddToScore:
    clc
    adc SCORE_DIGIT_1
    sta SCORE_DIGIT_1
    sta WSYNC

    ; Handle digit overflow
    lda SCORE_DIGIT_1
    cmp #80
    bne NoScoreCarry
    lda #0
    sta SCORE_DIGIT_1
    lda #8
    clc
    adc SCORE_DIGIT_2
    sta SCORE_DIGIT_2
    cmp #80
    bne NoScoreCarry
    lda #0
    sta SCORE_DIGIT_2
    lda #8
    clc
    adc SCORE_DIGIT_3
    sta SCORE_DIGIT_3
    sta WSYNC 
NoScoreCarry:

    ; Handle clearing of inputs
    lda SWCHA
    ora LAST_INPUT
    sta LAST_INPUT
    lda INPT4
    ora LAST_INPUT_FIRE
    sta LAST_INPUT_FIRE
    sta WSYNC

    ; Check for player death
    lda PLAYER_Y_POS_HI
    cmp #157
    bcs DoPlayerDeath
    lda CXP0FB
    ora CXP1FB
    and #%01000000
    beq FinishDeathCheck
DoPlayerDeath:
    lda #2
    sta GAME_STATE
    lda #0
    sta SOUND_0_TIMER
    sta FRAME_COUNTER_LO
    sta FRAME_COUNTER_HI
    lda #8
    sta AUDC1
    lda #$F0
    sta SOUND_1_TIMER
    lda #1
    sta SOUND_0_DIVIDER
    sta WSYNC

FinishDeathCheck:

    ; Rest of overscan
    lda #13

;;;;;;;;;;;;;;;;;
;; Common end of overscan
;;;;;;;;;;;;;;;;;
FinishOverscan:
; Process sound 0
    sta TEMP_STORE
    ldy #0 ; Volume
    ldx SOUND_0_DIVIDER
    lda SOUND_0_TIMER
    beq FinishSound0
    sbc #8
    eor #$FF
    ldy #$08
DivideSound0:
    lsr
    dex
    bne DivideSound0

    sta AUDF0
    inc SOUND_0_TIMER

FinishSound0:
    sty AUDV0
    lda TEMP_STORE
    clc
    sbc #1
    sta WSYNC

; Process sound 1
    sta TEMP_STORE
    ldy #0 ; Volume
    ldx SOUND_1_DIVIDER
    lda SOUND_1_TIMER
    beq FinishSound1
    eor #$FF
    ldy #$08
DivideSound1:
    lsr
    dex
    bne DivideSound1

    sta AUDF1
    inc SOUND_1_TIMER

FinishSound1:
    sty AUDV1
    lda TEMP_STORE
    clc
    sbc #1
    sta WSYNC

    sta CXCLR
OverscanLoop:
    ; Next frame
    sta WSYNC
    sbc #1
    bne OverscanLoop

    jmp StartFrame

    ORG $F800 
RandTap0:
    .BYTE %00000100

RandTap1:
    .BYTE %01000000

RandTap2:
    .BYTE %10000000

PipeMask:
    .BYTE %01111110
    .BYTE %11111111
    .BYTE %00000000
    .BYTE %11111111
    .BYTE %01111110

BirdSprite:
    .BYTE %00000110
    .BYTE %00010110
    .BYTE %00001010
    .BYTE %00001010
    .BYTE %00001110
    .BYTE %00001110
    REPEAT 10
        .BYTE %00000000
    REPEND

Digit0:
    .BYTE %01100000
    .BYTE %10010000
    .BYTE %10010000
    .BYTE %10010000
    .BYTE %01100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit1:
    .BYTE %00100000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit2:
    .BYTE %01100000
    .BYTE %10010000
    .BYTE %00100000
    .BYTE %01000000
    .BYTE %11110000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit3:
    .BYTE %11100000
    .BYTE %00010000
    .BYTE %11110000
    .BYTE %00010000
    .BYTE %11100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit4:
    .BYTE %10010000
    .BYTE %10010000
    .BYTE %11110000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit5:
    .BYTE %11110000
    .BYTE %10000000
    .BYTE %11100000
    .BYTE %00010000
    .BYTE %11100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit6:
    .BYTE %01110000
    .BYTE %10000000
    .BYTE %01100000
    .BYTE %10010000
    .BYTE %01100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit7:
    .BYTE %11110000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit8:
    .BYTE %01100000
    .BYTE %10010000
    .BYTE %01100000
    .BYTE %10010000
    .BYTE %01100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit9:
    .BYTE %01110000
    .BYTE %10010000
    .BYTE %01110000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit0R:
    .BYTE %00011000
    .BYTE %00100100
    .BYTE %00100100
    .BYTE %00100100
    .BYTE %00011000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit1R:
    .BYTE %00001000
    .BYTE %00001000
    .BYTE %00001000
    .BYTE %00001000
    .BYTE %00001000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit2R:
    .BYTE %00011000
    .BYTE %00100100
    .BYTE %00010000
    .BYTE %00001000
    .BYTE %00111100
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit3R:
    .BYTE %00011100
    .BYTE %00100000
    .BYTE %00111100
    .BYTE %00100000
    .BYTE %00011100
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit4R:
    .BYTE %00100100
    .BYTE %00100100
    .BYTE %00111100
    .BYTE %00100000
    .BYTE %00100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit5R:
    .BYTE %00111100
    .BYTE %00000100
    .BYTE %00011100
    .BYTE %00100000
    .BYTE %00111100
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit6R:
    .BYTE %00111000
    .BYTE %00000100
    .BYTE %00011000
    .BYTE %00100100
    .BYTE %00011000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit7R:
    .BYTE %00111100
    .BYTE %00100000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit8R:
    .BYTE %00011000
    .BYTE %00100100
    .BYTE %00011000
    .BYTE %00100100
    .BYTE %00011000
    .BYTE $00
    .BYTE $00
    .BYTE $00

Digit9R:
    .BYTE %00111000
    .BYTE %00100100
    .BYTE %00111000
    .BYTE %00100000
    .BYTE %00100000
    .BYTE $00
    .BYTE $00
    .BYTE $00

CosTable:
     .byte $0F
     .byte $0E
     .byte $0E
     .byte $0D
     .byte $0C
     .byte $0B
     .byte $0A
     .byte $08
     .byte $07
     .byte $06
     .byte $04
     .byte $03
     .byte $02
     .byte $01
     .byte $00
     .byte $00
     .byte $00
     .byte $00
     .byte $00
     .byte $01
     .byte $02
     .byte $03
     .byte $04
     .byte $06
     .byte $07
     .byte $08
     .byte $0A
     .byte $0B
     .byte $0C
     .byte $0D
     .byte $0E
     .byte $0E

    ORG $FFFA             ; Cart config (so 6502 can start it up)
 
    .WORD Main      ;     NMI
    .WORD Main      ;     RESET
    .WORD Main      ;     IRQ
 
    END
