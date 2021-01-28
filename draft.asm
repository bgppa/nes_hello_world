; A simple program to sum up part of the basic Nerdy Nights tutorial
; Start screen -> Move Mario until hit Goomba -> Game Over (repeat) 
  .inesprg 1
  .ineschr 1
  .inesmap 0
  .inesmir 1

  .rsset $0000
old_buttons   .rs 1 ; the previous read buttons 
new_buttons   .rs 1 ; new read input: compare to old to deduce newly pressed
GameStatus    .rs 1 ; 0 is "title screen", 1 is "game", 2 is "game over"
GameStartsNow .rs 1 ; Track if the previous state was the Title screen
YesCollision  .rs 1 ; If Mario hit Goomba

;;; --- Main loop 
  .bank 0
  .org $C000
RESET:
  sei
  cld
  ; Clear all the variables of interest
  lda #$00
  sta old_buttons
  sta new_buttons
  sta GameStatus
  sta GameStartsNow
  sta YesCollision
  jsr WaitForPPU    ; Wait to prepare the Graphic Unit
  jsr ClearScreen   ; Write $FF in all the Sprite CPU mem., to clear the sceen  
  LoadPalettes:     ; Load Palettes
  lda $2002         ; tell PPU to be ready
  lda #$3F
  sta $2006
  lda #$00
  sta $2006         
  ldx #$00          ; start the "palette loading mode"
LoadPalettesLoop:
  lda mypalette, x
  sta $2007
  inx
  cpx #$20
  bne LoadPalettesLoop

  lda #%10000000    ; Enable NMI
  sta $2000
  lda #%00010000  ; enable sprites
  sta $2001

Forever:          ; do nothing more: all is done in NMI
  jmp Forever


;; ---- SUBROUTINES
WaitForPPU:
  bit $2002         ; makes sure vblank flag is off: suggestion by tokumaru
tmp1:
  bit $2002
  bpl tmp1
  rts
  

; Read the current controller input and store into new_buttons
ReadController:
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
ReadControllerLoop:
  lda $4016
  lsr A
  rol new_buttons
  dex
  bne ReadControllerLoop
  rts


ClearScreen:
  ldx #$00
clear_loop:
  lda #$FF
  sta $0200, x
  inx 
  cpx #$00
  bne clear_loop
  rts


; Load the sprites for the title screen
LoadTitleSprites:
  ldx #$00
LoadTitleSpritesLoop:
  lda title_sprites, x
  sta $0200, x
  inx
  cpx #$54
  bne LoadTitleSpritesLoop
  rts


; Load the sprites for the game screen. Need to load only when the game start.
LoadGameSprites:
  ldx #$00
LoadGameSpritesLoop:
  lda game_sprites, x
  sta $0200, x
  inx
  cpx #$20
  bne LoadGameSpritesLoop
  rts


; Load the sprites for the Game Over screen.
LoadLostSprites:
  ldx #$00
LoadLostSpritesLoop:
  lda lost_sprites, x
  sta $0200, x
  inx
  cpx #$48
  bne LoadLostSpritesLoop
  rts


; Upgrade the Mario sprite: up/down/left/right and check collision with Goomba
UpgradeMario:
  jsr ReadController
  lda new_buttons
  and #%00001000
  cmp #%00001000
  bne UpDone
  jsr MoveMarioUp

UpDone:
  lda new_buttons
  and #%00000100
  cmp #%00000100
  bne DownDone
  jsr MoveMarioDown

DownDone:
  lda new_buttons
  and #%00000010
  cmp #%00000010
  bne LeftDone
  jsr MoveMarioLeft

LeftDone:
  lda new_buttons
  and #%00000001
  cmp #%00000001
  bne RightDone
  jsr MoveMarioRight

RightDone:
  jsr CheckCollision
  rts


;; Move mario UP
MoveMarioUp:
  ; decrease the Y byte of the first 4 sprites
  ldx #$00
up_loop:
  lda $0200,x
  sec
  sbc #$01
  sta $0200,x
  inx
  inx
  inx
  inx
  cpx #$10
  bne up_loop
  rts


MoveMarioDown:
  ; increase the Y byte of the first 4 sprites
  ldx #$00
down_loop:
  lda $0200,x
  clc
  adc #$01
  sta $0200,x
  inx
  inx
  inx
  inx
  cpx #$10
  bne down_loop
  rts


MoveMarioLeft:
 ; decrease the X byte of the first 4 sprites
  ldx #$00
left_loop:
  lda $0203,x
  sec
  sbc #$01
  sta $0203,x
  inx
  inx
  inx
  inx
  cpx #$10
  bne left_loop
  rts
  

MoveMarioRight:
 ; increase the X byte of the first 4 sprites
  ldx #$00
right_loop:
  lda $0203,x
  clc 
  adc #$01
  sta $0203,x
  inx
  inx
  inx
  inx
  cpx #$10
  bne right_loop
  rts


;; Check if Mario hit Goomba. Naive and trivial method, inefficient, but ok.
CheckCollision:
  lda #$00
  sta YesCollision
  ; CMP + BCS (branch if A >= DATA)
  lda $0213         ; check if Goomba has X coordinate higher than 
  cmp $020F         ; the low-right corner of mario
  bcs EndCollision  ; if yes, no collision. Otherwise:

  lda $0210         ; ...check if Goomba hat has Y coordinate lower than
  cmp $020C         ; Mario feets
  bcs EndCollision  ; if so, no collision. Otherwise:

  lda $020B         ; check if the left side of Mario has X coord higher than
  cmp $021B         ; the right side of Gumba
  bcs EndCollision  ; if so, no collision. Otherwise:

  lda $0200         ; check if Mario head has Y coord higher than
  cmp $021C         ; Goomba feets.
  bcs EndCollision  ; if so, no collision. Otherwise, collision:

  lda #$01
  sta YesCollision

EndCollision:
  rts


;; NMI Vector: actions to take at each frame!
NMI:
  lda GameStatus          ; Check the current GameStatus
  cmp #$00                ; 0 = Title / Start screen
  bne NotStartScreen
  jsr LoadTitleSprites    ; if Start screen, load the start sprites

        ;; In the Title screen, wait until a _new_ START is pressed:
  lda new_buttons         ; store in old_buttons the latest key reading
  sta old_buttons
  jsr ReadController      ; store in new_buttons the current key reading
        ;; check if the START bit _was_ ZERO and now is ONE (i.e. new press)
        ;; equivalent logic: NOT(old_buttons) AND (new_buttons) AND 00010000
  lda #$FF
  sec
  sbc old_buttons
  and new_buttons
  and #%00010000
  cmp #%00010000
  bne EndNMI        ; not pressed: jump to the end, i.e. stay in Title screen

  jsr ClearScreen   ; else, clear the screen, increase the GameStatus and END
  ldx GameStatus
  inx
  stx GameStatus
  stx GameStartsNow
  jmp EndNMI


NotStartScreen:
  lda GameStatus        ; if NOT Starting screen, check if Game screen (i.e. 1)
  cmp #$01
  bne GameOverScreen    ; if 1, Game screen, otherwise is Game over

  lda GameStartsNow     ; We are in the Game: if first time, load new sprites
  cmp #$00              ; otherwise just upload the previous sprites
  beq JustUpgradeSprites

  lda #$00              ; Load the game sprites if first time loading game
  sta GameStartsNow     ; set "no first time"
  jsr LoadGameSprites

JustUpgradeSprites:
  jsr UpgradeMario          ; Update the Mario sprite and check for collisions
  lda YesCollision          
  cmp #$00                  ; Check the collision variable
  beq EndNMI                ; if no collision: just end.

  jsr ClearScreen           ; Otherwise, Game Over: i) clear the screen
  ldx GameStatus            ; ii) set the GameStatus to 2 = Game Over
  inx
  stx GameStatus
  ldx #$00
  stx YesCollision          ; iii) reset the collision counter
  jmp EndNMI                ; iv) end NMI

GameOverScreen:
  jsr LoadLostSprites       ; Game over screen: message to press start 
  lda new_buttons           ; check if Start pressed again: if so, Start screen
  sta old_buttons
  jsr ReadController
  lda #$FF
  sec
  sbc old_buttons
  and new_buttons
  and #%00010000
  cmp #%00010000
  bne EndNMI                ; Start not pressed: stay here and end 

  jsr ClearScreen           ; Otherwise, clear and increase the game status
  ldx #$00
  stx GameStartsNow
  stx GameStatus
  jmp EndNMI

EndNMI:
  ; just update the graphics and end 
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  rti

;;;;;; Second bank referring to CPU memory
  .bank 1
  .org $E000

mypalette:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  .db $0F,$02,$23,$15,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C


title_sprites:
  ;   y   tile  attr  x
  .db $60, $23, $00, $60  ; H
  .db $60, $33, $00, $68  ; E 
  .db $60, $20, $00, $70  ; L
  .db $60, $20, $00, $78  ; L
  .db $60, $02, $00, $80  ; O
  .db $68, $21, $00, $60  ; W
  .db $68, $02, $00, $68  ; O
  .db $68, $10, $00, $70  ; R
  .db $68, $20, $00, $78  ; L
  .db $68, $22, $00, $80  ; D
  .db $68, $30, $00, $88  ; !
  .db $B0, $11, $00, $50  ; P
  .db $B0, $10, $00, $58  ; R
  .db $B0, $33, $00, $60  ; E
  .db $B0, $12, $00, $68  ; S
  .db $B0, $12, $00, $70  ; S
  .db $B8, $12, $00, $80  ; S
  .db $B8, $13, $00, $88  ; T
  .db $B8, $01, $00, $90  ; A
  .db $B8, $10, $00, $98  ; R
  .db $B8, $13, $00, $A0  ; T

game_sprites:
  ;   y   tile  attr  x
  .db $80, $40, $00, $60  ; Mario hat,    part 1
  .db $80, $41, $00, $68  ; Mario hat,    part 2 
  .db $88, $42, $00, $60  ; Mario body,   part 1
  .db $88, $43, $00, $68  ; Mario body,   part 2 
  .db $80, $70, $00, $A0  ; Goomba head,  part 1
  .db $80, $71, $00, $A8  ; Goomba head,  part 2 
  .db $88, $72, $00, $A0  ; Goomba body,  part 1
  .db $88, $73, $00, $A8  ; Goomba body,  part 2 

lost_sprites:
  ;   y   tile  attr  x
  .db $30, $00, $00, $70  ; G
  .db $30, $01, $00, $78  ; A 
  .db $30, $32, $00, $80  ; M
  .db $30, $33, $00, $88  ; E
  .db $38, $02, $00, $90  ; 0
  .db $38, $03, $00, $98  ; V
  .db $38, $33, $00, $A0  ; E
  .db $38, $10, $00, $A8  ; R
  .db $80, $11, $00, $30  ; P
  .db $80, $10, $00, $38  ; R
  .db $80, $33, $00, $40  ; E
  .db $80, $12, $00, $48  ; S
  .db $80, $12, $00, $50  ; S
  .db $88, $12, $00, $60  ; S
  .db $88, $13, $00, $68  ; T
  .db $88, $01, $00, $70  ; A
  .db $88, $10, $00, $78  ; R
  .db $88, $13, $00, $80  ; T

  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0


;;;; Bank referring to PPU memory
  .bank 2   ; memento: this bank refers entirely to the CHR memory, NOT CPU
  .org $0000
  .incbin "mario2.chr"
