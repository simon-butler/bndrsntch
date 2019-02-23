;==========================================================
; BNDRSNTCH
;
; MAIN source file, build entry point.
;
;==========================================================


;==========================================================
; PRE-PROCESSED CONSTANTS
;==========================================================

; Zero page addresses

; (x, y) screen position plotting
Z_SCR_X           = $02
Z_SCR_Y           = $03
Z_SCR_LOW_BYTE    = $04
Z_SCR_HI_BYTE     = $05
Z_COL_LOW_BYTE    = $06
Z_COL_HI_BYTE     = $07
Z_OFFSET          = $08

; temporary storage
Z_TEMP_1_HIGH     = $09
Z_TEMP_1_LOW      = $0A
Z_TEMP_1_MISC     = $0B


;==========================================================
; PROG SETUP
;==========================================================

; Create BASIC program listing for this program

!macro start_at .address {
  * = $0801
  !byte $0c,$08,$00,$00,$9e
  !if .address >= 10000 { !byte 48 + ((.address / 10000) % 10) }
  !if .address >=  1000 { !byte 48 + ((.address /  1000) % 10) }
  !if .address >=   100 { !byte 48 + ((.address /   100) % 10) }
  !if .address >=    10 { !byte 48 + ((.address /    10) % 10) }
  !byte $30 + (.address % 10), $00, $00, $00
  * = .address
}


;==========================================================
; MAIN CODE
;==========================================================

; put this code starting at mem $1000
+start_at $1000
* = $1000

title_screen
  jsr clear_screen
  jsr set_screen_bg_cols
  ldy #$00        ; zero Y, used in zero page indirect ($z),Y mem access below
  lda #$00        ; zero x and y zero page coordinate storage
  sta Z_SCR_X
  sta Z_SCR_Y
  lda #$53        ; store heart character code in A
  ldx #$02        ; store RED colour value in X
test_set_xy       ; (x,y) at (0,0)
  jsr set_xy      ; (x,y) -> (high,low) for both screen and color map
  sta (Z_SCR_LOW_BYTE), Y   ; write character to screen
  pha             ; push A to stack
  txa             ; X -> A, (for colour)
  sta (Z_COL_LOW_BYTE), Y   ; write colour to map
  pla             ; restore A from stack
test_set_xy_2
  inc Z_SCR_Y     ; increase y coord by 1, not at (0,1)
  jsr set_xy
  sta (Z_SCR_LOW_BYTE), Y
  inx             ; X++, use next colour (CYAN)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  pla
test_set_xy_offset_only
  iny             ; increase Y by 1, which is not the y plot position but the offset used in mem access, i.e. is (x + 1), i.e. (1,1)
  sta (Z_SCR_LOW_BYTE), Y
  inx             ; X++, use next colour (PURPLE) not actaully used
  inx             ; X++, use next colour (GREEN)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  pla
test_set_xy_3
  inc Z_SCR_X     ; increase x coord by 1
  inc Z_SCR_Y     ; increase y coord by 1
  ldy #$00        ; reset offset
  jsr set_xy      ; (x,y) now at (1,2)
  sta (Z_SCR_LOW_BYTE), Y
  inx             ; X++, use next colour (BLUE)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  ;pla

infinite_loop
  jmp *


;==========================================================
; ROUTINES
;==========================================================

; === clear_screen
;   clears (puts a SPACE character) at all locations of default screen memory
; params:
;   none
; uses:
;   X, Y, A
; side effects:
;   carry flag not restored (TODO)
clear_screen
  lda #$00                  ; put address $0400 (start of screen chars) at zero page temp addr Z_TEMP_1_LOW / HIGH
  tay                       ; - save #$00 to Y for later
  sta Z_TEMP_1_HIGH
  lda #$04
  sta Z_TEMP_1_LOW
  lda #$E8                  ; put high byte of address $07E8 (end of screen chars + 1) at zero page $16
  sta Z_TEMP_1_MISC
  ldx #$08                  ; load low byte of address $07E8 to X
  lda #$20                  ; put screen char SPACE in A
clear_screen_loop_1
  sta (Z_TEMP_1_HIGH), Y    ; (Z_TEMP_1_LOW/HIGH) + Y <- #$00
  iny
  bne clear_screen_skip_1
  inc Z_TEMP_1_LOW
  jmp clear_screen_loop_1
clear_screen_skip_1
  cpx Z_TEMP_1_LOW          ; check low byte of address $07E8 (end of screen chars + 1) against current addr low byte (zero page)
  bne clear_screen_loop_1
  cpy Z_TEMP_1_MISC         ; check high byte of address $07E8 (end of screen chars + 1) against current addr high byte (in Y)
  bne clear_screen_loop_1
  rts

; === set_screen_bg_cols
; black main screen, dark grey border
; params:
;   none
; uses:
;   A
; side effects:
;   none
set_screen_bg_cols
  pha
  lda #$0B
  sta $D020
  lda #$00
  sta $D021
  pla
  rts

; === set_xy
;   sets screen draw xy position, including color map position
; params:
;   Z_SCR_X - x value
;   Z_SCR_Y - y value
; uses:
;   X, Y, A, c
; side effects:
;   carry flag not restored (TODO)
;   translates to hi/low byte and resets offset
set_xy
  pha                                   ; push A to stack
  tya                                   ; Y -> A, and push to stack
  pha
  txa                                   ; X -> A, and push to stack
  pha
  ldx Z_SCR_X                           ; get x position from zero page var to X
  ldy Z_SCR_Y                           ; get y position from zero page var to Y
  lda #$04                              ; load high byte of screen start in A
  sta Z_SCR_HI_BYTE                     ; store in zero page
  lda #$D8                              ; load high byte of color map start in A
  sta Z_COL_HI_BYTE                     ; store in zero page
set_last_xy_complex_add
  lda #$00                              ; zero A, will track low byte of address until end of routine
set_last_xy_complex_add_test_y
  cpy #$00                              ; compare Y with $00
  beq set_last_xy_complex_add_finish    ; finish by adding X value at end
  dey                                   ; otherwise decrease Y by one and continue
set_last_xy_complex_add_y
  clc                                   ; clear carry flag before addition
  adc #$28                              ; add $28(40), i.e. row width, to A
  bcc set_last_xy_complex_add_test_y    ; if carry clear, didn't cross byte boundary, test to see if more rows left
  inc Z_SCR_HI_BYTE                     ; otherwise add one to scr hi byte (page)
  inc Z_COL_HI_BYTE                     ; repeat for col map
  jmp set_last_xy_complex_add_test_y    ; then test number of rows left
set_last_xy_complex_add_finish
  clc                                   ; clear carry flag before addition
  adc Z_SCR_X                           ; add X position to A
  bcc set_last_xy_complex_add_finish_a  ; if carry clear, didn't cross byte boundary, finished
  inc Z_SCR_HI_BYTE                     ; otherwise add one to hi byte (page)
  inc Z_COL_HI_BYTE                     ; repeat for col map
set_last_xy_complex_add_finish_a
  sta Z_SCR_LOW_BYTE                    ; finally store A in low byte, has been keeping running low byte count
  sta Z_COL_LOW_BYTE                    ; repeat for col map
set_last_xy_return
  pla                                   ; pull X value back from stack to A
  tax                                   ; A -> X, from pulled stack
  pla                                   ; pull Y value back from stack to A
  tay                                   ; A -> Y, from pulled stack
  pla                                   ; pull A value back from stack
  rts
