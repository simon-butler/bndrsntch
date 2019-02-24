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
Z_TEMP_1_LOW      = $10
Z_TEMP_1_HIGH     = $11

Z_TEMP_2_LOW      = $12
Z_TEMP_2_HIGH     = $13

Z_TEMP_3_LOW      = $14
Z_TEMP_3_HIGH     = $15

Z_TEMP_1_MISC     = $16
Z_TEMP_2_MISC     = $17
Z_TEMP_3_MISC     = $18

; working vars for specific routines
; - in general this is needed because these routines are called by other routines
Z_PSH_REG_TEMP          = $20     ; push all registers temp storage
Z_BYTE_MATCH_LOW        = $21
Z_BYTE_MATCH_HIGH       = $22
Z_BYTE_MATCH_BYTE       = $23
Z_BYTE_MATCH_LIST_SIZE  = $24


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

start_screen
  jsr clear_screen
  jsr set_screen_bg_cols
start_map
  ldx #$60 ;>screen_map
  jsr copy_map_to_screen
  jmp *

; --- OLD
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
  jsr push_all_registers
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
  jsr pull_all_registers
  rts


; === copy_map_to_screen
;   special map copy routine from stored memory location to screen, with coded colouring.
;   colours maze as dark gray, blocks as white, and block edges as light grey.
; params:
;   X - page where map starts, i.e. the high byte, assumes starts at zero starting offset
; uses:
;   A, X, Y
; side effects:
;   A, X, Y have random value after this routine
; returns:
;   none
copy_map_to_screen
  stx Z_TEMP_3_HIGH                     ; X -> Z_TEMP_3_HIGH, input for page where map data exists
  lda #$04
  sta Z_TEMP_1_HIGH                     ; put high part of address $0400 (screen chars) zero temp 1 addr
  lda #$D8
  sta Z_TEMP_2_HIGH                     ; put high part of address $D800 (col map) zero temp 2 addr
  lda #$00                              ; zero out low part of temp 1, 2, 3 and save to Y (for indirect mem access)
  sta Z_TEMP_1_LOW
  sta Z_TEMP_2_LOW
  sta Z_TEMP_3_LOW
  tay
  lda #$03                              ; put page count for reaching end of screen block (1000 bytes) in temp 1 misc, low value E8 hardcoded below
  sta Z_TEMP_1_MISC
copy_mp2scr_loop
  lda (Z_TEMP_3_LOW), Y                 ; load next character from memory
  sta (Z_TEMP_1_LOW), Y                 ; store in character mem
  ; match byte to map list, check for colouring
  sty Z_TEMP_2_MISC                     ; save Y in temp storage
  sta Z_TEMP_3_MISC                     ; save A in temp storage
  ldx #<list_of_map_chars                ; X <- low byte addr of list
  ldy #>list_of_map_chars                ; Y <- high byte addr of list
  jsr match_byte_from_list
  cmp #$FF                              ; compare with not found code $FF
  beq copy_mp2scr_non_map_char
copy_mp2scr_is_map_char
  lda #$0B                              ; set colour to grey
  jmp copy_mp2scr_set_col
copy_mp2scr_non_map_char
  lda Z_TEMP_3_MISC                     ; get A (character) from temp storage
  cmp #$A0                              ; check if is full block (only used for centre)
  beq copy_mp2scr_block_char
  lda #$0F                              ; set colour to light gray
  jmp copy_mp2scr_set_col
copy_mp2scr_block_char
  lda #$01                              ; set colour to white
copy_mp2scr_set_col
  ldy Z_TEMP_2_MISC                     ; restore Y from temp storage
  sta (Z_TEMP_2_LOW), Y                 ; store col in col mem
copy_mp2scr_next_pos
  iny                                   ; increase Y offset, affects all pointers
  cpy #$00
  beq copy_mp2scr_next_page             ; check if byte wrapped around to #$00, if so add page
  lda Z_TEMP_1_MISC                     ; load page counter to check if should check low byte amount
  cmp #$00
  bne copy_mp2scr_loop                  ; if didn't load zero then continue looping, only check low byte if no more pages left
  cpy #$E8                              ; check Y offset pointer against last position
  bne copy_mp2scr_loop                  ; if not equal, not finished yet, continue looping
  jmp copy_mp2scr_finish                ; otherwise we're done, finish
copy_mp2scr_next_page
  inc Z_TEMP_1_HIGH                     ; increase page for all pointers
  inc Z_TEMP_2_HIGH
  inc Z_TEMP_3_HIGH
  dec Z_TEMP_1_MISC                     ; decrease page counter by 1
  jmp copy_mp2scr_loop                  ; continue, end is not exactly on a page
copy_mp2scr_finish
  rts


; === match_byte_from_list
;   matches a given byte to byte in list
; params:
;   A - byte to match
;   X - list addr low byte
;   Y - list addr high byte
; uses:
;   A, X, Y
; side effects:
;   none, X, Y restored to original value after
; returns:
;   A - if matched, index of match (from list, not including list len), otherwise $FF
match_byte_from_list
  stx Z_BYTE_MATCH_LOW                  ; store address low byte
  sty Z_BYTE_MATCH_HIGH                 ; store address high byte
  sta Z_BYTE_MATCH_BYTE                 ; store byte to match in temp zero page storage
  ldy #$00                              ; zeroed zero page pointer offset
  lda (Z_BYTE_MATCH_LOW), Y             ; get list size to A
  sta Z_BYTE_MATCH_LIST_SIZE            ; store list size in temp zero page storage
match_byte_from_list_loop
  iny                                   ; Y++, pointer to next list entry
  lda (Z_BYTE_MATCH_LOW), Y             ; get next item of list to A
  cmp Z_BYTE_MATCH_BYTE                 ; compare list item with match char (in temp storage 2)
  beq match_byte_from_list_found        ; if match, goto found condition
  cpy Z_BYTE_MATCH_LIST_SIZE            ; check if reached end of list by comparing ptr index
  bne match_byte_from_list_loop         ; if not yet reached then loop
match_byte_from_list_no_match
  lda #$FF                              ; set no match code
  jmp match_byte_from_list_finish
match_byte_from_list_found
  dey                                   ; Y--, set Y to true index of match on list, not including list size entry
  tya                                   ; Y -> A, for return value
match_byte_from_list_finish
  ldx Z_BYTE_MATCH_LOW                  ; restore low and high byte addr to X, Y
  ldy Z_BYTE_MATCH_HIGH
  rts


; === push_all_registers
;   pushes the A, X, Y registers to stack
; side effects:
;   uses Z_PSH_REG_TEMP as temp storage, registers remain same as before this routine
push_all_registers
  sta Z_PSH_REG_TEMP                    ; save A in temp zero page storage
  pha                                   ; push A to stack
  tya                                   ; Y -> A, and push to stack
  pha
  txa                                   ; X -> A, and push to stack
  pha
  lda Z_PSH_REG_TEMP                    ; restore A from temp zero page storage
  rts


; === pull_all_registers
;   pushes the A, X, Y registers to stack
; side effects:
;   none
; return values:
;   A, X, Y contain same values as pushed previously with push_all_registers
pull_all_registers
  pla                                   ; pull X value back from stack to A
  tax                                   ; A -> X, from pulled stack
  pla                                   ; pull Y value back from stack to A
  tay                                   ; A -> Y, from pulled stack
  pla                                   ; pull A value back from stack
  rts


;==========================================================
; TABLES
;==========================================================

; NOTE: lists always start with the length of the list (which should be +1 size of list, to include size byte),
;       followed by the values

* = $5000

list_of_map_chars
; len (15), amount + 14 chars of data
!byte $0F,$14,$40,$42,$43,$5B,$5D,$6B,$6D,$6E,$70,$71,$72,$73,$7D

front_facing_info_data
;     -----chars----- -----exits-----
;     Up  Rt  Lft Dwn Up  Rt  Lft Dwn
!byte $42,$40,$40,$42,$FF,$00,$00,$FF     ; front only
!byte $6B,$72,$71,$73,$FF,$FF,$00,$FF     ; front and right
!byte $73,$71,$72,$6B,$FF,$00,$FF,$FF     ; front and left
!byte $70,$6E,$6D,$7D,$00,$FF,$00,$FF     ; right only
!byte $6E,$7D,$70,$6D,$00,$00,$FF,$FF     ; left only
!byte $5B,$5B,$5B,$5B,$FF,$FF,$FF,$FF     ; front, left and right
!byte $72,$73,$6B,$71,$00,$FF,$FF,$00     ; left and right


;==========================================================
; IMAGE DATA
;==========================================================

* = $6000

screen_map
!byte 112,113,110,109,115,112,64,64,64,114,110,112,64,64,110,109,110,112,64,64,64,125,109,64,115,112,113,110,112,113,110,112,67,67,67,110,112,114,110,93
!byte 109,110,66,112,91,125,112,64,114,113,115,109,64,110,107,64,91,125,111,111,111,111,111,111,109,115,112,125,93,112,115,93,112,64,114,113,125,93,66,93
!byte 114,125,109,115,109,110,107,64,91,114,113,64,64,115,93,112,115,106,160,160,160,160,160,160,116,66,109,114,115,107,91,113,115,112,125,112,110,93,107,125
!byte 93,112,110,107,64,113,125,112,115,109,110,112,64,115,107,125,93,106,160,160,160,160,160,160,116,109,114,125,93,107,91,110,66,93,112,113,91,113,91,110
!byte 109,125,109,115,112,64,110,66,66,112,125,109,110,107,125,112,125,106,160,160,160,160,160,160,116,112,113,110,93,109,91,91,115,109,91,64,115,112,125,66
!byte 64,64,114,125,109,110,109,125,66,107,114,64,115,107,110,109,110,106,160,160,160,160,160,160,116,107,64,125,93,112,115,93,107,110,107,110,66,109,114,125
!byte 112,110,93,112,110,107,64,110,109,113,115,112,125,93,109,64,115,106,160,160,160,160,160,160,116,107,114,64,91,91,91,125,93,109,115,109,113,114,91,110
!byte 93,109,113,115,107,125,112,115,112,64,113,107,64,113,64,64,125,106,160,160,160,160,160,160,116,109,113,64,125,109,91,110,107,64,91,64,64,115,93,93
!byte 109,114,64,125,109,114,115,109,115,112,64,125,111,111,111,111,111,122,160,160,160,160,160,160,76,111,111,111,111,111,109,115,107,110,109,110,112,113,125,93
!byte 64,115,112,64,110,109,91,110,93,109,110,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,66,112,125,107,64,64,125
!byte 112,91,113,110,109,114,115,109,91,110,93,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,110,109,115,112,113,110,112,114
!byte 93,107,64,113,114,125,109,110,109,115,93,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,91,64,113,91,64,91,125,93
!byte 93,109,114,110,109,64,110,93,112,125,93,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,112,64,125,112,113,110,93
!byte 109,114,115,109,64,64,125,109,113,114,115,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,64,115,112,114,113,110,107,125
!byte 112,125,107,64,64,64,114,64,64,113,125,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,93,112,115,109,115,112,113,91,110
!byte 109,110,93,112,64,110,109,64,64,114,110,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,107,110,107,125,112,113,125
!byte 112,115,109,125,112,113,64,114,64,91,125,106,160,160,160,160,160,160,79,20,119,119,119,80,160,160,160,160,160,160,116,109,114,115,107,91,110,66,112,110
!byte 109,113,64,110,66,112,64,113,110,107,110,106,160,160,160,160,160,160,116,93,112,114,110,106,160,160,160,160,160,160,116,112,125,93,93,107,91,113,125,93
!byte 64,64,64,91,113,91,110,112,125,93,93,106,160,160,160,160,160,160,116,107,125,107,115,106,160,160,160,160,160,160,116,109,110,93,107,115,109,64,64,113
!byte 112,114,110,109,110,107,125,109,110,109,115,106,160,160,160,160,160,160,116,109,110,93,93,106,160,160,160,160,160,160,116,112,125,109,125,109,114,114,114,64
!byte 125,93,93,112,91,113,64,114,113,64,115,106,160,160,160,160,160,160,116,112,91,113,125,106,160,160,160,160,160,160,116,107,64,64,64,64,115,93,93,112
!byte 112,125,109,91,125,112,64,113,64,64,115,106,160,160,160,160,160,160,116,93,107,114,110,106,160,160,160,160,160,160,116,107,110,112,114,64,113,115,93,93
!byte 107,110,112,113,64,115,112,64,64,110,109,110,119,119,119,119,119,119,112,91,125,93,107,110,119,119,119,119,119,119,112,125,93,107,115,112,110,107,91,115
!byte 107,91,125,112,110,93,93,112,64,125,112,115,112,64,110,112,114,64,115,107,110,109,91,91,114,110,112,64,114,110,107,114,91,113,125,107,91,125,107,125
!byte 125,109,114,125,109,125,93,109,64,64,125,109,125,112,113,125,109,110,109,125,109,64,125,109,113,113,125,112,125,109,125,93,109,64,64,125,109,64,125,112