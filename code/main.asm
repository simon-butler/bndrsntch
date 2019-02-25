;==========================================================
; BNDRSNTCH
;
; MAIN source file, build entry point.
;
;==========================================================


;==========================================================
; PRE-PROCESSED CONSTANTS
;==========================================================

; IMPORTANT!!! don't forget to use # before using the constants,
;         or they will be probably treated as zero page addresses

CN_SCR_MEM_START_LOW    = $00
CN_SCR_MEM_START_HIGH   = $04
CN_SCR_MEM_END_LOW      = $E8
CN_SCR_MEM_END_HIGH     = $07

CN_COL_MEM_START_LOW    = $00
CN_COL_MEM_START_HIGH   = $D8
CN_COL_MEM_END_LOW      = $E8
CN_COL_MEM_END_HIGH     = $DB

CN_CHAR_SPACE           = $20

CN_COL_VAL_BLACK        = $00
CN_COL_VAL_WHITE        = $01
CN_COL_VAL_RED          = $02
CN_COL_VAL_CYAN         = $03
CN_COL_VAL_PURPLE       = $04
CN_COL_VAL_GREEN        = $05
CN_COL_VAL_BLUE         = $06
CN_COL_VAL_YELLOW       = $07
CN_COL_VAL_ORANGE       = $08
CN_COL_VAL_BROWN        = $09
CN_COL_VAL_L_RED        = $0A
CN_COL_VAL_D_GREY       = $0B
CN_COL_VAL_M_GREY       = $0C
CN_COL_VAL_L_GREEN      = $0D
CN_COL_VAL_L_BLUE       = $0E
CN_COL_VAL_L_GREY       = $0F

CN_XY_MAP_STR_LINE_1_X  = $0E
CN_XY_MAP_STR_LINE_1_Y  = $0B


;==========================================================
; ZERO PAGE MAP
;==========================================================

; (x, y) screen position plotting
Z_SCR_X           = $02
Z_SCR_Y           = $03
Z_SCR_LOW_BYTE    = $04
Z_SCR_HI_BYTE     = $05
Z_COL_LOW_BYTE    = $06
Z_COL_HI_BYTE     = $07
Z_OFFSET          = $08

; general purpose low/high address pairs
Z_ADDR_1_LOW      = $10
Z_ADDR_1_HIGH     = $11
Z_ADDR_2_LOW      = $12
Z_ADDR_2_HIGH     = $13
Z_ADDR_3_LOW      = $14
Z_ADDR_3_HIGH     = $15
Z_ADDR_4_LOW      = $16
Z_ADDR_4_HIGH     = $17

Z_TEMP_1          = $18
Z_TEMP_2          = $19
Z_TEMP_3          = $1A
Z_TEMP_4          = $1B

Z_PSH_REG_A       = $1C
Z_PSH_REG_X       = $1D
Z_PSH_REG_Y       = $1E

; working vars for specific routines
Z_BYTE_MATCH_LOW        = $20
Z_BYTE_MATCH_HIGH       = $21
Z_BYTE_MATCH_BYTE       = $22
Z_BYTE_MATCH_LIST_SIZE  = $23

; user data
Z_PLYR_POS_X            = $40 ;$07
Z_PLYR_POS_Y            = $41 ;$14 (20)
Z_PLYR_FACING           = $42 ;$00   ;(0,1,2,3) -> (up,right,left,down)


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
  jsr set_screen_bg_cols    ; setup screen background and border colour to defaults
  lda #CN_COL_VAL_D_GREY    ; fill screen with black colour, black characters on black, i.e. nothing visible even with char data
  jsr fill_screen_cols
start_map
  ldx #>data_scr_map        ; put high byte (page) of screen map data in X, setup for copy_map_chars_to_screen
  jsr copy_map_chars_to_screen
start_player_data
  lda #$07                  ; player X pos = 07
  sta Z_PLYR_POS_X
  lda #$14                  ; player y pos = 20 ($14)
  sta Z_PLYR_POS_Y
  lda #$00                  ; player facing direction = UP (0)
  sta Z_PLYR_FACING
test_show_player_loc
  lda Z_PLYR_POS_X
  sta Z_SCR_X
  lda Z_PLYR_POS_Y
  sta Z_SCR_Y
  jsr plot_set_xy
  lda #CN_COL_VAL_WHITE
  ldy #$00
  sta (Z_COL_LOW_BYTE), Y
test_write_strings
  ; move plot point to string write position
  lda #CN_XY_MAP_STR_LINE_1_X
  sta Z_SCR_X
  lda #CN_XY_MAP_STR_LINE_1_Y
  sta Z_SCR_Y
  jsr plot_set_xy
  ; write string
  lda #>data_str_page_1
  sta Z_ADDR_1_HIGH
  lda #<data_str_pg1_facing_north
  sta Z_ADDR_1_LOW
  jsr draw_chars_list
infinite_loop
  jmp *

; --- OLD
  ldy #$00                  ; zero Y, used in zero page indirect ($z),Y mem access below
  lda #$00                  ; zero x and y zero page coordinate storage
  sta Z_SCR_X
  sta Z_SCR_Y
  lda #$53                  ; store heart character code in A
  ldx #$02                  ; store RED colour value in X
test_plot_set_xy                 ; (x,y) at (0,0)
  jsr plot_set_xy                ; (x,y) -> (high,low) for both screen and color map
  sta (Z_SCR_LOW_BYTE), Y   ; write character to screen
  pha                       ; push A to stack
  txa                       ; X -> A, (for colour)
  sta (Z_COL_LOW_BYTE), Y   ; write colour to map
  pla                       ; restore A from stack
test_plot_set_xy_2
  inc Z_SCR_Y               ; increase y coord by 1, not at (0,1)
  jsr plot_set_xy
  sta (Z_SCR_LOW_BYTE), Y
  inx                       ; X++, use next colour (CYAN)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  pla
test_plot_set_xy_offset_only
  iny                       ; increase Y by 1, which is not the y plot position but the offset used in mem access, i.e. is (x + 1), i.e. (1,1)
  sta (Z_SCR_LOW_BYTE), Y
  inx                       ; X++, use next colour (PURPLE) not actaully used
  inx                       ; X++, use next colour (GREEN)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  pla
test_plot_set_xy_3
  inc Z_SCR_X               ; increase x coord by 1
  inc Z_SCR_Y               ; increase y coord by 1
  ldy #$00                  ; reset offset
  jsr plot_set_xy                ; (x,y) now at (1,2)
  sta (Z_SCR_LOW_BYTE), Y
  inx                       ; X++, use next colour (BLUE)
  pha
  txa
  sta (Z_COL_LOW_BYTE), Y
  ;pla

infinite_loop_old
  jmp *


;==========================================================
; ROUTINES
;==========================================================

; === fill_screen_chars
;   fill entire screen with single character
; params:
;   A - character to fill screen with
; uses:
;   A, X
; side effects:
;   fill_mem routine called uses A, X, Z_ADDR_1 L/H, Z_ADDR_2 L/H
fill_screen_chars
  ldx #CN_SCR_MEM_START_LOW       ; set start / end addres in low / high for fill_mem call
  stx Z_ADDR_1_LOW
  ldx #CN_SCR_MEM_START_HIGH
  stx Z_ADDR_1_HIGH
  ldx #CN_SCR_MEM_END_LOW
  stx Z_ADDR_2_LOW
  ldx #CN_SCR_MEM_END_HIGH
  stx Z_ADDR_2_HIGH
  jsr fill_mem                    ; call fill mem, A already set by caller of this routine
  rts

; === fill_screen_cols
;   fill entire colour map with single colour
; params:
;   A - colour to fill colour map with
; uses:
;   A, X
; side effects:
;   fill_mem routine called uses A, X, Z_ADDR_1 L/H, Z_ADDR_2 L/H
fill_screen_cols
  ldx #CN_COL_MEM_START_LOW       ; set start / end addres in low / high for fill_mem call
  stx Z_ADDR_1_LOW
  ldx #CN_COL_MEM_START_HIGH
  stx Z_ADDR_1_HIGH
  ldx #CN_COL_MEM_END_LOW
  stx Z_ADDR_2_LOW
  ldx #CN_COL_MEM_END_HIGH
  stx Z_ADDR_2_HIGH
  jsr fill_mem                    ; call fill mem, A already set by caller of this routine
  rts


; === fill_mem
;   fill memory range with a single byte
; params:
;   Z_ADDR_1_HIGH / LOW - from memory location (inclusive)
;   Z_ADDR_2_HIGH / LOW - to memory location (inclusive)
;   A - byte to fill
; uses:
;   X, Y, A
; side effects:
;   none
fill_mem
  ldy #$00                  ; zero Y register
  tax                       ; keep copy of byte in X
fill_mem_loop
  txa                       ; restore byte to fill from X -> A
  sta (Z_ADDR_1_LOW), Y     ; store byte to fill in next address
  inc Z_ADDR_1_LOW          ; next mem addr, low byte
  lda Z_ADDR_1_LOW          ; load low byte mem addr (automatically checks for zero)
  bne fill_mem_check_high   ; if not zero, go to high byte check
fill_mem_paged
  inc Z_ADDR_1_HIGH         ; increase high byte, since low byte wrapped from $FF to $00
fill_mem_check_high
  lda Z_ADDR_1_HIGH         ; load high byte mem addr
  cmp Z_ADDR_2_HIGH         ; check against ending high byte
  beq fill_mem_check_low    ; if match (we're in last page) then go to check low byte for match
  jmp fill_mem_loop         ; otherwise still more bytes, loop
fill_mem_check_low
  lda Z_ADDR_1_LOW          ; load high byte mem addr
  cmp Z_ADDR_2_LOW          ; check against ending high byte (only reaches here if high byte already matched)
  beq fill_mem_finish       ; if match then we're done, finish
  jmp fill_mem_loop         ; otherwise still more bytes, loop
fill_mem_finish
  rts


; === draw_chars_list
;   draw a chars list (e.g. front sized char list AKA string) on screen at current screen plot pos, copying contents from memory.
;   DOES NOT change plot positions
; params:
;   !!! plot_set_xy should already have been called to set up plot location
;   Z_ADDR_1_HIGH / LOW - memory location of char list / string
; uses:
;   X, Y, A
;   Z_TEMP_1
; side effects:
;   modifies Z_ADDR_1_LOW to +1, make it easy to keep track of plotting position
; returns:
;   Y - size of char list written
draw_chars_list
  ldy #$00                  ; zero Y register, for indirect indexing
draw_chars_list_get_len
  lda (Z_ADDR_1_LOW), Y     ; get length of char list
  sta Z_TEMP_1              ; store char list len in temp zero page storage
  inc Z_ADDR_1_LOW          ; increase low byte position to point at first element in list
draw_chars_list_next
  lda (Z_ADDR_1_LOW), Y     ; get next character from memory to A
  sta (Z_SCR_LOW_BYTE), Y   ; store character in screen mem
  iny                       ; Y++
  cpy Z_TEMP_1              ; compare Y with char list len
  bne draw_chars_list_next  ; if not equal, continue with next char
  rts                       ; otherwise, done, finish


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


; === plot_set_xy
;   sets screen draw xy position, including color map position
; params:
;   Z_SCR_X - x value
;   Z_SCR_Y - y value
; uses:
;   X, Y, A, c
; side effects:
;   modifies all plot variables
;   translates to hi/low byte and resets offset
plot_set_xy
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
  rts

; === plot_inc_x_line_wrap
;   increase plot x position only, fast method for this common need,
;   does not increase Y position, wraps back to left side if was at end.
;   assumes positions already set and correct (not out of range)
; params:
;   none
; side effects:
;   all plot variables
plot_inc_x
  inc Z_SCR_X                           ; increase x position
  lda Z_SCR_X                           ; load x position to A
  cmp #$29                              ; check out of range, i.e. equal to 41 ($29)
  bne plot_inc_x_add_to_bytes           ; if not, update bytes
  lda #$00                              ; otherwise, set A to zero
  sta Z_SCR_X                           ;   and store in X, i.e. back to left side
  lda Z_SCR_LOW_BYTE                    ; load screen low byte (will be same as col map)
  clc                                   ; clear carry flag, in prep for subtract
  sbc $27                               ; subtract 39 from low byte
  sta Z_SCR_LOW_BYTE                    ; save adjusted A as screen low byte
  sta Z_COL_LOW_BYTE                    ;   and to col map low byte
  bcc plot_inc_x_finish                 ; if carry (AKA borrow in this case) not set, nothing more to adjust, finish
  dec Z_SCR_HI_BYTE                     ; otherwise decrease page of screen
  dec Z_COL_HI_BYTE                     ;   and of col map
  jmp plot_inc_x_finish
plot_inc_x_add_to_bytes
  inc Z_SCR_LOW_BYTE                    ; increase low byte of scr 
  inc Z_COL_LOW_BYTE                    ; increase low byte of col map
  lda Z_SCR_LOW_BYTE                    ; load screen low byte (will auto set zero flag if zero, i.e. wrapped)
  bne plot_inc_x_finish                 ; if not zero, don't need to update page, done
  inc Z_SCR_HI_BYTE                     ; otherwise increase page of screen
  inc Z_COL_HI_BYTE                     ;   and of col map
plot_inc_x_finish
  rts


; === copy_visible_map_to_screen
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
copy_visible_map_to_screen
  stx Z_ADDR_3_HIGH                     ; X -> Z_ADDR_3_HIGH, input for page where map data exists
  lda #$04
  sta Z_ADDR_1_HIGH                     ; put high part of address $0400 (screen chars) zero temp 1 addr
  lda #$D8
  sta Z_ADDR_2_HIGH                     ; put high part of address $D800 (col map) zero temp 2 addr
  lda #$00                              ; zero out low part of temp 1, 2, 3 and save to Y (for indirect mem access)
  sta Z_ADDR_1_LOW
  sta Z_ADDR_2_LOW
  sta Z_ADDR_3_LOW
  tay
  lda #$03                              ; put page count for reaching end of screen block (1000 bytes) in temp 1 misc, low value E8 hardcoded below
  sta Z_TEMP_1
copy_vm2scr_loop
  lda (Z_ADDR_3_LOW), Y                 ; load next character from memory
  sta (Z_ADDR_1_LOW), Y                 ; store in character mem
  ; match byte to map list, check for colouring
  sty Z_TEMP_2                          ; save Y in temp storage
  sta Z_TEMP_3                          ; save A in temp storage
  ldx #<data_map_chars_list             ; X <- low byte addr of list
  ldy #>data_map_chars_list             ; Y <- high byte addr of list
  jsr match_byte_from_list
  cmp #$FF                              ; compare with not found code $FF
  beq copy_vm2scr_non_map_char
copy_vm2scr_is_map_char
  lda #$0B                              ; set colour to grey
  jmp copy_vm2scr_set_col
copy_vm2scr_non_map_char
  lda Z_TEMP_3                          ; get A (character) from temp storage
  cmp #$A0                              ; check if is full block (only used for centre)
  beq copy_vm2scr_block_char
  lda #$0F                              ; set colour to light gray
  jmp copy_vm2scr_set_col
copy_vm2scr_block_char
  lda #$01                              ; set colour to white
copy_vm2scr_set_col
  ldy Z_TEMP_2                          ; restore Y from temp storage
  sta (Z_ADDR_2_LOW), Y                 ; store col in col mem
copy_vm2scr_next_pos
  iny                                   ; increase Y offset, affects all pointers
  cpy #$00
  beq copy_vm2scr_next_page             ; check if byte wrapped around to #$00, if so add page
  lda Z_TEMP_1                          ; load page counter to check if should check low byte amount
  cmp #$00
  bne copy_vm2scr_loop                  ; if didn't load zero then continue looping, only check low byte if no more pages left
  cpy #$E8                              ; check Y offset pointer against last position
  bne copy_vm2scr_loop                  ; if not equal, not finished yet, continue looping
  jmp copy_vm2scr_finish                ; otherwise we're done, finish
copy_vm2scr_next_page
  inc Z_ADDR_1_HIGH                     ; increase page for all pointers
  inc Z_ADDR_2_HIGH
  inc Z_ADDR_3_HIGH
  dec Z_TEMP_1                          ; decrease page counter by 1
  jmp copy_vm2scr_loop                  ; continue, end is not exactly on a page
copy_vm2scr_finish
  rts


; === copy_map_chars_to_screen
;   map copy routine from stored memory location to screen, characters only, does not set any colour map data
; params:
;   X - page where map starts, i.e. the high byte, assumes starts at zero starting offset
; uses:
;   A, X, Y
; side effects:
;   A, X, Y have random value after this routine
; returns:
;   none
copy_map_chars_to_screen
  stx Z_ADDR_3_HIGH                     ; X -> Z_ADDR_3_HIGH, input for page where map data exists
  lda #$04
  sta Z_ADDR_1_HIGH                     ; put high part of address $0400 (screen chars) zero temp 1 addr
  lda #$D8
  lda #$00                              ; zero out low part of temp 1, 2, 3 and save to Y (for indirect mem access)
  sta Z_ADDR_1_LOW
  sta Z_ADDR_3_LOW
  tay
  lda #$03                              ; put page count for reaching end of screen block (1000 bytes) in temp 1 misc, low value E8 hardcoded below
  sta Z_TEMP_1
copy_mp2scr_loop
  lda (Z_ADDR_3_LOW), Y                 ; load next character from memory
  sta (Z_ADDR_1_LOW), Y                 ; store in character mem
copy_mp2scr_next_pos
  iny                                   ; increase Y offset, affects all pointers
  cpy #$00
  beq copy_mp2scr_next_page             ; check if byte wrapped around to #$00, if so add page
  lda Z_TEMP_1                          ; load page counter to check if should check low byte amount
  cmp #$00
  bne copy_mp2scr_loop                  ; if didn't load zero then continue looping, only check low byte if no more pages left
  cpy #$E8                              ; check Y offset pointer against last position
  bne copy_mp2scr_loop                  ; if not equal, not finished yet, continue looping
  jmp copy_mp2scr_finish                ; otherwise we're done, finish
copy_mp2scr_next_page
  inc Z_ADDR_1_HIGH                     ; increase page for all pointers
  inc Z_ADDR_3_HIGH
  dec Z_TEMP_1                          ; decrease page counter by 1
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


; === save_registers
;   save A, X, Y registers to zero page
; side effects:
;   uses Z_PSH_REG_A / X / Y as temp storage
;   registers remain same as before this routine
save_registers
  sta Z_PSH_REG_A
  stx Z_PSH_REG_X
  sty Z_PSH_REG_Y
  rts


; === restore_registers
;   restore A, X, Y registers from temp zero page storage
; params:
;   Z_PSH_REG_A / X / Y are a kind of input, should be previously saved with save_registers
; side effects:
;   none
; return values:
;   A, X, Y contain same values as pushed previously with save_registers
restore_registers
  lda Z_PSH_REG_A
  ldx Z_PSH_REG_X
  ldy Z_PSH_REG_Y
  rts


;==========================================================
; TABLES
;==========================================================

; NOTE: lists always start with the length of the list (which should be +1 size of list, to include size byte),
;       followed by the values

* = $5000

data_map_chars_list
; len (15), amount + 14 chars of data
!byte $0F
!byte $14,$40,$42,$43,$5B,$5D,$6B,$6D,$6E,$70,$71,$72,$73,$7D

data_front_facing_info
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
; STRING DATA
;==========================================================

; strings are character lists, starting with list size NOT INCLUDING this byte
; this makes the max char list size 254

* = $5100

data_str_page_1
data_str_pg1_facing_north
!byte $0C
!scr "facing north"
data_str_pg1_facing_south
!byte $0C
!scr "facing south"
data_str_pg1_facing_east
!byte $0B
!scr "facing east"
data_str_pg1_facing_west
!byte $0B
!scr "facing west"

;==========================================================
; IMAGE DATA
;==========================================================

* = $6000

data_scr_map
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