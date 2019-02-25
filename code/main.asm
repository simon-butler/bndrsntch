;==========================================================
; BNDRSNTCH
;
; MAIN source file, build entry point.
;
;==========================================================

;==========================================================
; SYSTEM AND KERNAL ROUTINES 
;==========================================================

FN_GETIN        = $FFE4     ; get character from buffer (keyboard buffer unless changed)

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
; - core
Z_PLYR_POS_X            = $40
Z_PLYR_POS_Y            = $41
Z_PLYR_FACING           = $42
; - helper
Z_PLYR_LOC_TYPE         = $43
Z_PLYR_LOC_TY_ADDR_LOW  = $44
Z_PLYR_LOC_TY_ADDR_HIGH = $45


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
  lda #CN_COL_VAL_BLACK     ; fill screen with black colour, black characters on black, i.e. nothing visible even with char data
  ;lda #CN_COL_VAL_D_GREY    ; fill screen with GREY colour, for test purposes
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
update_player_pos_type
  jsr determine_location_type
show_player_loc
  ; player location shown by making col map at player pos white
  lda Z_PLYR_POS_X
  sta Z_SCR_X
  lda Z_PLYR_POS_Y
  sta Z_SCR_Y
  jsr plot_set_xy
  lda #CN_COL_VAL_WHITE
  ldy #$00
  sta (Z_COL_LOW_BYTE), Y
show_player_facing_str
  jsr write_player_facing_str
game_loop_move
  jsr wait_for_key                      ; wait for a key from user
  ldx #<data_key_codes_to_facing_dir    ; set up LOW / HIGH addr of key to facing direction mapping
  ldy #>data_key_codes_to_facing_dir
  jsr match_byte_from_list              ; try to match key to facing direction
  cmp #$FF                              ; check if match failed
  beq game_loop_move                    ; no match, continue getting key : TODO show bad input message
  pha                                   ; save A, direction to face
  lda Z_PLYR_POS_X                      ; set player (x,y) to screen (x,y)
  sta Z_SCR_X
  lda Z_PLYR_POS_Y
  sta Z_SCR_Y
  jsr plot_set_xy                       ; update plot variables with (x,y)
  lda #CN_COL_VAL_M_GREY                ; set A to medium grey
  ldy #$00                              ; zero Y for indirect addr
  sta (Z_COL_LOW_BYTE), Y               ; store A (med grey) in col map (at loc x,y)
  pla                                   ; restore A, has direction to face
  jsr move_player_in_dir                ; try to move player in direction specified
  cmp #$FF                              ; check if move failed
  beq show_player_loc                   ; move did fail, redraw current position as latest position
  jmp update_player_pos_type            ; otherwise update from new move position, new draw and pass to main loop
infinite_loop
  jmp *


;==========================================================
; ROUTINES - HIGH LEVEL
;==========================================================

; these routines perform some common game particular function,
; mostly using lower level functions


; === move_player_in_dir
;   try to move a player in direction given current position
; assumptions:
;   - A input is number between $00 and $03 inclusive
;   - player location already determined
;   - data_facing_matrix does not cross page boundary
; params:
;   A - direction to move: (0, 1, 2, 3) -> (Up, right, left, down)
; uses:
;   A, Y
;   Z_TEMP_1, Z_TEMP_2
;   Z_PLYR_LOC_TY_ADDR_LOW / HIGH
;   Z_ADDR_1_LOW / HIGH
;   all player core variables
; side effects:
; returns:
;   A - new player direction if successful, or $FF if failed to move
;   player core pos will be updated if successful
;   player facing direction will be updated if successful
move_player_in_dir
  sta Z_TEMP_1                      ; store direction to try to move to face in temp storage
  clc                               ; clear carry flag to prepare for addition
  adc #$04                          ; add $04, set offset to facing direction part of location type row bytes
  tay                               ; A -> Y for indirect addressing offset
  lda (Z_PLYR_LOC_TY_ADDR_LOW), Y   ; get facing direction byte code ($00 = can move, $FF = can't move)
  cmp #$FF                          ; check if can't move
  beq mv_plyr_in_dir_failed         ; if can't move, finish already (fail code already in A)
  lda #>data_facing_matrix          ; put high byte (page) of matrix facing table in temp addr 1 storage high
  sta Z_ADDR_1_HIGH
  lda #<data_facing_matrix          ; get low byte of matrix facing table to A, will move through rows to get for current facing dir
  ldx Z_PLYR_FACING                 ; read current direction facing from temp var to X
mv_plyr_in_dir_facing_loop
  beq mv_plyr_in_dir_facing_read    ; when X == 0, go to read position
  clc                               ; clear carry flag before addition
  adc #$04                          ; add row length of data_facing_matrix
  dex
  jmp mv_plyr_in_dir_facing_loop
mv_plyr_in_dir_facing_read
  sta Z_ADDR_1_LOW                  ; save low byte addr, is row start in data_facing_matrix
  ldy Z_TEMP_1                      ; read dir to move to Y, used as offset to read the right position
  lda (Z_ADDR_1_LOW), Y             ; get new direction to face
  sta Z_PLYR_FACING                 ; store direction to face
  ; move player x
  lda #>data_x_movement_facing_dir  ; put high byte (page) of data x movement for new direction in addr 1
  sta Z_ADDR_1_HIGH
  lda #<data_x_movement_facing_dir  ; same for low byte
  sta Z_ADDR_1_LOW
  ldy Z_PLYR_FACING                 ; load new (and now current) player facing direction to Y for offset
  lda (Z_ADDR_1_LOW), Y             ; get amount to move in x direction
  sta Z_TEMP_2                      ; temp store 2 amount
  dec Z_PLYR_POS_X                  ; amount needs to be offset by -1 by dec cur value, not using negative numbers
  lda Z_PLYR_POS_X                  ; get position
  clc                               ; clear carry flag before addition
  adc Z_TEMP_2                      ; add amount to move x position to x position
  sta Z_PLYR_POS_X                  ; store updated position back in player pos x
  ; move player y
  lda #>data_y_movement_facing_dir  ; put high byte (page) of data y movement for new direction in addr 1
  sta Z_ADDR_1_HIGH
  lda #<data_y_movement_facing_dir  ; same for low byte
  sta Z_ADDR_1_LOW
  lda (Z_ADDR_1_LOW), Y             ; get amount to move in x direction (Y is still correct from x position update)
  sta Z_TEMP_2                      ; temp store 2 amount
  dec Z_PLYR_POS_Y                  ; amount needs to be offset by -1 by dec cur value, not using negative numbers
  lda Z_PLYR_POS_Y                  ; get position
  clc                               ; clear carry flag before addition
  adc Z_TEMP_2                      ; add amount to move x position to x position
  sta Z_PLYR_POS_Y                  ; store updated position back in player pos x
  lda Z_PLYR_FACING                 ; put updated / current player facing direction in A, is indication of success on return
  jmp mv_plyr_in_dir_finish
mv_plyr_in_dir_failed
  lda Z_TEMP_1
mv_plyr_in_dir_finish
  rts

; === write_player_facing_str
;   write player facing string at first line (CN_XY_MAP_STR_LINE_1_X / Y) of map write space
; params:
;   none
; uses:
;   A, X
;   Z_ADDR_1_HIGH / LOW
; side effects:
;   plot_xy:                    A, X, Y, c, plot variables
;   draw_chars_list_with_col:   A, X, Y, Z_TEMP_1
; returns:
;   none
write_player_facing_str
  lda #>data_str_page_1
  sta Z_ADDR_1_HIGH
  lda Z_PLYR_FACING                 ; get player facing direction
  cmp #$00                          ; check if north
  beq show_pl_facing_str_n          ; if so, jump to north
  cmp #$01
  beq show_pl_facing_str_e
  cmp #$02
  beq show_pl_facing_str_w
  lda #<data_str_pg1_facing_south   ; otherwise, pass through last condition, is south (facing == $03), load string low bytes
  jmp show_pl_facing_str_finish
show_pl_facing_str_n
  lda #<data_str_pg1_facing_north   ; get low bytes for facing north string
  jmp show_pl_facing_str_finish
show_pl_facing_str_e
  lda #<data_str_pg1_facing_east    ; get low bytes for facing north string
  jmp show_pl_facing_str_finish
show_pl_facing_str_w
  lda #<data_str_pg1_facing_west    ; get low bytes for facing north string
  jmp show_pl_facing_str_finish
show_pl_facing_str_finish
  sta Z_ADDR_1_LOW                  ; store low byte for string
  ; move plot point to string write position
  lda #CN_XY_MAP_STR_LINE_1_X
  sta Z_SCR_X
  lda #CN_XY_MAP_STR_LINE_1_Y
  sta Z_SCR_Y
  jsr plot_set_xy
  ldx #CN_COL_VAL_WHITE             ; load white colour for string write
  jsr draw_chars_list_with_col      ; finally write string
  rts


; === determine_location_type
;   determines location type from player location and direction facing info,
;   i.e. gets table row number in front facing info table, used in other routines and logic
; assumptions:
;   - player core data is correct
;   - there are 7 rows of table data
;   - table data does not cross page boundary
; params:
;   none
; uses:
;   A, X, Y, c
;   Z_TEMP_1
; side effects:
;   none
; returns:
;   X and Z_PLYR_LOC_TYPE - row of location, or $FF if error
;   Z_PLYR_LOC_TY_ADDR_LOW / HIGH pointing at row start, or last row if error
;   plot_xy:    A, X, Y, c, plot variables
determine_location_type
  ; first get character at player location
  lda Z_PLYR_POS_X                  ; store player pos x in plot x
  sta Z_SCR_X
  lda Z_PLYR_POS_Y                  ; store player pos y in plot y
  sta Z_SCR_Y
  jsr plot_set_xy                   ; move to player pos
  ldy #$00                          ; zero Y, for indirect mem access
  lda (Z_SCR_LOW_BYTE), Y           ; get character at player post on map
  sta Z_TEMP_1                      ; store char in temp storage 1
  ; set up table scanning method
  lda #>data_front_facing_info      ; load high byte (page) of front facing info table
  sta Z_PLYR_LOC_TY_ADDR_HIGH       ; store in high byte of player location type addr
  lda #<data_front_facing_info      ; load low byte of table
  sta Z_PLYR_LOC_TY_ADDR_LOW        ; store in low byte of player location type addr
  ldx #$00                          ; zero X, will be used to count row
  ldy Z_PLYR_FACING
det_loc_type_loop
  lda (Z_PLYR_LOC_TY_ADDR_LOW), Y   ; get character for this facing direction (Y) and row (with addr)
  cmp Z_TEMP_1
  beq det_loc_type_finish           ; found, finish
  inx                               ; otherwise X++, row to next row
  cpx #$07                          ; check if row is on 8th row, i.e. gone past end
  beq det_loc_type_row_err          ; if so, go to error state, exit
  lda Z_PLYR_LOC_TY_ADDR_LOW        ; get low byte of current addr 1 ptr
  clc                               ; clear carry flag before addition
  adc #$08                          ; add 8 to low byte count, i.e. next row (assumes does not cross page)
  sta Z_PLYR_LOC_TY_ADDR_LOW        ; store updated low byte address
  jmp det_loc_type_loop
det_loc_type_row_err
  ldx #$FF                          ; set row to error code
  ; TODO : remove this error handing, debug only, make fatal
  lda #$53    ;heart char
  jsr fill_screen_chars
  lda #$02
  jsr fill_screen_cols
  jmp *
det_loc_type_finish
  stx Z_PLYR_LOC_TYPE
  rts

;==========================================================
; ROUTINES - LOW LEVEL
;==========================================================

; these routines are general and form a bespoke standard library


; === wait_for_key
;   loop continuously until key pressed
; params:
;   none
; uses:
;   A
; side effects:
;   none
; returns:
;   A - key code (ASCII) of key pressed
wait_for_key
  jsr FN_GETIN          ; use kernal GETIN function, get key input if any
  cmp #$00              ; check if no (null) input
  beq wait_for_key      ; if no input, continue to wait
  rts                   ; otherwise return

; === fill_screen_chars
;   fill entire screen with single character
; params:
;   A - character to fill screen with
; uses:
;   A, X, ADDR 1 H / L, ADDR 2 H / L
; side effects:
;   fill_mem:   A, X, Y
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
;   A, X, ADDR 1 H / L, ADDR 2 H / L
; side effects:
;   fill_mem:   A, X, Y
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
;   A, X, Y
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
;   A, Y
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


; === draw_chars_list_with_col
;   draw a chars list (e.g. front sized char list AKA string) on screen at current screen plot pos, copying contents from memory.
;   also writes a colour over character positions in col map.
;   DOES NOT change plot positions
; params:
;   !!! plot_set_xy should already have been called to set up plot location
;   Z_ADDR_1_HIGH / LOW - memory location of char list / string
;   X - colour value to write over chars in col map
; uses:
;   A, X, Y
;   Z_TEMP_1
; side effects:
;   modifies Z_ADDR_1_LOW to +1, make it easy to keep track of plotting position
; returns:
;   Y - size of char list written
draw_chars_list_with_col
  ldy #$00                  ; zero Y register, for indirect indexing
draw_chars_list_wc_get_len
  lda (Z_ADDR_1_LOW), Y     ; get length of char list
  sta Z_TEMP_1              ; store char list len in temp zero page storage
  inc Z_ADDR_1_LOW          ; increase low byte position to point at first element in list
draw_chars_list_wc_next
  lda (Z_ADDR_1_LOW), Y     ; get next character from memory to A
  sta (Z_SCR_LOW_BYTE), Y   ; store character in screen mem
  txa                       ; get colour from X -> A
  sta (Z_COL_LOW_BYTE), Y   ; store character in screen mem
  iny                       ; Y++
  cpy Z_TEMP_1              ; compare Y with char list len
  bne draw_chars_list_wc_next  ; if not equal, continue with next char
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
;   A, X, Y, c
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
; uses:
;   A, c
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
; assumptions:
;   list must start with a byte which is the size of data list (i.e. not including that len byte)
; params:
;   A - byte to match
;   X - list addr low byte
;   Y - list addr high byte
; uses:
;   A, X, Y
;   Z_BYTE_MATCH_HIGH / LOW
;   Z_BYTE_MATCH_BYTE
;   Z_BYTE_MATCH_LIST_SIZE
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
  inc Z_BYTE_MATCH_LOW                  ; increase low byte ptr to offset for start of data
match_byte_from_list_loop
  lda (Z_BYTE_MATCH_LOW), Y             ; get next item of list to A
  cmp Z_BYTE_MATCH_BYTE                 ; compare list item with match char (in temp storage 2)
  beq match_byte_from_list_found        ; if match, goto found condition
  cpy Z_BYTE_MATCH_LIST_SIZE            ; check if reached end of list by comparing ptr index
  beq match_byte_from_list_no_match     ; if not yet reached then loop
  iny                                   ; Y++, next offset
  jmp match_byte_from_list_loop         ; continue searching for match
match_byte_from_list_no_match
  lda #$FF                              ; set no match code
  jmp match_byte_from_list_finish
match_byte_from_list_found
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
; len 14, then 14 chars of data
!byte $0E
!byte $14,$40,$42,$43,$5B,$6B,$6D,$6E,$70,$71,$72,$73,$7D
; note, $42 is visually identical to $5D, we prefer to use $42, be careful with this

data_key_codes_to_facing_dir
!byte $04
;     Up  Rt  Lft Dwn
;     'W' 'D' 'A' 'S'
!byte $57,$44,$41,$53
;!byte $AE,$AC,$B0,$53

; TODO : Dwn (back) is not allowed, should enable this when supported
data_front_facing_info
;     -----chars----- -----exits-----
;     Up  Rt  Lft Dwn Up  Rt  Lft Dwn
!byte $42,$40,$40,$42,$00,$FF,$FF,$FF     ; front only
!byte $6B,$72,$71,$73,$00,$00,$FF,$FF     ; front and right
!byte $73,$71,$72,$6B,$00,$FF,$00,$FF     ; front and left
!byte $70,$6E,$6D,$7D,$FF,$00,$FF,$FF     ; right only
!byte $6E,$7D,$70,$6D,$FF,$FF,$00,$FF     ; left only
!byte $5B,$5B,$5B,$5B,$00,$00,$00,$FF     ; front, left and right
!byte $72,$73,$6B,$71,$FF,$00,$00,$FF     ; left and right

data_facing_matrix
;     Up  Rt  Lft Dwn
!byte $00,$01,$02,$03 ; Up ->
!byte $01,$03,$00,$02 ; Right ->
!byte $02,$00,$03,$01 ; Left ->
!byte $03,$02,$01,$00 ; Down ->

data_x_movement_facing_dir
;     Up  Rt  Lft Dwn
!byte $01,$02,$00,$01
data_y_movement_facing_dir
;     Up  Rt  Lft Dwn
!byte $00,$01,$01,$02

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
!byte $0C
!scr "facing east "
data_str_pg1_facing_west
!byte $0C
!scr "facing west "

;==========================================================
; IMAGE DATA
;==========================================================

* = $6000

data_scr_map
!byte 112,113,110,109,115,112,64,64,64,114,110,112,64,64,110,109,110,112,64,64,64,125,109,64,115,112,113,110,112,113,110,112,67,67,67,110,112,114,110,66
!byte 109,110,66,112,91,125,112,64,114,113,115,109,64,110,107,64,91,125,111,111,111,111,111,111,109,115,112,125,66,112,115,66,112,64,114,113,125,66,66,66
!byte 114,125,109,115,109,110,107,64,91,114,113,64,64,115,66,112,115,106,160,160,160,160,160,160,116,66,109,114,115,107,91,113,115,112,125,112,110,66,107,125
!byte 66,112,110,107,64,113,125,112,115,109,110,112,64,115,107,125,66,106,160,160,160,160,160,160,116,109,114,125,66,107,91,110,66,66,112,113,91,113,91,110
!byte 109,125,109,115,112,64,110,66,66,112,125,109,110,107,125,112,125,106,160,160,160,160,160,160,116,112,113,110,66,109,91,91,115,109,91,64,115,112,125,66
!byte 64,64,114,125,109,110,109,125,66,107,114,64,115,107,110,109,110,106,160,160,160,160,160,160,116,107,64,125,66,112,115,66,107,110,107,110,66,109,114,125
!byte 112,110,66,112,110,107,64,110,109,113,115,112,125,66,109,64,115,106,160,160,160,160,160,160,116,107,114,64,91,91,91,125,66,109,115,109,113,114,91,110
!byte 66,109,113,115,107,125,112,115,112,64,113,107,64,113,64,64,125,106,160,160,160,160,160,160,116,109,113,64,125,109,91,110,107,64,91,64,64,115,66,66
!byte 109,114,64,125,109,114,115,109,115,112,64,125,111,111,111,111,111,122,160,160,160,160,160,160,76,111,111,111,111,111,109,115,107,110,109,110,112,113,125,66
!byte 64,115,112,64,110,109,91,110,66,109,110,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,66,112,125,107,64,64,125
!byte 112,91,113,110,109,114,115,109,91,110,66,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,110,109,115,112,113,110,112,114
!byte 66,107,64,113,114,125,109,110,109,115,66,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,91,64,113,91,64,91,125,66
!byte 66,109,114,110,109,64,110,66,112,125,66,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,112,64,125,112,113,110,66
!byte 109,114,115,109,64,64,125,109,113,114,115,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,64,115,112,114,113,110,107,125
!byte 112,125,107,64,64,64,114,64,64,113,125,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,66,112,115,109,115,112,113,91,110
!byte 109,110,66,112,64,110,109,64,64,114,110,106,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,116,107,125,107,110,107,125,112,113,125
!byte 112,115,109,125,112,113,64,114,64,91,125,106,160,160,160,160,160,160,79,20,119,119,119,80,160,160,160,160,160,160,116,109,114,115,107,91,110,66,112,110
!byte 109,113,64,110,66,112,64,113,110,107,110,106,160,160,160,160,160,160,116,66,112,114,110,106,160,160,160,160,160,160,116,112,125,66,66,107,91,113,125,66
!byte 64,64,64,91,113,91,110,112,125,66,66,106,160,160,160,160,160,160,116,107,125,107,115,106,160,160,160,160,160,160,116,109,110,66,107,115,109,64,64,113
!byte 112,114,110,109,110,107,125,109,110,109,115,106,160,160,160,160,160,160,116,109,110,66,66,106,160,160,160,160,160,160,116,112,125,109,125,109,114,114,114,64
!byte 125,66,66,112,91,113,64,114,113,64,115,106,160,160,160,160,160,160,116,112,91,113,125,106,160,160,160,160,160,160,116,107,64,64,64,64,115,66,66,112
!byte 112,125,109,91,125,112,64,113,64,64,115,106,160,160,160,160,160,160,116,66,107,114,110,106,160,160,160,160,160,160,116,107,110,112,114,64,113,115,66,66
!byte 107,110,112,113,64,115,112,64,64,110,109,110,119,119,119,119,119,119,112,91,125,66,107,110,119,119,119,119,119,119,112,125,66,107,115,112,110,107,91,115
!byte 107,91,125,112,110,66,66,112,64,125,112,115,112,64,110,112,114,64,115,107,110,109,91,91,114,110,112,64,114,110,107,114,91,113,125,107,91,125,107,125
!byte 125,109,114,125,109,125,66,109,64,64,125,109,125,112,113,125,109,110,109,125,109,64,125,109,113,113,125,112,125,109,125,66,109,64,64,125,109,64,125,112