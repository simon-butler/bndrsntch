;==========================================================
; BNDRSNTCH
;
; MAIN source file, build entry point.
;
;==========================================================

;==========================================================
; MEMORY MAP
;
; Video bank 1 $4000 - $7FFF is used for first person mode
; Video bank 2 $8000 - $BFFF is used for map mode
;
; zero page
;   $0000 - $0001   : reserved
;   $0002 - $007F   : general purpose
;   $0080 - $00FF   : transferred tables
; $1000 - $2000     : main code and routines
; $3000 - $3200     : game logic tables and data, and strings
; $4400 - $47FF     : screen for first person mode (bank 1)
; $5000 - $57FF     : character map for first person (bank 1) written directly by loader
; $7000 - $72FF     ; first person drawing data tables
; $8400 - $87FF     : screen for map mode (bank 2) written to directly by loader
; 
;==========================================================

;==========================================================
; SYSTEM AND KERNAL ROUTINES 
;==========================================================

FN_GETIN                = $FFE4     ; get character from buffer (keyboard buffer unless changed)

;==========================================================
; VIC MEM LOCATIONS
;==========================================================

VIC_BG_COL              = $D021
VIC_BORDER_COL          = $D020

VIC_CIA2_BANK_SELECT    = $DD00     ; bits 0, 1 are system memroy bank select (default 11b)
VIC_CIA2_DATA_DIR_A     = $DD02     ; set bits 0, 1 to enable bank switching

;==========================================================
; PRE-PROCESSED CONSTANTS
;==========================================================

; IMPORTANT!!! don't forget to use # before using the constants,
;         or they will be probably treated as zero page addresses

CN_VID_BANK_0           = $03
CN_VID_BANK_1           = $02
CN_VID_BANK_2           = $01
CN_VID_BANK_3           = $00

; video bank 0
CN_BK0_SCR_START_LOW    = $00
CN_BK0_SCR_START_HIGH   = $04
CN_BK0_SCR_END_LOW      = $E8
CN_BK0_SCR_END_HIGH     = $07

CN_BK0_CHMAP_START_LOW  = $00
CN_BK0_CHMAP_START_HIGH = $C0
CN_BK0_CHMAP_END_LOW    = $FF
CN_BK0_CHMAP_END_HIGH   = $CF

; video bank 1
CN_BK1_SCR_START_LOW    = $00
CN_BK1_SCR_START_HIGH   = $44
CN_BK1_SCR_END_LOW      = $E8
CN_BK1_SCR_END_HIGH     = $47

CN_BK1_SCR_MIDLINE_LOW  = $08   ; midline is up to and including the first 13 lines
CN_BK1_SCR_MIDLINE_HIGH = $46

CN_BK1_CHMAP_START_LOW  = $00
CN_BK1_CHMAP_START_HIGH = $50
CN_BK1_CHMAP_END_LOW    = $FF
CN_BK1_CHMAP_END_HIGH   = $5F

; video bank 2
CN_BK2_SCR_START_LOW    = $00
CN_BK2_SCR_START_HIGH   = $84
CN_BK2_SCR_END_LOW      = $E8
CN_BK2_SCR_END_HIGH     = $87

; video bank 3
CN_BK3_SCR_START_LOW    = $00   ; bank 3 not used, just here for helper generalisation completeness
CN_BK3_SCR_START_HIGH   = $C4
CN_BK3_SCR_END_LOW      = $E8
CN_BK3_SCR_END_HIGH     = $C7

CN_COL_MEM_START_LOW    = $00
CN_COL_MEM_START_HIGH   = $D8
CN_COL_MEM_MIDLINE_LOW  = $08   ; midline is up to and including the first 13 lines
CN_COL_MEM_MIDLINE_HIGH = $DA
CN_COL_MEM_END_LOW      = $E8
CN_COL_MEM_END_HIGH     = $DB

; PESCII characters
CN_CHAR_SPACE           = $20
CN_CHAR_BLOCK           = $A0

; special characters, used in video bank 1 char set
CN_SPEC_CHAR_BLANK      = $00
CN_SPEC_CHAR_BLOCK      = $01

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
Z_SCR_START_LOW   = $08   ; this is set by the set_video_bank function, can be overridden
Z_SCR_START_HIGH  = $09

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
Z_TEMP_5          = $1C

Z_PSH_REG_A       = $1D
Z_PSH_REG_X       = $1E
Z_PSH_REG_Y       = $1F

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
Z_PLYR_LOC_TYPE         = $43     ; location type, i.e. the index in the types table, see data_front_facing_info
Z_PLYR_LOC_TY_ADDR_LOW  = $44     ; low byte of addr of start of row of current type index
Z_PLYR_LOC_TY_ADDR_HIGH = $45     ; high byte ^ same
Z_PLYR_LOOK_AHEAD_TYPE  = $46     ; loc type of loc we can look ahead to (front path must be open in cur loc)
Z_PLYR_LOOK_AHEAD_TY_ADDR_LOW   = $47   ; low byte of addr of start of row of look ahead type index
Z_PLYR_LOOK_AHEAD_TY_ADDR_HIGH  = $48   ; high byte ^ same
Z_PLYR_LOOK_AHEAD_X     = $49     ; x pos of look ahead to next location, $FF if wall ahead (convinence)
Z_PLYR_LOOK_AHEAD_Y     = $4A     ; y pos of ^ same

; lookup tables
Z_TABLE_START           = $80     ; not yet used, but possible idea


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

start_game
  jsr init_helper_vars
  jsr enable_video_bank_selection       ; enable video bank selection, only needs to be done once
start_init_player_data
  lda #$07                              ; player X pos = 07
  sta Z_PLYR_POS_X
  lda #$14                              ; player y pos = 20 ($14)
  sta Z_PLYR_POS_Y
  lda #$00                              ; player facing direction = UP (0)
  sta Z_PLYR_FACING
start_enter_screen
  jmp start_first_person
  ; NOTE - in this version the map is unreachable
;*** map mode
start_map
  lda #CN_VID_BANK_2                    ; load code for video bank 2
  jsr set_video_bank                    ; set video bank to 2
  lda #CN_COL_VAL_D_GREY                ; set border colour to dark grey
  sta VIC_BORDER_COL
  lda #CN_COL_VAL_BLACK                 ; set background colour to black
  sta VIC_BG_COL
  ;lda #CN_COL_VAL_D_GREY               ; fill screen with GREY colour, for test purposes
  jsr fill_screen_cols                  ; fill screen with black colour, black characters on black, i.e. nothing visible
  ldx #>data_scr_map                    ; put high byte (page) of screen map data in X, setup for copy_map_chars_to_screen
  ;jsr copy_map_chars_to_screen
map_loop_update_player
  jsr determine_location_type           ; update player location type pointers to data table
  jsr update_player_look_ahead          ; update same for look ahead
map_loop_show_plyr_on_map
  jsr draw_std_player_on_map            ; draw player on map in standard colours
  jsr write_player_facing_str           ; write "FACING XXXX" on map, where "XXXX" is direction (north, east, etc)
map_loop_move
  jsr wait_for_key                      ; wait for a key from user
  ldx #<data_key_codes_to_facing_dir    ; set up LOW / HIGH addr of key to facing direction mapping
  ldy #>data_key_codes_to_facing_dir
  jsr match_byte_from_list              ; try to match key to facing direction
  cmp #$FF                              ; check if match failed
  beq map_loop_move                     ; no match, continue getting key : TODO show bad input message
  pha                                   ; save A, direction to face
  jsr draw_dim_player_on_map            ; dim down (uses grey, not completely black) current player (and look ahead) pos on map
  pla                                   ; restore A, has direction to face
  jsr move_player_in_dir                ; try to move player in direction specified
  cmp #$FF                              ; check if move failed
  beq map_loop_show_plyr_on_map         ; move did fail, redraw current position as latest position
  jmp map_loop_update_player            ; otherwise update from new move position, new draw and pass to main loop
;*** first person mode
start_first_person
  lda #CN_VID_BANK_1                    ; load code for video bank 1
  jsr set_video_bank                    ; set video bank to 1
  lda #CN_COL_VAL_BLACK                 ; set border col to black
  sta VIC_BORDER_COL
  lda #CN_COL_VAL_L_BLUE                ; set background col to light blue
  sta VIC_BG_COL
first_pers_draw_scr
  jsr draw_two_tone_bg                  ; draw two tone bg (stylistic basis)
  ;jsr special_char_test                 ; character test of all special characters for bank 1
  ;jsr wait_for_key
  jsr draw_left_long_corridor           ; test drawing long corridor
unreachable_infinite_loop
  jmp *


;==========================================================
; ROUTINES - HIGH LEVEL
;==========================================================

; these routines perform some common game particular function,
; mostly using lower level functions

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


; === update_player_look_ahead
;   updates the look ahead vars based on the current player position and location type
; params:
;   none
; uses:
;   A, Y
;   Z_TEMP_1
;   Z_PLYR_LOC_TY_ADDR_LOW / HIGH
; side effects:
;   Z_PLYR_LOOK_AHEAD_TYPE
;   Z_PLYR_LOOK_AHEAD_X / Y
; returns:
;   none
update_player_look_ahead
  ldy #$04                          ; set offset to facing direction part of location type row bytes, firsr one is UP, i.e. forward
  lda (Z_PLYR_LOC_TY_ADDR_LOW), Y   ; get facing direction byte code ($00 = can move, $FF = can't move)
  bne upd_plyr_look_ahead_off       ; if not $00, can't go or see forward as is a wall, set look ahead to off
  ; set look ahead x pos
  lda Z_PLYR_POS_X                  ; read current player x position to add to 
  sta Z_PLYR_LOOK_AHEAD_X           ; store in look ahead x
  lda #>data_x_movement_facing_dir  ; put high byte (page) of data x movement for new direction in addr 1
  sta Z_ADDR_1_HIGH
  lda #<data_x_movement_facing_dir  ; same for low byte
  sta Z_ADDR_1_LOW
  ldy Z_PLYR_FACING                 ; load new (and now current) player facing direction to Y for offset
  lda (Z_ADDR_1_LOW), Y             ; get amount to move in x direction
  sta Z_TEMP_1                      ; temp store 2 amount
  dec Z_PLYR_LOOK_AHEAD_X           ; amount needs to be offset by -1 by dec cur value, not using negative numbers
  lda Z_PLYR_LOOK_AHEAD_X           ; get position
  clc                               ; clear carry flag before addition
  adc Z_TEMP_1                      ; add amount to move x position to x position
  sta Z_PLYR_LOOK_AHEAD_X           ; store updated position back in look ahead x
  ; set look ahead y pos
  lda Z_PLYR_POS_Y                  ; read current player y position to add to 
  sta Z_PLYR_LOOK_AHEAD_Y           ; store in look ahead y
  lda #>data_y_movement_facing_dir  ; put high byte (page) of data y movement for new direction in addr 1
  sta Z_ADDR_1_HIGH
  lda #<data_y_movement_facing_dir  ; same for low byte
  sta Z_ADDR_1_LOW
  lda (Z_ADDR_1_LOW), Y             ; get amount to move in y direction (Y is still correct from y position update)
  sta Z_TEMP_1                      ; temp store 2 amount
  dec Z_PLYR_LOOK_AHEAD_Y           ; amount needs to be offset by -1 by dec cur value, not using negative numbers
  lda Z_PLYR_LOOK_AHEAD_Y           ; get position
  clc                               ; clear carry flag before addition
  adc Z_TEMP_1                      ; add amount to move y position to y position
  sta Z_PLYR_LOOK_AHEAD_Y           ; store updated position back in look ahead y
  ; TODO : read type in look ahead map location, and set low / high addr
  lda #$FF
  sta Z_PLYR_LOOK_AHEAD_TYPE
  sta Z_PLYR_LOOK_AHEAD_TY_ADDR_LOW
  sta Z_PLYR_LOOK_AHEAD_TY_ADDR_HIGH
  jmp upd_plyr_look_ahead_finish
upd_plyr_look_ahead_off
  lda #$FF
  sta Z_PLYR_LOOK_AHEAD_TYPE
  sta Z_PLYR_LOOK_AHEAD_TY_ADDR_LOW
  sta Z_PLYR_LOOK_AHEAD_TY_ADDR_HIGH
  sta Z_PLYR_LOOK_AHEAD_X
  sta Z_PLYR_LOOK_AHEAD_Y
upd_plyr_look_ahead_finish
  rts


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
  lda (Z_ADDR_1_LOW), Y             ; get amount to move in y direction (Y is still correct from y position update)
  sta Z_TEMP_2                      ; temp store 2 amount
  dec Z_PLYR_POS_Y                  ; amount needs to be offset by -1 by dec cur value, not using negative numbers
  lda Z_PLYR_POS_Y                  ; get position
  clc                               ; clear carry flag before addition
  adc Z_TEMP_2                      ; add amount to move x position to y position
  sta Z_PLYR_POS_Y                  ; store updated position back in player pos y
  lda Z_PLYR_FACING                 ; put updated / current player facing direction in A, is indication of success on return
  jmp mv_plyr_in_dir_finish
mv_plyr_in_dir_failed
  lda Z_TEMP_1
mv_plyr_in_dir_finish
  rts


;==========================================================
; ROUTINES - MAP DRAWING
;==========================================================

; === draw_std_player_on_map
;   wrapper for normal draw colours of draw_player_map_pos
; params:
;   none
; uses:
;   X, Y
; side effects:
;   draw_player_map_pos: A, X, Y, Z_TEMP_1, Z_TEMP_2, all player core and some helper data
; returns:
;   none
draw_std_player_on_map
  ldx #CN_COL_VAL_WHITE
  ldy #CN_COL_VAL_L_GREY
  jsr draw_player_map_pos
  rts


; === draw_dim_player_on_map
;   wrapper for normal draw colours of draw_player_map_pos
; params:
;   none
; uses:
;   X, Y
; side effects:
;   draw_player_map_pos: A, X, Y, Z_TEMP_1, Z_TEMP_2, all player core and some helper data
; returns:
;   none
draw_dim_player_on_map
  ldx #CN_COL_VAL_D_GREY
  ldy #CN_COL_VAL_D_GREY
  jsr draw_player_map_pos
  rts


; === draw_player_map_pos
;   draw (with colour only) player position (and look ahead pos if exists) on map on screen
; params:
;   X - player loc colour
;   Y - look ahead loc colour
; uses:
;   A, X, Y
;   Z_TEMP_1, Z_TEMP_2
;   all player core and some helper data
; side effects:
;   none
; returns:
;   none
draw_player_map_pos
  stx Z_TEMP_1                ; store player loc colour from X -> temp 1
  sty Z_TEMP_2                ; store look ahead loc colour from Y -> temp 2
  ldy #$00                    ; set Y to zero, used in indirect addressing
  lda Z_PLYR_POS_X            ; load player position x
  sta Z_SCR_X                 ; store in screen plot x
  lda Z_PLYR_POS_Y            ; load player position y
  sta Z_SCR_Y                 ; store in screen plot y
  jsr plot_set_xy             ; update plot variables based on inputed (x,y)
  lda Z_TEMP_1                ; load player loc colour
  sta (Z_COL_LOW_BYTE), Y     ; store in player pos
  lda Z_PLYR_LOOK_AHEAD_X     ; load look ahead x pos
  cmp #$FF                    ; compare with disabled code
  beq draw_plyr_mp_pos_finish ; if is disabled, finish, don't draw look ahead
  sta Z_SCR_X                 ; otherwise store in screen plot x
  lda Z_PLYR_LOOK_AHEAD_Y     ; load look ahead y
  sta Z_SCR_Y                 ; store in screen plot y
  jsr plot_set_xy             ; update plot variables based on inputed (x,y)
  lda Z_TEMP_2                ; load look ahead loc colour
  sta (Z_COL_LOW_BYTE), Y     ; store in look ahead pos
draw_plyr_mp_pos_finish
  rts


; === draw_darken_look_ahead
;   darken the player look ahead drawing location, use for when no longer looking at it
; params:
;   none
; uses:
;   A, Y
;   all player core and some helper data
; side effects:
;   none
; returns:
;   none
draw_darken_look_ahead
  ldy #$00                    ; set Y to zero, used in indirect addressing
  lda Z_PLYR_LOOK_AHEAD_X     ; load look ahead x pos
  cmp #$FF                    ; compare with disabled code
  beq draw_drk_lk_ahd_finish  ; if is disabled, finish, don't draw look ahead
  sta Z_SCR_X                 ; otherwise store in screen plot x
  lda Z_PLYR_LOOK_AHEAD_Y     ; load look ahead y
  sta Z_SCR_Y                 ; store in screen plot y
  jsr plot_set_xy             ; update plot variables based on inputed (x,y)
  lda #CN_COL_VAL_M_GREY      ; load medium grey
  sta (Z_COL_LOW_BYTE), Y     ; store medium grey in look ahead pos
draw_drk_lk_ahd_finish
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


;==========================================================
; ROUTINES - FIRST PERSON DRAWING
;==========================================================

; === draw_two_tone_bg
draw_two_tone_bg
  ; fill entire colour memory (foreground) with black
  lda #CN_COL_MEM_START_LOW
  sta Z_ADDR_1_LOW
  lda #CN_COL_MEM_START_HIGH
  sta Z_ADDR_1_HIGH
  lda #CN_COL_MEM_END_LOW
  sta Z_ADDR_2_LOW
  lda #CN_COL_MEM_END_HIGH
  sta Z_ADDR_2_HIGH
  lda #CN_COL_VAL_BLACK
  jsr fill_mem
  ; fill scr mem first half with blocks (makes black)
  lda #CN_BK1_SCR_START_LOW
  sta Z_ADDR_1_LOW
  lda #CN_BK1_SCR_START_HIGH
  sta Z_ADDR_1_HIGH
  lda #CN_BK1_SCR_MIDLINE_LOW
  sta Z_ADDR_2_LOW
  lda #CN_BK1_SCR_MIDLINE_HIGH
  sta Z_ADDR_2_HIGH
  lda #CN_SPEC_CHAR_BLOCK
  jsr fill_mem
  ; fill scr mem first half with spaces (makes see through, i.e. bg col, i.e. light blue)
  lda #CN_BK1_SCR_MIDLINE_LOW
  sta Z_ADDR_1_LOW
  lda #CN_BK1_SCR_MIDLINE_HIGH
  sta Z_ADDR_1_HIGH
  lda #CN_BK1_SCR_END_LOW
  sta Z_ADDR_2_LOW
  lda #CN_BK1_SCR_END_HIGH
  sta Z_ADDR_2_HIGH
  lda #CN_SPEC_CHAR_BLANK
  jsr fill_mem
  rts


; === special_char_test
special_char_test
  lda #CN_BK1_SCR_START_LOW
  sta Z_ADDR_1_LOW
  lda #CN_BK1_SCR_START_HIGH
  sta Z_ADDR_1_HIGH
  ldy #$00
special_char_test_loop
  tya
  sta (Z_ADDR_1_LOW), Y
  iny
  cpy #$13
  bne special_char_test_loop
  rts


; === draw_left_long_corridor
;   draw long left corridor
draw_left_long_corridor
  lda #<data_corridor_right_exit_near_and_far_left
  sta Z_ADDR_1_LOW
  lda #>data_corridor_right_exit_near_and_far_left
  sta Z_ADDR_1_HIGH
  jsr draw_from_tables
  ;
  lda #<data_corridor_right_exit_near_and_far_right
  sta Z_ADDR_1_LOW
  lda #>data_corridor_right_exit_near_and_far_right
  sta Z_ADDR_1_HIGH
  jsr draw_from_tables
  rts


; === draw_from_tables
;   draw from specific draw table formatted table
; assumptions:
;   table follows format, see DRAW INSTRUCTIONS data section notes
; params:
;   Z_ADDR_1_LOW / HIGH - set to table start
; uses:
;   A, X, Y
;   Z_TEMP_1 / 2 / 3 / 4
;   Z_ADDR_1_LOW / HIGH
;   Z_ADDR_2_LOW / HIGH
;   Z_ADDR_3_LOW / HIGH
; side effects:
;   plot_set_xy: A, X, Y, plot variables
; returns:
;   none
draw_from_tables
draw_from_tab_section
  ldy #$00                    ; zero Y for indirect addressing
  lda (Z_ADDR_1_LOW), Y       ; get draw character from table
  beq draw_from_tab_finish    ; if A == $00 then finished
  sta Z_TEMP_1                ; store draw character in temp 1
  iny                         ; Y++
  lda (Z_ADDR_1_LOW), Y       ; get character colour from table
  sta Z_TEMP_2                ; store col in temp 2
  iny                         ; Y++
  lda (Z_ADDR_1_LOW), Y       ; get data length from table, store in X
  sta Z_TEMP_3                ; store total number of chars to write in temp 3
  iny                         ; Y++
  ; set pointers to first character
  lda (Z_ADDR_1_LOW), Y       ; get character colour from table
  sta Z_SCR_X                 ; store x from table in scr x var
  iny                         ; Y++
  lda (Z_ADDR_1_LOW), Y       ; get character colour from table
  sta Z_SCR_Y                 ; store y from table in scr y var
  jsr plot_set_xy             ; update screen and col map pointing addresses
  lda Z_SCR_LOW_BYTE          ; copy screen ptr low/high addr to temp addr 2
  sta Z_ADDR_2_LOW
  lda Z_SCR_HI_BYTE
  sta Z_ADDR_2_HIGH
  lda Z_COL_LOW_BYTE          ; copy col map ptr low/high addr to temp addr 3
  sta Z_ADDR_3_LOW
  lda Z_COL_HI_BYTE
  sta Z_ADDR_3_HIGH
  ; update addr 1, tracking on table
  lda Z_ADDR_1_LOW            ; get low address, will add head so points at byte offsets
  clc                         ; clear carry flag before addition
  adc #$05                    ; add $05 to low byte, to move to past section header
  sta Z_ADDR_1_LOW            ; store result (doesn't affect carry flag)
  bcc draw_from_tab_skip      ; if did not cross page boundary, continue to loop setup
  inc Z_ADDR_1_HIGH           ; otherwise add one to high byte (page)
draw_from_tab_skip
  ldy #$00                    ; zero Y, tracks offset in table
  sty Z_TEMP_4                ; store in temp 4
draw_from_tab_loop
  ldy #$00                    ; zero Y for indirect write, offset not used here
  lda Z_TEMP_1                ; load draw char
  sta (Z_ADDR_2_LOW), Y       ; store char in screen
  lda Z_TEMP_2                ; load col
  sta (Z_ADDR_3_LOW), Y       ; store col in col mem
  ldy Z_TEMP_4                ; restore table offset in Y
  cpy Z_TEMP_3                ; check if counter has reached end of offsets (will be $00 if no offsets)
  beq draw_from_tab_sec_end   ; finished seciton, check if one next
  lda (Z_ADDR_1_LOW), Y       ; get next draw loc offset
  iny                         ; increase pointer to next table offset byte
  sty Z_TEMP_4                ; store next table offset byte offset in temp 4
  clc                         ; clear carry flag before addition
  adc Z_ADDR_2_LOW            ; add address low byte to offset
  sta Z_ADDR_2_LOW            ; store result in addr 2 low byte (char scr) doesn't affect carry flag
  sta Z_ADDR_3_LOW            ; store in addr 3 (col map), tracks the same on low byte
  bcc draw_from_tab_loop      ; if didn't cross page boundary, continue
  inc Z_ADDR_2_HIGH           ; otherwise next char scr page
  inc Z_ADDR_3_HIGH           ; same, next col mem page
  jmp draw_from_tab_loop      ; otherwise get next (x,y) pair (Y already on next read position)
draw_from_tab_sec_end
  tya                         ; Y -> A, has number of offset bytes here
  clc                         ; clear carry flag before addition
  adc Z_ADDR_1_LOW            ; add offset bytes to low byte of base table pointer addr (1)
  sta Z_ADDR_1_LOW            ; save result of add in addr 1 low, note does not affect carry flag checked in next operation
  bcc draw_from_tab_section   ; if didn't cross page boundary, continue to check for next section
  inc Z_ADDR_1_HIGH           ; otherwise add 1 to page (high byte)
  jmp draw_from_tab_section   ; then continue to check for next section
draw_from_tab_finish
  rts





;==========================================================
; ROUTINES - LOW LEVEL
;==========================================================

; these routines are general and form a bespoke standard library

; === init_helper_vars
;   initialise some variables used by helper routines
; params:
;   none
; uses:
;   A
init_helper_vars
  lda #CN_BK0_SCR_START_LOW
  sta Z_SCR_START_LOW
  lda #CN_BK0_SCR_START_HIGH
  sta Z_SCR_START_HIGH
  rts

; === enable_video_bank_selection
;   enables video bank selection in VIC chip
; uses:
;   A
enable_video_bank_selection
  lda VIC_CIA2_DATA_DIR_A       ; read value of data direction A of CIA#2
  ora #$03                      ; set bits 0 and 1 on
  sta VIC_CIA2_DATA_DIR_A       ; write with video bank selection enabled
  rts

; === set_video_bank
;   set the video bank the VIC is pointing to
; params:
;   A - bank to select (must be number between $00 and $03 inclusive)
; uses:
;   A, Z_TEMP_1
; side effects:
;   none
; returns:
;   none
set_video_bank
  sta Z_TEMP_1                  ; save video bank number to temp 1
  lda VIC_CIA2_BANK_SELECT      ; read value of bank selection register
  and #$FC                      ; mask out bits 0, 1 with AND
  ora Z_TEMP_1                  ; set video bank by ORing against number saved in temp 1
  sta VIC_CIA2_BANK_SELECT      ; write to video bank selection reg
  lda Z_TEMP_1                  ; load video bank number to compare so screen start can be updated
  cmp #CN_VID_BANK_1            ; check if bank 1
  beq set_video_bank_1
  cmp #CN_VID_BANK_2            ; check if bank 2
  beq set_video_bank_2
  cmp #CN_VID_BANK_3            ; check if bank 3
  beq set_video_bank_3
set_video_bank_0                ; otherwise continue to set for bank 0
  lda #CN_BK0_SCR_START_LOW
  sta Z_SCR_START_LOW
  lda #CN_BK0_SCR_START_HIGH
  sta Z_SCR_START_HIGH
  jmp set_video_bank_finish
set_video_bank_1                ; set for bank 1
  lda #CN_BK1_SCR_START_LOW
  sta Z_SCR_START_LOW
  lda #CN_BK1_SCR_START_HIGH
  sta Z_SCR_START_HIGH
  jmp set_video_bank_finish
set_video_bank_2                ; set for bank 2
  lda #CN_BK2_SCR_START_LOW
  sta Z_SCR_START_LOW
  lda #CN_BK2_SCR_START_HIGH
  sta Z_SCR_START_HIGH
  jmp set_video_bank_finish
set_video_bank_3                ; set for bank 3
  lda #CN_BK3_SCR_START_LOW
  sta Z_SCR_START_LOW
  lda #CN_BK3_SCR_START_HIGH
  sta Z_SCR_START_HIGH
set_video_bank_finish
  rts

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
  ldx #CN_BK2_SCR_START_LOW       ; set start / end addres in low / high for fill_mem call
  stx Z_ADDR_1_LOW
  ldx #CN_BK2_SCR_START_HIGH
  stx Z_ADDR_1_HIGH
  ldx #CN_BK2_SCR_END_LOW
  stx Z_ADDR_2_LOW
  ldx #CN_BK2_SCR_END_HIGH
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
; notes:
;   this routine uses speed code and jumps to sub_fill_mem_unrolled_page if it can, it saves more than twice the cycles,
;     about 7668 cycles better (including jsr and rts, but not unrolled check)
;   you can comment out everything between the speed code tags and this will work fine without it.
; assumptions:
;   there is at least one byte to write, if addr 1 == addr 2 entirely, this routine with have undefined results
; params:
;   Z_ADDR_1_HIGH / LOW - from memory location (inclusive)
;   Z_ADDR_2_HIGH / LOW - to memory location (exclusive)
;   A - byte to fill
; uses:
;   A, X, Y
; side effects:
;   none
fill_mem
  ldy #$00                  ; zero Y register
  ; --- START SPEED CODE
  tax                       ; keep copy of byte in X
fill_mem_unrolled_check     ; this is where we check to see if we can do an entire page as an unrolled loop, saving dozens of cycles
  lda Z_ADDR_1_LOW          ; entire page must start at zero, check starting position low byte is $00
  bne fill_mem_loop         ; if not zero (zero flag auto set when loading to A) can't do unrolled loop, skip to main loop
  lda Z_ADDR_1_HIGH         ; load starting high byte mem addr 1
  cmp Z_ADDR_2_HIGH         ; check against ending high byte
  beq fill_mem_loop         ; if match then not a full page to do, can't do unrolled loop, skip to main loop
  txa                       ; restore byte to fill from X -> A
  jsr sub_fill_mem_unrolled_page    ; jump to sub routine to do an entire page
  inc Z_ADDR_1_HIGH
  jmp fill_mem_unrolled_check
  ; --- END SPEED CODE
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
  lda Z_ADDR_1_LOW          ; load low byte current mem addr
  cmp Z_ADDR_2_LOW          ; check against ending high byte (only reaches here if high byte already matched)
  beq fill_mem_finish       ; if match then we're done, finish
  jmp fill_mem_loop         ; otherwise still more bytes, loop
fill_mem_finish
  rts


; === copy_map_chars_to_screen
;   map copy routine from stored memory location to screen, characters only, does not set any colour map data
; assumptions:
;   there is at least one byte to write, if addr 1 == addr 2 entirely, this routine with have undefined results
; params:
;   Z_ADDR_1_HIGH / LOW - from memory location (inclusive) to write to
;   Z_ADDR_2_HIGH / LOW - to memory location (exclusive) to write to
;   Z_ADDR_3_HIGH / LOW - start memory location (inclusive) for reading
;   A - byte to fill
; uses:
;   A, X, Y
; side effects:
;   none
copy_mem
  ldy #$00                  ; zero Y register
copy_mem_loop
  lda (Z_ADDR_3_LOW), Y     ; read byte from source
  sta (Z_ADDR_1_LOW), Y     ; store byte to fill in next address
  inc Z_ADDR_3_LOW          ; next mem addr (read from), low byte
  inc Z_ADDR_1_LOW          ; next mem addr (write to), low byte
  lda Z_ADDR_1_LOW          ; load low byte mem addr (automatically checks for zero)
  bne copy_mem_check_high   ; if not zero, go to high byte check
copy_mem_paged
  inc Z_ADDR_1_HIGH         ; increase high byte of write to addr
  inc Z_ADDR_3_HIGH         ; increase high byte of read from addr
copy_mem_check_high
  lda Z_ADDR_1_HIGH         ; load high byte mem addr
  cmp Z_ADDR_2_HIGH         ; check against ending high byte
  beq copy_mem_check_low    ; if match (we're in last page) then go to check low byte for match
  jmp copy_mem_loop         ; otherwise still more bytes, loop
copy_mem_check_low
  lda Z_ADDR_1_LOW          ; load low byte current mem addr
  cmp Z_ADDR_2_LOW          ; check against ending high byte (only reaches here if high byte already matched)
  beq copy_mem_finish       ; if match then we're done, finish
  jmp copy_mem_loop         ; otherwise still more bytes, loop
copy_mem_finish
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
  lda Z_SCR_START_HIGH                  ; load high byte of screen start in A
  sta Z_SCR_HI_BYTE                     ; store in zero page
  lda #CN_COL_MEM_START_HIGH            ; load high byte of color map start in A
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


; TODO!!! : this has not been tested
; === load_z_tables
;   load tables from memory into zero page tables for further use
; assumptions:
;   table ends with $00
; params:
;   Z_ADDR_1_LOW / HIGH - start of memory area to transfer to zero page
; uses:
;   X, Y
;   Z_TEMP_1
; side effects:
;   none
; return value:
;   none
load_z_tables
  ldy #$00                  ; zero Y, used in indirect addressing
load_z_tables_loop
  ldx Z_ADDR_1_LOW, Y     ; load next byte
  beq load_z_tables_finish  ; if $00 then finish
  stx Z_TABLE_START, Y    ; store byte in zero table
  iny                       ; Y++, next byte position
  jmp load_z_tables_loop    ; continue loop
load_z_tables_finish
  stx Z_TABLE_START, Y    ; store ending $00 in zero table
  rts


; === sub_fill_mem_unrolled_page
;   unrolled loop to fill a page of memory with a byte
;   NOTE: this routine takes up 768 bytes, nearly a full page! if running out of space it could be commented out
; assumptions:
;   Y is already set to zero
; params:
;   A - byte to store in memory
;   Z_ADDR_1_LOW / HIGH - set to starting point of memory page (doesn't technically need to be start of page at low byte $00)
; uses:
;   A, Y
;   Z_ADDR_1_LOW / HIGH
; side effects:
;   none
; returns:
;   none
sub_fill_mem_unrolled_page
  sta (Z_ADDR_1_LOW), Y     ; store byte to fill in next address
  iny                       ; $01
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $02
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $03
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $04
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $05
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $06
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $07
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $08
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $09
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $0F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $10
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $11
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $12
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $13
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $14
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $15
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $16
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $17
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $18
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $19
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $1F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $20
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $21
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $22
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $23
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $24
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $25
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $26
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $27
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $28
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $29
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $2F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $30
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $31
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $32
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $33
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $34
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $35
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $36
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $37
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $38
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $39
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $3F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $40
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $41
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $42
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $43
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $44
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $45
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $46
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $47
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $48
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $49
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $4F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $50
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $51
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $52
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $53
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $54
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $55
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $56
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $57
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $58
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $59
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $5F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $60
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $61
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $62
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $63
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $64
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $65
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $66
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $67
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $68
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $69
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $6F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $70
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $71
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $72
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $73
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $74
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $75
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $76
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $77
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $78
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $79
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $7F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $80
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $81
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $82
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $83
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $84
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $85
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $86
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $87
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $88
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $89
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $8F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $90
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $91
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $92
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $93
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $94
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $95
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $96
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $97
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $98
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $99
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9A
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9B
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9C
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9D
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9E
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $9F
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $A9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AD
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $AF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $B9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BD
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $BF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $C9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CD
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $CF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $D9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DD
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $DF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $E9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $EA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $EB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $EC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $ED
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $EE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $EF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F0
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F1
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F2
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F3
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F4
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F5
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F6
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F7
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F8
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $F9
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FA
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FB
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FC
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FD
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FE
  sta (Z_ADDR_1_LOW), Y
  iny                       ; $FF
  sta (Z_ADDR_1_LOW), Y
  iny                       ; back to $00
  rts

; this label is just here to easily see what the last address of routines is, for memory calculations
debug_label_end_of_routines   ; = $1740 in this version


;==========================================================
; TABLES
;==========================================================

; NOTE: lists always start with the length of the list (which should be +1 size of list, to include size byte),
;       followed by the values

* = $3000

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

; this label is just here to easily see what the last address of routines is, for memory calculations
debug_label_end_of_tables_strings   ; = $3097 in this version

;==========================================================
; SCREEN COPY
;==========================================================

; used directly as screen location when video bank 0 (default) selected
; this is just for a little visual glitch effect when the same starts
* = $0400
data_scr_glyph_is_a_glitch
!byte 32,32,32,12,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,7,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,15,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,14
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,1,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,160,160,160,160,160,160,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,9,32,32,32,32,32,32,32
!byte 32,32,32,32,32,32,32,32,32,32,32,32,32,4,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32

; used directly in video bank 2 (actually used in game)
* = $8400
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


;==========================================================
; CHARACTER SET COPY
;==========================================================

* = $5000

!byte $00,$00,$00,$00,$00,$00,$00,$00   ; $00 - blank
!byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF   ; $01 - block
!byte $FE,$FC,$F8,$F0,$E0,$C0,$80,$00   ; $02 - top left filled diagonal half
!byte $01,$03,$07,$0F,$1F,$3F,$7F,$FF   ; $03 - bottom right filled diagonal half
!byte $7F,$3F,$1F,$0F,$07,$03,$01,$00   ; $04 - top right filled diagonal half
!byte $80,$C0,$E0,$F0,$F8,$FC,$FE,$FF   ; $05 - bottom left filled diagonal half
!byte $FC,$F8,$F1,$E3,$C7,$8F,$1F,$3F   ; $06 - top left and bottom right filled diagonal halves
!byte $3F,$1F,$8F,$C7,$E3,$F1,$F8,$FC   ; $07 - top right and bottom left filled diagonal halves
!byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F   ; $08 - bottom left tiny block
!byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE   ; $09 - bottom right tiny block
!byte $FE,$FF,$FF,$FF,$FF,$FF,$FF,$FF   ; $0A - top right tiny block
!byte $7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF   ; $0B - top left tiny block
!byte $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC   ; $0C - left side almost block
!byte $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F   ; $0D - right side almost block
!byte $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C   ; $0E - left and right side almost block
!byte $FF,$FF,$FF,$FF,$FF,$FF,$00,$00   ; $0F - top side almost block
!byte $00,$00,$FF,$FF,$FF,$FF,$FF,$FF   ; $10 - bottom side almost block
!byte $FC,$F8,$F0,$E0,$C4,$8C,$1C,$3C   ; $11 - top right filled diagonal half and bottom right sided island
!byte $3F,$1F,$0F,$07,$23,$31,$38,$3C   ; $12 - top left filled diagonal half and bottom left sided island

;==========================================================
; DRAW INSTRUCTIONS
;==========================================================

; Draw instructions follow a special format, split into sections, and terminated by a $00
; For each section:
;   first three bytes:    character, colour, number of offset bytes ONLY
;   4th and 5th btyes:    (x,y) of starting position on screen
;   next bytes:           each byte is the byte offset to the next character to draw, e.g. $28 (40) is y+1, the next row
;
; Notes
; 1. If only one byte to draw, no next bytes.
; 2. The byte offset cannot be negative, so the draw order needs to be from top to bottom, and left to right within a row.
; 3. The byte offset cannot be greater than $FF, so if there's a larger gap it needs to be split into a separate section.

* = $7000

; double long left side corridor
data_corridor_double_long_left
; top row, main line
;    char col len
!byte $07,$00,$0A
!byte $08,$00      ; <- starting (x,y)
!byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29     ; <- offsets (in bytes) for 2nd to 11th character to write
; top row, bottom side patch up
!byte $0A,$00,$0A
!byte $07,$00
!byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29
; top row, top side patch up
!byte $08,$00,$09
!byte $09,$00
!byte $29,$29,$29,$29,$29,$29,$29,$29,$29
; wall right side
!byte $0C,$00,$01
!byte $12,$0B
!byte $28
; floor border
!byte $03,$02,$0A
!byte $12,$0E
!byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27
; floor
!byte $01,$02,$41
!byte $13,$0E
!byte $27,$01
!byte $26,$01,$01
!byte $25,$01,$01,$01
!byte $24,$01,$01,$01,$01
!byte $23,$01,$01,$01,$01,$01
!byte $22,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1E,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $13,$0D
; END, null terminated
!byte $00

; double long right side corridor
data_corridor_double_long_right
; top row, main line
;    char col len
!byte $06,$00,$0A
!byte $1F,$00
!byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27
; top row, bottom side patch up
!byte $0B,$00,$0A
!byte $20,$00
!byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27
; top row, top side patch up
!byte $09,$00,$09
!byte $1E,$00
!byte $27,$27,$27,$27,$27,$27,$27,$27,$27
; wall right side
!byte $0D,$00,$01
!byte $15,$0B
!byte $28
; floor border
!byte $05,$02,$0A
!byte $15,$0E
!byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29
; floor
!byte $01,$02,$41
!byte $14,$0E
!byte $28,$01
!byte $27,$01,$01
!byte $26,$01,$01,$01
!byte $25,$01,$01,$01,$01
!byte $24,$01,$01,$01,$01,$01
!byte $23,$01,$01,$01,$01,$01,$01
!byte $22,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $14,$0D
; END, null terminated
!byte $00

data_corridor_right_exit_near_left
; top row, main line
!byte $07,$00,$04
!byte $07,$00
!byte $29,$29,$29,$29
; top row, bottom side patch up
!byte $0A,$00,$04
!byte $06,$00
!byte $29,$29,$29,$29
; top row, top side patch up
!byte $08,$00,$03
!byte $08,$00
!byte $29,$29,$29
; top row, far top section
!byte $07,$00,$02
!byte $10,$08
!byte $29,$29
; top row, far bottom side patch up
!byte $0A,$00,$01
!byte $10,$09
!byte $29
; top row, far top side patch up
!byte $08,$00,$02
!byte $10,$07
!byte $29,$29
; left turn wall join
!byte $12,$00,$00
!byte $0F,$07
; left turn wall top
!byte $10,$00,$02
!byte $0C,$07
!byte $01,$01
; left turn wall left side
!byte $0D,$00,$04
!byte $0F,$08
!byte $28,$28,$28,$28
; left turn wall right side
!byte $0C,$00,$07
!byte $0B,$05
!byte $28,$28,$28,$28,$28,$28,$28
; wall front left side
!byte $0C,$00,$01
!byte $12,$0B
!byte $28
; left turn wall flat
!byte $01,$06,$0E
!byte $0C,$0D
!byte $01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01
; floor border far side and near side
!byte $03,$02,$07
!byte $12,$0E
!byte $27,$27,$27,$9C,$27,$27,$27
!byte $01,$02,$47
!byte $13,$0E
!byte $27,$01
!byte $26,$01,$01
!byte $25,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1E,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $13,$0D
; END, null terminated
!byte $00

data_corridor_right_exit_near_right
; top row, main line
!byte $06,$00,$04
!byte $20,$00
!byte $27,$27,$27,$27
; top row, bottom side patch up
!byte $0B,$00,$04
!byte $21,$00
!byte $27,$27,$27,$27
; top row, top side patch up
!byte $09,$00,$03
!byte $1F,$00
!byte $27,$27,$27
; top row, far top section
!byte $06,$00,$02
!byte $17,$08
!byte $27,$27
; top row, far bottom side patch up
!byte $0B,$00,$01
!byte $17,$09
!byte $27
; top row, far top side patch up
!byte $09,$00,$02
!byte $17,$07
!byte $27,$27
; right turn wall join
!byte $11,$00,$00
!byte $18,$07
; right turn wall top
!byte $10,$00,$02
!byte $19,$07
!byte $01,$01
; right turn wall left side
!byte $0C,$00,$04
!byte $18,$08
!byte $28,$28,$28,$28
; right turn wall right side
!byte $0D,$00,$07
!byte $1C,$05
!byte $28,$28,$28,$28,$28,$28,$28
; wall front right side
!byte $0D,$00,$01
!byte $15,$0B
!byte $28
; right turn wall flat
!byte $01,$06,$0E
!byte $19,$0D
!byte $01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01
; floor border far side and near side
!byte $05,$02,$07
!byte $15,$0E
!byte $29,$29,$29,$A4,$29,$29,$29
; floor
!byte $01,$02,$47
!byte $14,$0E
!byte $28,$01
!byte $27,$01,$01
!byte $26,$01,$01,$01
!byte $25,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $14,$0D
; END, null terminated
!byte $00

; ---

data_corridor_right_exit_near_and_far_left
; top row, main line
!byte $07,$00,$04
!byte $07,$00
!byte $29,$29,$29,$29
; top row, bottom side patch up
!byte $0A,$00,$04
!byte $06,$00
!byte $29,$29,$29,$29
; top row, top side patch up
!byte $08,$00,$03
!byte $08,$00
!byte $29,$29,$29
; top row, far top section
!byte $07,$00,$00
!byte $10,$08
; top row, far bottom side patch up
!byte $0A,$00,$00
!byte $10,$09
; top row, far top side patch up
!byte $08,$00,$00
!byte $10,$07

; left turn wall right side
!byte $0C,$00,$03
!byte $10,$09
!byte $28,$28,$28

; left turn wall join
!byte $12,$00,$00
!byte $0F,$07
; left turn wall top
!byte $10,$00,$02
!byte $0C,$07
!byte $01,$01
; left turn wall left side
!byte $0D,$00,$04
!byte $0F,$08
!byte $28,$28,$28,$28
; left turn wall right side
!byte $0C,$00,$07
!byte $0B,$05
!byte $28,$28,$28,$28,$28,$28,$28

; wall front left side
!byte $12,$00,$00
!byte $12,$0B
; wall front left side
!byte $10,$00,$00
!byte $11,$0B
; wall front left side
!byte $0E,$00,$00
!byte $12,$0C
; right turn wall flat
!byte $01,$06,$01
!byte $11,$0D
!byte $28

; left turn wall right side
!byte $0C,$00,$07
!byte $0B,$05
!byte $28,$28,$28,$28,$28,$28,$28

; left turn wall flat
!byte $01,$06,$0E
!byte $0C,$0D
!byte $01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01
; floor border far side and near side
!byte $03,$02,$07
!byte $12,$0E
!byte $27,$27,$27,$9C,$27,$27,$27

!byte $01,$02,$48
!byte $13,$0E
!byte $26,$01,$01
!byte $26,$01,$01
!byte $25,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1E,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $13,$0D
; END, null terminated
!byte $00


data_corridor_right_exit_near_and_far_right
; top row, main line
!byte $06,$00,$04
!byte $20,$00
!byte $27,$27,$27,$27
; top row, bottom side patch up
!byte $0B,$00,$04
!byte $21,$00
!byte $27,$27,$27,$27
; top row, top side patch up
!byte $09,$00,$03
!byte $1F,$00
!byte $27,$27,$27
; top row, middle top section
!byte $06,$00,$00
!byte $17,$08
; top row, far bottom side patch up
!byte $0B,$00,$00
!byte $17,$09
; top row, far top side patch up
!byte $09,$00,$00
!byte $17,$07
; right turn wall join
!byte $11,$00,$00
!byte $18,$07
; right turn wall top
!byte $10,$00,$02
!byte $19,$07
!byte $01,$01
; right turn wall left side
!byte $0C,$00,$04
!byte $18,$08
!byte $28,$28,$28,$28
; right turn wall right side
!byte $0D,$00,$07
!byte $1C,$05
!byte $28,$28,$28,$28,$28,$28,$28
; right turn wall left side
!byte $0D,$00,$03
!byte $17,$09
!byte $28,$28,$28
; wall front right side
!byte $0D,$00,$01
!byte $15,$0B
!byte $28
; right turn wall flat
!byte $01,$06,$0E
!byte $19,$0D
!byte $01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01,$26,$01,$01
; floor border far side and near side
!byte $05,$02,$06
!byte $15,$0E
!byte $52,$29,$A4,$29,$29,$29

; right turn wall flat
!byte $01,$06,$01
!byte $16,$0D
!byte $28

; wall front right side
!byte $11,$00,$00
!byte $15,$0B
; wall front right side
!byte $10,$00,$00
!byte $16,$0B
; wall front right side
!byte $0E,$00,$00
!byte $15,$0C

; floor
!byte $01,$02,$48
!byte $14,$0E
!byte $28,$01,$01
!byte $26,$01,$01
!byte $26,$01,$01,$01
!byte $25,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01
!byte $21,$01,$01,$01,$01,$01,$01,$01,$01
!byte $20,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $1F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
; hall end
!byte $01,$00,$00
!byte $14,$0D
; END, null terminated
!byte $00

;==========================================================
; IMAGE DATA
;==========================================================

; none yet