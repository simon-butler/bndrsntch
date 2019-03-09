
; 3 sprites generated with spritemate on 09/03/2019, 16:31:22
; Byte 64 of each sprite contains multicolor (high nibble) & color (low nibble) information

LDA #$08 ; sprite multicolor 1
STA $D025
LDA #$06 ; sprite multicolor 2
STA $D026


; sprite 1 / singlecolor / color: $01
sprite_1
!byte $00,$38,$00,$00,$fe,$00,$03,$ff
!byte $80,$00,$c6,$00,$02,$ba,$80,$03
!byte $7d,$80,$03,$7d,$80,$07,$01,$c0
!byte $05,$6d,$40,$05,$39,$40,$05,$83
!byte $40,$05,$bb,$40,$0e,$44,$e0,$18
!byte $28,$30,$1a,$28,$b0,$17,$01,$d0
!byte $17,$83,$d0,$17,$d7,$d0,$0f,$cf
!byte $e0,$0b,$83,$b0,$18,$28,$30,$01

; sprite 2 / singlecolor / color: $01
sprite_2
!byte $1c,$ee,$30,$39,$e7,$98,$39,$c9
!byte $98,$31,$ae,$dc,$73,$6f,$8c,$71
!byte $f7,$8c,$f1,$f7,$8e,$f1,$f7,$8e
!byte $e1,$e7,$86,$61,$e7,$8f,$f1,$c6
!byte $1f,$d2,$00,$d7,$e3,$01,$de,$07
!byte $83,$e0,$07,$c3,$e0,$07,$e7,$c0
!byte $07,$c3,$c0,$07,$c3,$e0,$07,$81
!byte $e0,$0f,$81,$e0,$0f,$81,$e0,$01

; sprite 3 / singlecolor / color: $01
sprite_3
!byte $0f,$81,$e0,$0f,$81,$e0,$0f,$81
!byte $e0,$0f,$81,$f0,$0f,$00,$f0,$0f
!byte $00,$f0,$0f,$00,$f0,$1f,$00,$f0
!byte $1f,$00,$f8,$1e,$00,$78,$1e,$00
!byte $78,$1e,$00,$78,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01