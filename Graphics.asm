;************************************************************************
;* Graphics Data
;************************************************************************

FirstInvader0
	.byte $80 ; |X       |
	.byte $5B ; | X XX XX|
	.byte $3C ; |  XXXX  |
	.byte $5A ; | X XX X |
	.byte $FF ; |XXXXXXXX|
	.byte $FF ; |XXXXXXXX|
	.byte $42 ; | X    X |
	.byte $66 ; | XX  XX |
	.byte $08 ; |    X   |
	.byte $00 ; |        |
FirstInvader1
	.byte $42 ; | X    X |
	.byte $5A ; | X XX X |
	.byte $3C ; |  XXXX  |
	.byte $5A ; | X XX X |
	.byte $FF ; |XXXXXXXX|
	.byte $FF ; |XXXXXXXX|
	.byte $42 ; | X    X |
	.byte $66 ; | XX  XX |
	.byte $10 ; |   X    |
	.byte $00 ; |        |
SecondInvader0
	.byte $81 ; |X      X|
	.byte $C3 ; |XX    XX|
	.byte $A5 ; |X X  X X|
	.byte $DB ; |XX XX XX|
	.byte $DB ; |XX XX XX|
	.byte $FF ; |XXXXXXXX|
	.byte $7E ; | XXXXXX |
	.byte $A5 ; |X X  X X|
	.byte $24 ; |  X  X  |
	.byte $00 ; |        |
SecondInvader1
	.byte $81 ; |X      X|
	.byte $E7 ; |XXX  XXX|
	.byte $A5 ; |X X  X X|
	.byte $DB ; |XX XX XX|
	.byte $DB ; |XX XX XX|
	.byte $FF ; |XXXXXXXX|
	.byte $7E ; | XXXXXX |
	.byte $A5 ; |X X  X X|
	.byte $81 ; |X      X|
	.byte $00 ; |        |
ThirdInvader0
	.byte $66 ; | XX  XX |
	.byte $24 ; |  X  X  |
	.byte $7E ; | XXXXXX |
	.byte $99 ; |X  XX  X|
	.byte $DB ; |XX XX XX|
	.byte $DB ; |XX XX XX|
	.byte $7E ; | XXXXXX |
	.byte $3C ; |  XXXX  |
	.byte $AA ; |X X X X |
	.byte $00 ; |        |
ThirdInvader1
	.byte $2C ; |  X XX  |
	.byte $24 ; |  X  X  |
	.byte $7E ; | XXXXXX |
	.byte $99 ; |X  XX  X|
	.byte $DB ; |XX XX XX|
	.byte $FF ; |XXXXXXXX|
	.byte $7E ; | XXXXXX |
	.byte $3C ; |  XXXX  |
	.byte $55 ; | X X X X|
	.byte $00 ; |        |
ForthInvader0
	.byte $42 ; | X    X |
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $3C ; |  XXXX  |
	.byte $FF ; |XXXXXXXX|
	.byte $18 ; |   XX   |
	.byte $81 ; |X      X|
	.byte $81 ; |X      X|
	.byte $00 ; |        |
ForthInvader1
	.byte $E7 ; |XXX  XXX|
	.byte $BD ; |X XXXX X|
	.byte $BD ; |X XXXX X|
	.byte $FF ; |XXXXXXXX|
	.byte $3C ; |  XXXX  |
	.byte $7E ; | XXXXXX |
	.byte $18 ; |   XX   |
	.byte $42 ; | X    X |
	.byte $42 ; | X    X |
	.byte $00 ; |        |
FifthInvader0
	.byte $99 ; |X  XX  X|
	.byte $A5 ; |X X  X X|
	.byte $FF ; |XXXXXXXX|
	.byte $B5 ; |X XX X X|
	.byte $3C ; |  XXXX  |
	.byte $3C ; |  XXXX  |
	.byte $24 ; |  X  X  |
	.byte $2E ; |  X XXX |
	.byte $70 ; | XXX    |
	.byte $00 ; |        |
FifthInvader1
	.byte $18 ; |   XX   |
	.byte $24 ; |  X  X  |
	.byte $BD ; |X XXXX X|
	.byte $EF ; |XXX XXXX|
	.byte $BD ; |X XXXX X|
	.byte $3C ; |  XXXX  |
	.byte $24 ; |  X  X  |
	.byte $74 ; | XXX X  |
	.byte $0E ; |    XXX |
	.byte $00 ; |        |
SixthInvader0
	.byte $6A ; | XX X X |
	.byte $81 ; |X      X|
	.byte $99 ; |X  XX  X|
	.byte $BD ; |X XXXX X|
	.byte $24 ; |  X  X  |
	.byte $BD ; |X XXXX X|
	.byte $99 ; |X  XX  X|
	.byte $81 ; |X      X|
	.byte $56 ; | X X XX |
	.byte $00 ; |        |
SixthInvader1
	.byte $6A ; | XX X X |
	.byte $C3 ; |XX    XX|
	.byte $5A ; | X XX X |
	.byte $7E ; | XXXXXX |
	.byte $99 ; |X  XX  X|
	.byte $7E ; | XXXXXX |
	.byte $5A ; | X XX X |
	.byte $C3 ; |XX    XX|
	.byte $56 ; | X X XX |
	.byte $00 ; |        |
SeventhInvader0
	.byte $24 ; |  X  X  |
	.byte $5A ; | X XX X |
	.byte $5A ; | X XX X |
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $99 ; |X  XX  X|
	.byte $4A ; | X  X X |
	.byte $89 ; |X   X  X|
	.byte $00 ; |        |
SeventhInvader1
	.byte $66 ; | XX  XX |
	.byte $DB ; |XX XX XX|
	.byte $7E ; | XXXXXX |
	.byte $FF ; |XXXXXXXX|
	.byte $7E ; | XXXXXX |
	.byte $99 ; |X  XX  X|
	.byte $4A ; | X  X X |
	.byte $52 ; | X X  X |
	.byte $81 ; |X      X|
	.byte $00 ; |        |
EighthInvader0
	.byte $42 ; | X    X |
	.byte $BD ; |X XXXX X|
	.byte $FF ; |XXXXXXXX|
	.byte $DB ; |XX XX XX|
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $5A ; | X XX X |
	.byte $49 ; | X  X  X|
	.byte $94 ; |X  X X  |
	.byte $00 ; |        |
EighthInvader1
	.byte $81 ; |X      X|
	.byte $BD ; |X XXXX X|
	.byte $FF ; |XXXXXXXX|
	.byte $5A ; | X XX X |
	.byte $7E ; | XXXXXX |
	.byte $7E ; | XXXXXX |
	.byte $5A ; | X XX X |
	.byte $92 ; |X  X  X |
	.byte $29 ; |  X X  X|
	.byte $00 ; |        |
NinethInvader0
	.byte $99 ; |X  XX  X|
	.byte $B5 ; |X XX X X|
	.byte $DB ; |XX XX XX|
	.byte $EF ; |XXX XXXX|
	.byte $BD ; |X XXXX X|
	.byte $1C ; |   XXX  |
	.byte $B5 ; |X XX X X|
	.byte $30 ; |  XX    |
	.byte $B1 ; |X XX   X|
	.byte $00 ; |        |
NinethInvader1
	.byte $99 ; |X  XX  X|
	.byte $AD ; |X X XX X|
	.byte $DB ; |XX XX XX|
	.byte $EF ; |XXX XXXX|
	.byte $BD ; |X XXXX X|
	.byte $38 ; |  XXX   |
	.byte $AD ; |X X XX X|
	.byte $0D ; |    XX X|
	.byte $0C ; |    XX  |
	.byte $00 ; |        |
TenthInvader0
	.byte $24 ; |  X  X  |
	.byte $7E ; | XXXXXX |
	.byte $56 ; | X X XX |
	.byte $FF ; |XXXXXXXX|
	.byte $BC ; |X XXXX  |
	.byte $3D ; |  XXXX X|
	.byte $A4 ; |X X  X  |
	.byte $2F ; |  X XXXX|
	.byte $F0 ; |XXXX    |
	.byte $00 ; |        |
TenthInvader1
	.byte $41 ; | X     X|
	.byte $7E ; | XXXXXX |
	.byte $6A ; | XX X X |
	.byte $FF ; |XXXXXXXX|
	.byte $3D ; |  XXXX X|
	.byte $BC ; |X XXXX  |
	.byte $25 ; |  X  X X|
	.byte $F4 ; |XXXX X  |
	.byte $0F ; |    XXXX|
InvaderGhost
	.byte $00 ; |        |

	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |
	.byte $00 ; |        |

;Colors
    IF SYSTEM = NTSC
FirstInvaderColor
    .byte $BE, $BE, $BE, $BA, $C8, $C8, $C2, $C2, $Ce
SecondInvaderColor
	.byte $20, $22, $22, $24, $24, $24, $26, $2A, $2E
ThirdInvaderColor
	.byte $12, $14, $16, $18, $18, $18, $18, $12, $14
ForthInvaderColor
	.byte $62, $64, $64, $64, $62, $68, $68, $68, $68, #ColorMissiles ;color of grp1 missile and missile1
FifthInvaderColor
	.byte $A6, $A6, $A6, $A2, $A2, $A4, $A4, $A6, $A6, #ColorMissiles ;nice trick, grp1 missile and missile1 color
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60
FirstInvaderColor
    .byte $5E, $5E, $5E, $5A, $58, $58, $52, $52, $3E
SecondInvaderColor
	.byte $40, $42, $42, $44, $44, $44, $46, $4A, $4E
ThirdInvaderColor
	.byte $22, $24, $26, $28, $28, $28, $28, $22, $24
ForthInvaderColor
	.byte $C2, $C4, $C4, $C4, $C2, $C8, $C8, $C8, $C8, #ColorMissiles ;color of grp1 missile and missile1
FifthInvaderColor
	.byte $96, $96, $96, $92, $92, $94, $94, $96, $96, #ColorMissiles ;nice trick, grp1 missile and missile1 color
        ELSE
            IF SYSTEM = SECAM
FirstInvaderColor
    .byte $08, $08, $08, $08, $08, $08, $08, $08, $08
SecondInvaderColor
	.byte $04, $04, $04, $04, $04, $04, $04, $04, $04
ThirdInvaderColor
	.byte $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C
ForthInvaderColor
	.byte $06, $06, $06, $06, $06, $06, $06, $06, $06, #ColorMissiles ;color of grp1 missile and missile1
FifthInvaderColor
	.byte $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A, #ColorMissiles ;nice trick, grp1 missile and missile1 color
            ENDIF
        ENDIF
    ENDIF
SpaceShip0
	.byte $10 ; |   X    |
	.byte $10 ; |   X    |
	.byte $38 ; |  XXX   |
	.byte $38 ; |  XXX   |
	.byte $38 ; |  XXX   |
	.byte $BA ; |X XXX X |
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $FE ; |XXXXXXX |
	.byte $FE ; |XXXXXXX |
	.byte $BA ; |X XXX X |
	.byte $38 ; |  XXX   |
	.byte $10 ; |   X    |
	.byte $00 ; |        |
SpaceShip1
	.byte $10 ; |   X    |
	.byte $10 ; |   X    |
	.byte $38 ; |  XXX   |
	.byte $38 ; |  XXX   |
	.byte $38 ; |  XXX   |
	.byte $BA ; |X XXX X |
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $FE ; |XXXXXXX |
	.byte $FE ; |XXXXXXX |
	.byte $BA ; |X XXX X |
	.byte $92 ; |X  X  X |
	.byte $00 ; |        |
	.byte $00 ; |        |

    IF SYSTEM = NTSC
ColorSpaceShip
	.byte $E2, $E4, $90, $92, $92, $92, $92, $92, $92, $04, $42, $40, $40, $00
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60
ColorSpaceShip
	.byte $22, $24, $90, $92, $92, $92, $92, $92, $92, $04, $62, $60, $60, $00
        ELSE
            IF SYSTEM = SECAM
ColorSpaceShip
	.byte $00, $00, $02, $02, $02, $02, $02, $02, $02, $00, $04, $04, $04, $00
            ENDIF
        ENDIF
    ENDIF

Ufo
	.byte $18 ; |   XX   |
	.byte $2C ; |  X XX  |
	.byte $5E ; | X XXXX |
	.byte $FF ; |XXXXXXXX|
	.byte $FF ; |XXXXXXXX|
	.byte $3C ; |  XXXX  |
	.byte $00 ; |        |

    IF SYSTEM = NTSC
ColorUfo
	.byte $0E, $0E, $0E, $84, $82, $94, $00
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60
ColorUfo
	.byte $0E, $0E, $0E, $B4, $B2, $B4, $00
        ELSE
            IF SYSTEM = SECAM
ColorUfo
	.byte $0E, $0E, $0E, $02, $02, $02, $00
            ENDIF
        ENDIF
    ENDIF

Shield0
	.byte $00 ; |        |
	.byte $D1 ; |XX X   X|
	.byte $90 ; |X  X    |
	.byte $D1 ; |XX X   X|
	.byte $5D ; | X XXX X|
	.byte $55 ; | X X X X|
	.byte $D5 ; |XX X X X|
Shield1
	.byte $08 ; |    X   |
	.byte $09 ; |    X  X|
	.byte $69 ; | XX X  X|
	.byte $69 ; | XX X  X|
	.byte $6B ; | XX X XX|
	.byte $4B ; | X  X XX|
	.byte $6B ; | XX X XX|
Number0
	.byte $7E ; | XXXXXX |
	.byte $66 ; | XX  XX |
	.byte $66 ; | XX  XX |
	.byte $66 ; | XX  XX |
	.byte $66 ; | XX  XX |
	.byte $66 ; | XX  XX |
	.byte $7E ; | XXXXXX |
Number1
	.byte $38 ; |  XXX   |
	.byte $18 ; |   XX   |
	.byte $18 ; |   XX   |
	.byte $18 ; |   XX   |
	.byte $18 ; |   XX   |
	.byte $18 ; |   XX   |
	.byte $7E ; | XXXXXX |
Number2
	.byte $7E ; | XXXXXX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
	.byte $7E ; | XXXXXX |
	.byte $60 ; | XX     |
	.byte $60 ; | XX     |
	.byte $7E ; | XXXXXX |
Number3
	.byte $7E ; | XXXXXX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
	.byte $1E ; |   XXXX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
	.byte $7E ; | XXXXXX |
Number4
	.byte $60 ; | XX     |
	.byte $66 ; | XX  XX |
	.byte $66 ; | XX  XX |
	.byte $7E ; | XXXXXX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
Number5
	.byte $7E ; | XXXXXX |
	.byte $60 ; | XX     |
	.byte $60 ; | XX     |
	.byte $7E ; | XXXXXX |
	.byte $06 ; |     XX |
	.byte $06 ; |     XX |
	.byte $7E ; | XXXXXX |

	align $100
final_0
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00001110
	.byte %00001010
	.byte %00001010
	.byte %00001010
	.byte %00001110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01110111
	.byte %00010101
	.byte %00010101
	.byte %01110101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11101010
	.byte %10101010
	.byte %10101010
	.byte %10101100
	.byte %11001110
	.byte %10101010
	.byte %10101010
	.byte %11101110

final_1
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11101010
	.byte %10001010
	.byte %10001010
	.byte %10001110
	.byte %10001010
	.byte %10001010
	.byte %10001010
	.byte %10001110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01110011
	.byte %01010000
	.byte %01010000
	.byte %01010000
	.byte %01010011
	.byte %01010010
	.byte %01010010
	.byte %01010011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10100100
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %11101010
	.byte %10101010
	.byte %10101010
	.byte %11101010

final_2
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101110
	.byte %10101000
	.byte %10101000
	.byte %10101000
	.byte %10101100
	.byte %10101000
	.byte %10101000
	.byte %11101110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101001
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10111010
	.byte %00101010
	.byte %00101010
	.byte %10111010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11100111
	.byte %10100100
	.byte %10100100
	.byte %10100100
	.byte %10100100
	.byte %10100100
	.byte %10100100
	.byte %11100100

final_3
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01000111
	.byte %01000100
	.byte %01000010
	.byte %01000010
	.byte %01000001
	.byte %01000001
	.byte %01000000
	.byte %11100111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111011
	.byte %10100010
	.byte %10100010
	.byte %10100010
	.byte %10110010
	.byte %10100010
	.byte %10100010
	.byte %10111011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01000101
	.byte %01000101
	.byte %01000101
	.byte %01000101
	.byte %01000111
	.byte %01000101
	.byte %01000101
	.byte %11100101

final_4
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10100010
	.byte %00100010
	.byte %00100010
	.byte %00100010
	.byte %00111011
	.byte %00101010
	.byte %10101010
	.byte %10111010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001001
	.byte %10001001
	.byte %10001001
	.byte %10001001
	.byte %10001001
	.byte %10001001
	.byte %10001001
	.byte %00011101
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01110111
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010100
	.byte %01010100
	.byte %01010100
	.byte %01110111

final_5
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10100000
	.byte %10000000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01011100
	.byte %01010000
	.byte %01010000
	.byte %01010000
	.byte %11011000
	.byte %01010000
	.byte %01010000
	.byte %01011100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01110101
	.byte %01010101
	.byte %01010101
	.byte %01110111

;TITLE GAME
	align $100
logo_0
	.byte %11101110
	.byte %10100010
	.byte %11101110
	.byte %10001010
	.byte %10001010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000011
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000010
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101010
	.byte %10101011
	.byte %10101010
	.byte %10101010
	.byte %10110010
	.byte %10000010
	.byte %10000010
	.byte %10000100
logo_1
	.byte %01110101
	.byte %01000100
	.byte %01000101
	.byte %01000101
	.byte %01110101
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101011
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10110011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011101
	.byte %00010101
	.byte %10011101
	.byte %10000101
	.byte %10011000
	.byte %01000000
	.byte %00000000
	.byte %00000000
logo_2
	.byte %11010101
	.byte %01010101
	.byte %11011101
	.byte %00010101
	.byte %11011101
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00101011
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10110010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %00110011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11011101
	.byte %01010001
	.byte %01011101
	.byte %11010101
	.byte %01001100
	.byte %01000000
	.byte %01000000
	.byte %01000000
logo_3
	.byte %01011100
	.byte %01010100
	.byte %01010100
	.byte %01010100
	.byte %11011100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101100
	.byte %10111000
	.byte %10100000
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00011100
	.byte %00000100
	.byte %00011100
	.byte %00010000
	.byte %11001100
	.byte %00000000
	.byte %00000000
	.byte %00000000
logo_4
	.byte %01110111
	.byte %01000101
	.byte %01110101
	.byte %00010101
	.byte %01110111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11101100
	.byte %10001010
	.byte %10001010
	.byte %10001010
	.byte %10001010
	.byte %10001010
	.byte %11001010
	.byte %10001010
	.byte %10001010
	.byte %01101010
	.byte %00001100
	.byte %00000000
	.byte %10000000
	.byte %10000000
	.byte %10010001
	.byte %10010001
	.byte %10010001
	.byte %10010001
	.byte %11001100
	.byte %10000000
	.byte %10000000
	.byte %11100000
logo_5
	.byte %01110111
	.byte %01000100
	.byte %01110111
	.byte %00010001
	.byte %01110111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %11100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %10100000
	.byte %11100000
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %10010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %11011110
	.byte %00010000
	.byte %00000000
	.byte %00000000

;*************************************************
;PLAYFIELD of PLANET
PlayfieldPlanet
	.byte $80,$80 ;|   XX               | (  0)
	.byte $80,$88 ;|   XX   X           | (  1)
	.byte $C0,$08 ;|  XX    X           | (  2)
	.byte $C0,$18 ;|  XX   XX           | (  3)
	.byte $00,$18 ;|       XX           | (  4)
	.byte $00,$3C ;|      XXXX          | (  5)
	.byte $00,$3C ;|      XXXX          | (  6)
	.byte $00,$20 ;|      X             | (  7)
	.byte $00,$01 ;|           X        | (  8)
	.byte $00,$01 ;|           X        | (  9)
	.byte $00,$41 ;|     X     X        | ( 10)
	.byte $10,$41 ;|X    X     X        | ( 11)
	.byte $10,$49 ;|X    X  X  X        | ( 12)
	.byte $10,$69 ;|X    XX X  X        | ( 13)
	.byte $10,$E9 ;|X   XXX X  X        | ( 14)
	.byte $10,$C9 ;|X   XX  X  X        | ( 15)
	.byte $10,$88 ;|X   X   X           | ( 16)
	.byte $50,$08 ;|X X     X           | ( 17)
	.byte $50,$08 ;|X X     X           | ( 18)
	.byte $50,$08 ;|X X     X           | ( 19)
	.byte $70,$08 ;|XXX     X           | ( 20)
	.byte $70,$0C ;|XXX     XX          | ( 21)
	.byte $F0,$0C ;|XXXX    XX          | ( 22)
	.byte $F0,$9C ;|XXXXX  XXX          | ( 23)
	.byte $F0,$DC ;|XXXXXX XXX          | ( 24)
	.byte $F0,$DC ;|XXXXXX XXX          | ( 25)
	.byte $F0,$9C ;|XXXXX  XXX          | ( 26)
	.byte $70,$1C ;|XXX    XXX          | ( 27)
	.byte $30,$3C ;|XX    XXXX          | ( 28)
	.byte $10,$3C ;|X     XXXX          | ( 29)
	.byte $00,$3F ;|      XXXXXX        | ( 30) ;missile
	.byte $00,$3F ;|      XXXXXX        | ( 31) ;missile
	.byte $00,$3F ;|      XXXXXX        | ( 32) ;missile
	.byte $00,$3F ;|      XXXXXX        | ( 33) ;missile
	.byte $00,$7F ;|     XXXXXXX        | ( 34) ;missile
	.byte $00,$7C ;|     XXXXX          | ( 35)
	.byte $00,$7C ;|     XXXXX          | ( 36)
	.byte $00,$F8 ;|    XXXXX           | ( 37)
	.byte $00,$F0 ;|    XXXX            | ( 39)
	.byte $00,$E0 ;|    XXX             | ( 40)
	.byte $80,$C0 ;|   XXX              | ( 41)
	.byte $C0,$80 ;|  XXX               | ( 42)
	.byte $C0,$00 ;|  XX                | ( 43)
	.byte $00,$00 ;|                    | ( 44)
	.byte $00,$00 ;|                    | ( 45)
	.byte $00,$00 ;|                    | ( 46)
	.byte $00,$00 ;|                    | ( 47)
	.byte $20,$00 ;| X                  | ( 48)
	.byte $20,$00 ;| X                  | ( 49)
	.byte $60,$00 ;| XX                 | ( 50)
	.byte $F0,$00 ;|XXXX                | ( 51)
	.byte $70,$00 ;|XXX                 | ( 52)
	.byte $30,$00 ;|XX                  | ( 53)
	.byte $30,$00 ;|XX                  | ( 54)
	.byte $00,$00 ;|                    | ( 55)
	.byte $00,$00 ;|                    | ( 56)
	.byte $00,$00 ;|                    | ( 57)
	.byte $00,$00 ;|                    | ( 58)
	.byte $00,$00 ;|                    | ( 59)
	.byte $80,$00 ;|   X                | ( 60)
	.byte $80,$00 ;|   X                | ( 61)
	.byte $80,$00 ;|   X                | ( 62)
	.byte $80,$00 ;|   X                | ( 63)
	.byte $80,$00 ;|   X                | ( 64)
	.byte $80,$80 ;|   XX               | ( 65)
	.byte $C0,$80 ;|  XXX               | ( 66)
	.byte $C0,$80 ;|  XXX               | ( 67)
	.byte $C0,$A0 ;|  XXX X             | ( 68)
	.byte $C0,$A0 ;|  XXX X             | ( 69)
	.byte $C0,$A0 ;|  XXX X             | ( 70)
	.byte $C0,$B0 ;|  XXX XX            | ( 71)
	.byte $E0,$F0 ;| XXXXXXX            | ( 72)
	.byte $E0,$F8 ;| XXXXXXXX           | ( 73)
	.byte $E0,$F0 ;| XXXXXXX            | ( 74)
