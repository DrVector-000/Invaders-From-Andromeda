;************************************************************************
;* Constants
;************************************************************************

;---------------------------------------------------
; TV Standars
NTSC        = 0
PAL         = 1
SECAM       = 2
PAL60       = 3
SYSTEM      = PAL

Volume0                 = #4
Volume1                 = #6
LeftLimitSpaceShip      = #30
RightLimitSpaceShip     = #135
LeftLimitAliens         = #28
RightLimitAliens        = #160
XPosGRP0Energia         = #$4c
WhereStop               = #40
EndPointBallSpaceShip   = #2
HeightInvader           = #10
StartSpaceShip          = #150
MaxShields              = #5
TimeForChangeScreen     = #255
PreBarriers             = #126
EndBarriers             = #135
EnergySingleBarrier     = #5
LastLevelEndGame        = #10
RightLimitBarriers      = #56
LeftLimitBarriers       = #40

;---------------------------------------------------
; Colors
	;---------------------------------------------------
	; NTSC
    IF SYSTEM = NTSC
ColorPlanet                     = #$92
ColorLandPlanet                 = #$f2
ColorOrizonPlanet               = #$90
ColorMissiles                   = #$fe
ColorEnergyCounter              = #$0f
ColorEnergyCounterWhenOneIsLost = #$00
ColorBall                       = #$ae
ColorLogo                       = #$98
ColorEndGame                    = #$46
RedBarriers                     = #$41
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60
ColorPlanet                     = #$B2
ColorLandPlanet                 = #$42
ColorOrizonPlanet               = #$90
ColorMissiles                   = #$4E
ColorEnergyCounter              = #$0E
ColorEnergyCounterWhenOneIsLost = #$00
ColorBall                       = #$9E
ColorLogo                       = #$B8
ColorEndGame                    = #$66
RedBarriers                     = #$44
        ELSE
            IF SYSTEM = SECAM
ColorPlanet                     = #$02
ColorLandPlanet                 = #$04
ColorOrizonPlanet               = #$00
ColorMissiles                   = #$0C
ColorEnergyCounter              = #$0E
ColorEnergyCounterWhenOneIsLost = #$00
ColorBall                       = #$0A
ColorLogo                       = #$0A
ColorEndGame                    = #$04
RedBarriers                     = #$04
            ENDIF
        ENDIF
    ENDIF