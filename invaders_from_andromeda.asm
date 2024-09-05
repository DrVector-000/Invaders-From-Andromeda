;Copyright 2022 Cisano Carmelo
;
;This file is part of Invaders From Andromeda
;
;    Invaders From Andromeda is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Invaders From Andromeda is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with Invaders From Andromeda.  If not, see <http://www.gnu.org/licenses/>.

	processor 6502

	include "tia.h"
	include "pia6532.h"
	include "vars.h"

	include "macro.h"
	include "my_macro.h"
	include "energy_macro.h"
	include "joystick_macro.h"
	include "timer.h"

;===============================================================================
; Define Constants and Colors
;===============================================================================
	include "Consts.asm"

	
    SEG CODE
	ORG		$0000
	RORG	$F000

;---Graphics Data---
	include "Graphics.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TablePtrNumbLow
	.byte #<Number0, #<Number1, #<Number2, #<Number3, #<Number4, #<Number5
TablePtrNumbHi
	.byte #>Number0, #>Number1, #>Number2, #>Number3, #>Number4, #>Number5
TablePntrInvadersFrame0Low
	.byte #<FirstInvader0, #<SecondInvader0, #<ThirdInvader0, #<ForthInvader0, #<FifthInvader0
	.byte #<SixthInvader0, #<SeventhInvader0, #<EighthInvader0, #<NinethInvader0, #<TenthInvader0, #<InvaderGhost
TablePntrInvadersFrame1Low
	.byte #<FirstInvader1, #<SecondInvader1, #<ThirdInvader1, #<ForthInvader1, #<FifthInvader1
	.byte #<SixthInvader1, #<SeventhInvader1, #<EighthInvader1, #<NinethInvader1, #<TenthInvader1, #<InvaderGhost
TablePntrInvadersFrame0Hi
	.byte #>FirstInvader0, #>SecondInvader0, #>ThirdInvader0, #>ForthInvader0, #>FifthInvader0
	.byte #>SixthInvader0, #>SeventhInvader0, #>EighthInvader0, #>NinethInvader0, #>TenthInvader0, #>InvaderGhost
TablePntrInvadersFrame1Hi
	.byte #>FirstInvader0, #>SecondInvader0, #>ThirdInvader0, #>ForthInvader0, #>FifthInvader0
	.byte #>SixthInvader0, #>SeventhInvader0, #>EighthInvader0, #>NinethInvader0, #>TenthInvader0, #>InvaderGhost

TableColorInvadersLow
	.byte #<FirstInvaderColor, #<SecondInvaderColor, #<ThirdInvaderColor, #<ForthInvaderColor, #<FifthInvaderColor
TableColorInvadersHi
	.byte #>FirstInvaderColor, #>SecondInvaderColor, #>ThirdInvaderColor, #>ForthInvaderColor, #>FifthInvaderColor
TableSoundsLow
	.byte #<InvadersMarch, #<Fire, #<InvaderExplosion, #<UfoHit, #<SpaceShipHit, #<VictorySongCh0, #<VictorySongCh1
TableSoundsHi
	.byte #>InvadersMarch, #>Fire, #>InvaderExplosion, #>UfoHit, #>SpaceShipHit, #>VictorySongCh0, #>VictorySongCh1

TableNusiz
	      ;0   1    2    3    4    5    6    7
	.byte #0, #0,  #0,  #1,  #0,  #2,  #1,  #3
TableOffsetInvaders
	.byte #0, #32, #16, #16, #0, #0, #0, #0 ;for NUSIZX
TableOffsetInvadersGRP1
	.byte #48, #80, #64, #64, #48, #48, #48, #48
TableTimerBombing
	.byte #126, #90, #70, #60
TableCoordXMissile
	.byte #78, #62, #46, #30, #14, #$fd;.byte #80, #64, #48, #32, #16, #0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Start
	CLEAN_START
	
initializingMainVariables
	;when turns on...
	;draw logo	
	lda #TimeForChangeScreen
	sta TimerTitleScreen
	;set Volumes to 0 for no sound when reset is pressed
	lda #0
	sta AUDV0
	sta AUDV1
	;define some pointers
	lda #>SpaceShip0
	sta PointerSpaceShipHi
	lda TableColorInvadersHi
	sta PntrColorGRPHi

	lda #<Ufo
	sta PointerGRP1UfoLow
	lda #>Ufo
	sta PointerGRP1UfoHi
	
	lda #$36
	sta KindOfBarriers

	lda #>FirstInvader0
	sta PointerGRP0Hi
	sta PointerGRP1Hi
	;define playfiend features
	lda #1
	sta CTRLPF ;Playfield mirror 1+0 GRP has priority over PF
	sta Direction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame
	VERTICAL_SYNC

	sta HMCLR

	;set zero Cursor
	;lda #0
	sta CursorRowInvaders

verifyReset
	lsr SWCHB ;test Game Reset switch
	bcs setTimer ;reset?
	jmp Start

;---------------------------------------------------
; Vertical Blank
; NTSC 37 Scan lines
; PAL SECAM 45 Scan lines
setTimer
	IF SYSTEM = PAL || SYSTEM = SECAM
		lda #53 ; 76 / 64 * 45
	ELSE
		lda #44 ; 76 / 64 * 37
	ENDIF
	sta TIM64T

increaseMyTimers
	;increase Timer for every thing in the game
	inc TimerForAll
	inc TimerShiftInvadersColumn

checkGameCompleted
	lda LevelNumber
	cmp #LastLevelEndGame
	bne checkForTitleScreen
	jmp drawLogo

checkForTitleScreen
	lda TimerTitleScreen ;first start has the value for the logo screen
	cmp #TimeForChangeScreen
	bne increaseTimerTitleScreen
	jmp drawLogo

increaseTimerTitleScreen
	lda BooleanGameOver
	bne setZeroSomeVariables
	inc TimerTitleScreen ;increase timer

setZeroSomeVariables
	lda BooleanAtBeginOfEveryLevel
	bne setFrameSpaceShip ;no firt start
;only at the first start and at change level
InitializeValues
	lda #0
	sta CursorSound0
	sta TempoCH0

	lda #82
	sta CoordXSpaceShip ;center of the screen - relatively speaking

	lda #%11111100 ;252
	sta MaskDeleteBarrier

	lda #MaxShields
	ldx LevelNumber ;if the game is going, no shields are modified
	bne .gameInProgress ;otherwhise, the shields are restored
	sta ShieldNumber
	sta CoordXUfo
	sta BooleanGameOver
	lda #48
	sta CoordXBarries
	lda #1
	sta StepBall
.gameInProgress
	lda #%10101010
	sta DefineBarriers
	sta LevelBarriers

	lda #%00100000
	sta CursorLeftInvaders
	lda #%00000001
	sta CursorRightInvaders
	sta BooleanAtBeginOfEveryLevel
	sta CursorWhoBombs
	lda #2
	sta CursorSpringLeftRight

	lda #40 ;coordinate where invaders start
	sta CoordXInvaders
	lda #96
	sta OffsetXInvaders ;relative coordinate last invader
	
	lda #%00111111
	sta InvadersLine0
	sta InvadersLine1
	sta InvadersLine2
	sta InvadersLine3
	sta InvadersLine4

	lda #12
	sta CoordYInvaders

	sta PowerSingleBarrier ;every barrier shield

	lda #152
	sta CursorYBallSpaceShip

	lda #%00011111
	sta TimerMask
	lda #31 ;1 more for a best sound when 1 invader remains
	sta TotalInvaders

	lda #140
	sta CoordYMissile1
	sta CoordYMissile2

	sta CoordXMissile2 ;must not under 15 - necessary for the positionig routine at the first time
	sta BooleanNoBulletInGame
	sta TimerWhenInvaderStrike ;to delete
;end InitializeValues

;alternate frame of SpaceShip
setFrameSpaceShip
	ldy #<SpaceShip0
	lda TimerForAll
	and #%00000010
	bne storeSpaceShipPointer
	ldy #<SpaceShip1
storeSpaceShipPointer
	sty PointerSpaceShipLow

manageTimers
	;TIMER_INVADERS_BOMBING
	;before spaceship and aliens can shot
	lda BooleanNoBulletInGame
	beq postNoBulletInGame
	dec BooleanNoBulletInGame
postNoBulletInGame
	lda TimerMask
	bne decrementTimer
	lda TotalInvaders
	sta TimerMask
	jmp manageCollisionBallUfo
decrementTimer
	dec TimerMask
;end manageTimer

manageCollisionBallUfo
	lda UfoCollision
	beq noCollisionBallUfo

increaseShield
	lda ShieldNumber
	cmp #MaxShields
	bpl noIncreaseShield
	inc ShieldNumber
noIncreaseShield
	jsr ResetBallMissileGRP0
	jmp checkJoystick

noCollisionBallUfo
	lda BooleanFireSpaceShip
	beq checkJoystick
	lda CursorYBallSpaceShip
	cmp #EndPointBallSpaceShip ;arrived at the end of the missile path=ball arrived at the top of the screen
	bne incrementBallMissile
	jsr ResetBallMissileGRP0
	jmp checkJoystick

incrementBallMissile
	;increment pointer ball/missile SpaceShip
	;only if BooleanFireSpaceShip is true
	;1 pixel one frame, 2 pixel next frame = 3 pixel in 2 frames
	lda TimerForAll
	and #%00000001
	beq .stepOne
	lda CursorYBallSpaceShip
	sbc StepBall ;1 or 2 pixel set at alternate level
	jmp .stepTwo
.stepOne
	lda CursorYBallSpaceShip
	sbc #1
.stepTwo
	sta CursorYBallSpaceShip

checkJoystick
	JOYSTICK_MACRO

checkHitInvader
	FIND_HIT_INVADER_MACRO

	lda InvadersLine0
	ora InvadersLine1
	ora InvadersLine2
	ora InvadersLine3
	ora InvadersLine4
	;sta Temp
	and CurosorMaskInvaders ;set inside FIND_HIT_INVADER_MACRO
	beq .setBooleanToFalse

	lda BooleanShiftInvadersColumn
	beq checkGameOver
	lda TimerShiftInvadersColumn ;delay the shift of the column
	bne checkGameOver ;if timer!=0

;shift column of aliens
	ldy #$05
shiftColumn
	dey ;start from 4
	lda InvadersLine0,y ;the hit invader can be everywhere
	and CurosorMaskInvaders
	bne shiftColumn ;loop for to find where is the alien missing
	lda CurosorMaskInvaders
	ora InvadersLine0,y
	sta InvadersLine0,y ;fill the last line

	ldy #$ff
.setColumn
	iny ;start from 0
	lda InvadersLine0,y
	and CurosorMaskInvaders
	beq .setColumn ;loop until finds the line with the invader to delete

	lda InvadersLine0,y
	eor CurosorMaskInvaders ;it is deleted
	sta InvadersLine0,y ;new line generated
.setBooleanToFalse
	lda #0
	sta BooleanShiftInvadersColumn
;end shift column

checkGameOver
	lda ShieldNumber
	sta BooleanGameOver
	bne checkNoMoreInvaders
	;set gameover
checkNoMoreInvaders
	lda InvadersLine0
	ora InvadersLine1
	ora InvadersLine2
	ora InvadersLine3
	ora InvadersLine4
	sta Temp
	;end level
	bne defLeftXInvaders
	jmp letInvadersBombing
	;end level
defLeftXInvaders
	and CursorLeftInvaders
	bne defRightXInvaders
newLeftX
	;InvaderLineX = 00011111
	;the first column is gone
	;asl InvaderLineX = 00111110
	;all invaders are shifted on the left
	;CoordXInvaders+16
	;for continue to show in the right position
	asl InvadersLine0
	asl InvadersLine1
	asl InvadersLine2
	asl InvadersLine3
	asl InvadersLine4

	lda CoordXInvaders
	adc #16
	sta CoordXInvaders
	jmp checkNoMoreInvaders
;check the right column. If !=0 the new coloumn and X coordinate are the next - offset of -16px
defRightXInvaders
	lda Temp ;ORA operation above
	and CursorRightInvaders
	bne noNewX
newRightX
	asl CursorRightInvaders
	lda OffsetXInvaders ;for check right limits
	sbc #16
	sta OffsetXInvaders
	jmp defRightXInvaders
noNewX
	;check in GRP1 is alla gone for set the same X coordinate of GRP0 
	lda Temp
	and #%00000111
	sta BooleanGRP1AllGone

loadPointersInvaders
	LOAD_POINTERS_INVADERS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
moveInvaders
	lda TimerMask
	bne letInvadersBombing
.checkLeft
	lda CoordXInvaders
	cmp #LeftLimitAliens ;if 0 changes direction
	bpl .checkRight;bne .checkRight
	lda #2
	sta CursorSpringLeftRight
	jmp .doIt
.checkRight
	;lda CoordXInvaders ;loaded above
	adc OffsetXInvaders
	cmp #RightLimitAliens ;when 0 changes direction
	bmi .doIt
	lda #$fe ;-2
	sta CursorSpringLeftRight
.doIt
	lda CursorSpringLeftRight
	clc
	adc CoordXInvaders
	sta CoordXInvaders ;this variable can stuck the invaders
	;alternate frame animation of the invaders
	lda AlternateFrameInvader
	eor #1
	sta AlternateFrameInvader
	;alternate frame animation of the invaders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

letInvadersBombing
	TIMER_INVADERS_BOMBING

setSpeedUfo
	lda TimerForAll
	and #%00000001 ;it moves at alternate frames
	bne prePositionUfo
	inc CoordXUfo ;set speed of Ufo 1px every 2 frames
prePositionUfo
	lda CoordXUfo
	cmp #160
	bne setPositionUfo
	lda #0
	sta CoordXUfo
setPositionUfo
	ldy #1 ;parameter (GRP1) for subroutine
	jsr PositionElement

setPositionBall
	lda CoordXBallSpaceShip
	ldy #4 ;parameter (BALL) for subroutine - it is used as missile for the spaceship
	jsr PositionElement

setPositionMissile1
	lda CoordXMissile1
	ldy #3 ;parameter (MISSILE1) for subroutine
	jsr PositionElement

setPositionBarriers
	lda CoordXBarries
	ldy #2 ;parameter (MISSILE0) for subroutine
	jsr PositionElement

;;;;;;;;;;;;;;
Loop
	lda INTIM
	bne Loop

; Disable VBLANK
	lda #$0
	sta VBLANK
;last line before of scanloop

;Draw the 192 scanlines
	ldx #3

	sta CXCLR
	sta WSYNC ;wait for next scanline
	sta HMOVE

	ldy #0
;kernel grafico
drawUfo
	inx
	DRAWBALL_WITHOUT_STA_MACRO2
	DRAW_GRP1_UFO_MACRO
	cpx CoordYInvaders;#12 - start invaders
	bne drawUfo
	;get collision GRP1 - BALL
	lda #%01000000
	and CXP1FB
	sta UfoCollision

	sta CXCLR

setInvaders
	sta WSYNC
	inx
	DRAWBALL_MACRO2
	inx
	DRAWBALL_WITHOUT_STA_MACRO2

	inx ;for the drawball after stawsync used in positionGRP0

	sta HMCLR
	sta WSYNC
	sta ENABL

	DRAWBALL_WITHOUT_STA_MACRO2

	ldy CursorRowInvaders
copiesOfInvaders
	lda InvadersLine0,y
	lsr
	lsr
	lsr
	;and #%00000111
	tay
	lda TableNusiz,y
	sta NUSIZ0
	clc
	lda CoordXInvaders ;position - parameter for subroutine
	adc TableOffsetInvaders,y

positionGRP0
	sec
	sta WSYNC ;start a new line
		; one cycle of .DivideLoopGRP0 is saved
		sbc #15
		;;;;;;;;;;;;;
		;and the saved cycle is used for these
		ldy Bullet
		sty ENABL
		;;;;;;;;;;;;;
.DivideLoopGRP0
	sbc #15	;subtract 15
	bcs .DivideLoopGRP0	;branch until negative
	eor #7	;calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0	;fix coarse position
	sta HMP0
endPositionGRP0
	;inx ;incremented above

	inx ;used after sta wsync
	sta WSYNC

	DRAWBALL_MACRO2

	inx ;anticipate inx in positioGRP1
	DRAWBALL_WITHOUT_STA_MACRO2

	ldy CursorRowInvaders
	lda InvadersLine0,y
	and #%00000111
	tay
	lda TableNusiz,y
	sta NUSIZ1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda BooleanGRP1AllGone
	bne someInvaders
	lda CoordXInvaders ;if GRP1 is empty, set same GRP0 coordinate
	jmp positionGRP1

someInvaders
	;end add
	clc
	lda CoordXInvaders
	;adc #48 ;offset position after GRP0 - not necessary anymore because I created TableOffsetInvadersGRP1
	adc TableOffsetInvadersGRP1,y ;parameter for subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
positionGRP1
	;ldy Tmp
	sec
	sta WSYNC ;start a new line
		;one cycle of .DivideLoopGRP0 is saved
		sbc #15;sty ENABL
		;;;;;;;;;;;;;
		;and the saved cycle is used for these
		ldy Bullet;Tmp
		sty ENABL
		;;;;;;;;;;;;;
.DivideLoopGRP1
	sbc #15	;subtract 15
	bcs .DivideLoopGRP1	;branch until negative
	eor #7	;calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP1	;fix coarse position
	sta HMP1
endPositionGRP1

	sta WSYNC
	inx
	DRAWBALL_MACRO2

definePointers
	ldy CursorRowInvaders
	lda TableColorInvadersLow,y
	sta PntrColorGRPLow
	lda ArrayPointerGRP0Line0,y
	sta PointerGRP0Low
	lda ArrayPointerGRP1Line0,y ;this variable can have ghost pointer
	sta PointerGRP1Low

	;;;;;;;
	sta WSYNC	;sync w/ scanline
	sta HMOVE	;apply fine offsets

	inx
	DRAWBALL_MACRO2

	ldy #0 ;parameter (GRP0) for DRAW_GRP0_GRP1_MACRO

drawInvader
	inx ;anticipate for the WSYNC inside DRAW_GRP0_GRP1_MACR

	DRAWBALL_WITHOUT_STA_MACRO2

	DRAW_GRP0_GRP1_MACRO ;write ENABL inside

	cpy #HeightInvader
	bne drawInvader

	inc CursorRowInvaders
	lda CursorRowInvaders
	cmp #5
	beq positionGRP1Missile2
	jmp setInvaders

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
positionGRP1Missile2
	inx
	DRAWBALL_WITHOUT_STA_MACRO3
	lda CoordXMissile2 ;between #0 and #14 cannot be set, because ldy Bullet etc...
	sec
	sta WSYNC ;start a new line
		;one cycle of .DivideLoopGRP0 is saved
		sbc #15
		;;;;;;;;;;;;;
		;and the saved cycle is used for these
		ldy Bullet
		sty ENABL
		;;;;;;;;;;;;;
.DivideLoopGRP1Missile2
	sbc #15	;subtract 15
	bcs .DivideLoopGRP1Missile2	;branch until negative
	eor #7	;calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP1	;set position
	sta HMP1

endPositionGRP1Missile2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
preEmptySpace
	inx
	sta WSYNC
	sta HMOVE ;necessary for GRP1Missile2
	;inx

	DRAWBALL_MACRO2

	lda #0
	sta NUSIZ1

	;the color of GRP1Missile2 and Missile1 is setted by last color of fifth invader
collisionDetectP0P1BL
	ora CXP1FB
	ora CXP0FB
	;and #%01000000 ;mask used in FIND_HIT_INVADER_MACRO for save cycles here
	sta CollisionInvader
	sta CXCLR

	ldy CoordYMissile1
	lda CoordYMissile2
	sta CursorYMissile2
	;if no barries, delete all until emptySpace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;ball grp1-missile2 missile1 all togheter
	;prepare barriers!
preShowBarriers
	inx
	stx TempX
	DRAWBALL_WITHOUT_STA_MACRO2
	sta WSYNC
	sta ENABL
	lda PlayfieldPlanet,y
	sta ENAM1
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	sta GRP1
	;define triple copy and color of Barriers
	lda #RedBarriers
	sta COLUP0
	lda KindOfBarriers
	sta NUSIZ0
	;triple copy and color of Barriers
	dec CursorYMissile2
	dey
	ldx TempX
	cpx #PreBarriers
	bne preShowBarriers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;ball grp1-missile2 missile1 all togheter
	;draw barriers!
showBarriers
	inx
	stx TempX
	DRAWBALL_WITHOUT_STA_MACRO2
	sta WSYNC
	sta ENABL
	lda PlayfieldPlanet,y
	sta ENAM1
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	sta GRP1

	lda LevelBarriers
	sta ENAM0
	lsr LevelBarriers

	dec CursorYMissile2
	dey
	ldx TempX
	cpx #EndBarriers
	bne showBarriers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;ball grp1-missile2 missile1 all togheter
emptySpace
	inx
	stx TempX
	DRAWBALL_WITHOUT_STA_MACRO2
	sta WSYNC
	sta ENABL
	lda PlayfieldPlanet,y
	sta ENAM1
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	sta GRP1

	dec CursorYMissile2
	dey
	ldx TempX
	cpx #StartSpaceShip
	bne emptySpace

	;define missile1
	lda PlayfieldPlanet,y
	sta Tmp
	;define missile2
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	sta Temp ;used in the POSITIONING_GRP0_MACRO2

	;reset the Missile0/Barriers one copy for the spaceship
	lda #0
	sta NUSIZ0

	lda CoordXSpaceShip
	sta HMCLR
	POSITIONING_GRP0_MACRO2
;X is not necessary anymore
	dey
	sta WSYNC
	sta HMOVE

	;define missile1
	;dey ;above
	lda PlayfieldPlanet,y
	sta ENAM1
	;define missile2
	dec CursorYMissile2
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	sta GRP1

;pre missiles calculation
	lda #14 ;counter for the spaceship
	sta TempX

	dey
	lda PlayfieldPlanet,y
	sta Tmp

	dec CursorYMissile2
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	tax ;sta GRP1**

	sty CursorYMissile1

	ldy #0
	sty ENABL

drawSpaceShip
	;DRAW_GRP0_SPACESHIP_MACRO
	lda Tmp
	sta WSYNC
	sta ENAM1
	stx GRP1 ;**
	lda (PointerSpaceShipLow),y
	sta GRP0
	lda ColorSpaceShip,y
	sta COLUP0
	iny

	dec CursorYMissile1
	ldx CursorYMissile1
	lda PlayfieldPlanet,x
	sta Tmp

	dec CursorYMissile2
	ldx CursorYMissile2
	lda PlayfieldPlanet,x
	and #%00000010
	tax ;used for stx GRP1**

	dec TempX
	;lda TempX
	bne drawSpaceShip

	;tun off missile1 and grp1-missileGFX
	ldx #0
	stx ENAM1
	stx GRP1

changeColorEnergyCounterWhenOneIsLost
	lda #ColorEnergyCounter
	ldy BooleanShieldLost
	beq noLost
	lda #ColorEnergyCounterWhenOneIsLost ;#0
	;ldx #0 ;done above
	stx BooleanShieldLost
noLost
	sta COLUP0
	sta COLUP1



	;REPEAT 20
	;	sta WSYNC
	;REPEND



	POSITIONING_ENERGY_COUNTER_MACRO
	DEF_NUMBERS_POINTERS_MACRO

;DRAWING THE PLANET
horizonPlanet
	sta WSYNC
	lda #ColorOrizonPlanet
	sta COLUBK

setParametersPlanet
	sta WSYNC
	lda #ColorPlanet
	sta COLUBK
	lda #ColorLandPlanet
	sta COLUPF

	lda PointerGFXPlanet
	sbc #2
	and #%01111111
	sta Temp
LoopDrawPlanet

	ldy PointerGFXPlanet

	sta WSYNC
	lda PlayfieldPlanet,y
	sta PF0
	iny
	lda PlayfieldPlanet,y
	sta PF1
	iny
	tya
	and #%01111111
	sta PointerGFXPlanet

	SHOW_ENERGY_MACRO
finalLoop
	ldy PointerGFXPlanet

	sta WSYNC
	lda PlayfieldPlanet,y
	sta PF0
	iny
	lda PlayfieldPlanet,y
	sta PF1
	iny
	tya
	and #%01111111
	sta PointerGFXPlanet

	inx
	IF SYSTEM = PAL || SYSTEM = SECAM
		cpx #228
		;cpx #208
	ELSE
		cpx #192
	ENDIF
	bne finalLoop

	lda Temp
	sta PointerGFXPlanet

; 30 lines of overscan
LVOver
	IF SYSTEM = PAL || SYSTEM = SECAM
		lda #43 ; 76 / 64 * 36
	ELSE
		lda #36 ; 76 / 64 * 30
	ENDIF
	sta TIM64T
	sta WSYNC

; Reenable VBLANK for below (and top of next frame)
	lda #2
	sta VBLANK
Finale
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta COLUBK
	sta ENAM1
	sta GRP0
	sta GRP1
	sta NUSIZ1
	;sta ENABL
	sta Bullet

setColorBall
	;color ball/missile of SpaceShip
	lda #ColorBall
	sta COLUPF

;check all the collisions between barriers-missile0 and missile1, grp1, ball
collisionBarriersMissile1
	lda #%01000000
	and CXPPMM ;M0M1
	beq collisionBarriersMissile2
	lda #140
	sta CoordYMissile1
	dec PowerSingleBarrier ;every single line of the  barrier has N power
	bne collisionBarriersMissile2 ;if it becomes 0
	jsr DecreaseBarrier

collisionBarriersMissile2
	lda #%10000000
	and CXM0P ;M0P1
	beq collisionBarriersBall
	lda #140
	sta CoordYMissile2
	dec PowerSingleBarrier
	bne preCheck
	jsr DecreaseBarrier

collisionBarriersBall
	lda #%01000000
	and CXM0FB ;M0BL
	beq preCheck
	jsr ResetBallMissileGRP0

;check collision between grp0 and missile 1, grp1
preCheck
	lda ShieldNumber
	beq soundEffect ;necessary when game is over for no sound
collisionMissile1
	lda #%10000000
	and CXM1P ;M1P0
	beq collisionMissile2
	dec ShieldNumber
	sta BooleanShieldLost
	;reset missile1 position
	lda #140
	sta CoordYMissile1
collisionMissile2
	lda #%10000000
	and CXPPMM ;P0P1
	beq soundEffect
	dec ShieldNumber
	sta BooleanShieldLost
	;reset missile2 position
	lda #140
	sta CoordYMissile2

soundEffect
	PLAY_SOUND_MACRO

setBarriers
	lda DefineBarriers
	sta LevelBarriers ;necessary for to draw the barriers in the next frame

setDifficult
	;alternate speed ball/missile SpaceShip
	ldx #1
	lda LevelNumber
	and #%00000001
	beq .slowBullet
	ldx #2
.slowBullet
	stx StepBall

.setAlternateBarriers
	ldx #$36
	lda LevelNumber
	and #%00000001
	beq .storeBarriers
.twoBarriers
	ldx #$34
.storeBarriers
	stx KindOfBarriers

.moveBarriers
	lda LevelNumber
	cmp #4 ;from level 5, the barriers begin to move
	bmi LoopaOverscan
	jsr MoveLeftRight

LoopaOverscan
	lda INTIM
	bne LoopaOverscan
; Go back and do another frame
	jmp NextFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display the name of the game and the victory message
drawLogo
	lda INTIM
	bne drawLogo
	; Disable VBLANK
	lda #$0
	sta VBLANK
	ldy #55
deltaFrameUp
	sta WSYNC
	dey
	bne deltaFrameUp

	lda #3
	sta NUSIZ0
	sta NUSIZ1	; both players have 3 copies
	;lda #1
	sta VDELP0
	sta VDELP1
	lda #59
	ldy #0
	jsr PositionElement ;position GRP0

	lda #67
	ldy #1
	jsr PositionElement ;position GRP1

	sta WSYNC
	sta HMOVE

	lda #33 ;logo dimension
	sta Temp
	;if game over the logo is showen, otherwhise the victory message
	lda LevelNumber
	cmp #LastLevelEndGame
	beq .drawLoopVictoryMessage

.drawLoopLogo
	ldy Temp ;counts backwards
	lda logo_5,y ;6th
	sta Tmp

	sta WSYNC
	lda #ColorLogo
	sta COLUP0
	sta COLUP1

	lda logo_0,y ;1st
	sta GRP0
	lda logo_1,y ;2nd
	sta GRP1
	lda logo_2,y ;3rd
	sta GRP0
	lda logo_4,y ;5th
	tax
	lda logo_3,y ;4th
	ldy Tmp
	sta GRP1 ;4th
	stx GRP0 ;5th
	sty GRP1 ;6th
	sta GRP0
	dec Temp ;go to next line
	bpl .drawLoopLogo ;repeat until < 0

	sta WSYNC
	lda #0
	sta AUDC0
	sta AUDF0
	jmp .afterLoop

.drawLoopVictoryMessage
	ldy Temp ;counts backwards
	lda final_5,y ;6th
	sta Tmp

	sta WSYNC
	lda #ColorEndGame
	sta COLUP0
	sta COLUP1

	lda final_0,y ;1st
	sta GRP0
	lda final_1,y ;2nd
	sta GRP1
	lda final_2,y ;3rd
	sta GRP0
	lda final_4,y ;5th
	tax
	lda final_3,y ;4th
	ldy Tmp
	sta GRP1 ;4th
	stx GRP0 ;5th
	sty GRP1 ;6th
	sta GRP0
	dec Temp ;go to next line
	bpl .drawLoopVictoryMessage ;repeat until < 0

	sta WSYNC

.afterLoop

	lda #0
	sta VDELP0
	sta VDELP1
	sta GRP0
	sta GRP1
	sta NUSIZ0
	sta NUSIZ1

	sta WSYNC
	lda LevelNumber
	cmp #LastLevelEndGame
	beq .preDeltraFrame ;if last level, fire button for start game is not listen
	;manage fire button for start new game
	lda #%10000000  ;Fire?
	bit INPT4
	bne .preDeltraFrame	
	;the game begins
	lda #0
	sta TimerTitleScreen
	sta BooleanAtBeginOfEveryLevel
	sta LevelNumber
	sta BooleanVictorySongCh0
	sta BooleanVictorySongCh1

.preDeltraFrame
	ldy #71
.deltaFrame
	sta WSYNC
	dey
	bne .deltaFrame
	jmp changeColorEnergyCounterWhenOneIsLost

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINE
PositionElement SUBROUTINE
;ac is the X position 
;y is the graphic object
	sec
	sta WSYNC
	bit 0
.DivideLoop2
	sbc #15
	bcs .DivideLoop2
	eor #7
	asl
	asl
	asl
	asl
	sta RESP0,y	;fix coarse position
	sta HMP0,y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Reset Missile-Ball
ResetBallMissileGRP0 SUBROUTINE
	lda #0
	sta CollisionInvader
	sta BooleanFireSpaceShip
	;reset array ball position
	lda #152
	sta CursorYBallSpaceShip
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBROUTINE Decrease hit barrier
DecreaseBarrier SUBROUTINE
	lda #EnergySingleBarrier ;the full enery value is restored for the next line
	sta PowerSingleBarrier
	;DefineBarriers    = lda 10101010
	;MaskDeleteBarrier = and 11111100
	;--------------------------------
	;DefineBarriers    = sta 10101000 now the barrier has 3 lines
	;MaskDeleteBarrier = asl 11111000
	;MaskDeleteBarrier = asl 11110000
	;next AND
	;DefineBarriers    = lda 10101010
	;MaskDeleteBarrier = and 11110000
	;--------------------------------
	;DefineBarriers    = sta 10100000 now the barrier has 2 lines
	lda DefineBarriers
	and MaskDeleteBarrier
	sta DefineBarriers
	asl MaskDeleteBarrier
	asl MaskDeleteBarrier
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBROUTINE called by PLAY_SOUND_MACRO even for play Invader March
SoundEffectCH0 SUBROUTINE
	ldy CursorSound0
	;last 3 datas must be zero, so all the sounds stop
	lda (SoundMarchDataLow),y
	sta AUDC0
	iny
	lda (SoundMarchDataLow),y
	sta AUDF0
	iny
	lda (SoundMarchDataLow),y
	sta TempoCH0
	beq .endMarchSound
	iny
	sty CursorSound0
.endMarchSound
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBROUTINE called by PLAY_SOUND_MACRO
SoundEffectCH1 SUBROUTINE
	ldy CursorSound1
	;last 3 datas must be zero, so all the sounds stop
	lda (SoundDataLow),y
	sta AUDC1
	iny
	lda (SoundDataLow),y
	sta AUDF1
	iny
	lda (SoundDataLow),y
	sta TempoCH1
	beq .endSound
	iny
	sty CursorSound1
.endSound
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBROUTINE
MoveLeftRight SUBROUTINE
	lda TimerForAll
	and #%00011111
	bne .endSpring
	clc
	lda CoordXBarries
	adc Direction ;it can be 1 or -1
	sta CoordXBarries
.checkLeftLimit
	;lda CoordXBarries
	cmp #LeftLimitBarriers
	bne .checkRightLimit
	lda #$01
	sta Direction ;it becomes 1
	jmp .endSpring
.checkRightLimit
	lda CoordXBarries
	cmp #RightLimitBarriers
	bne .endSpring
	lda #$ff
	sta Direction ;it becomes -1
.endSpring
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SoundFX
	include "Sound.asm"

;************************************************************************
;* Interrupt Vectors
;************************************************************************
	SEG		IVECT
	ORG		$0FFA
	RORG	$FFFA

	.word Start
	.word Start
	.word Start
