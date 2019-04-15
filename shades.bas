'Cloned Shades
'(a clone of 'Shades' which was originally developed by
'UOVO - http://www.uovo.dk/ - for iOS)
'
'The goal of this game is to use the arrow keys to choose where
'to lay the next block falling down. If you align 4 blocks of
'the same color shade horizontally, you erase a line. If you
'pile two identical blocks, they will merge to become darker,
'unless they are already the darkest shade available (of 5).
'
'It has a tetris feeling to it, but it's not tetris at all.
'
'The idea is not original, since it's a clone, but I coded it
'from the ground up.
'
'Changes:
'- Beta 1
'       - So far I got the game to work, but I'm running into issues
'         trying to show scores on the screen, mainly because I relied
'         on POINT to check whether I could move blocks down or not.
'       - There's no interface except for the actual gameboard.
'
'- Beta 2
'       - Been discarded. At some point everything was working well but
'         I managed to screw it all up, to the point I made the decision
'         to go back to beta 1 and start over. I like to mention it here
'         because even failure can teach you a lesson, and this one is
'         not one to forget.
'
'- Beta 3
'       - Converted all audio files to .OGG for smaller files and faster
'         loading.
'       - Game window now has an appropriate icon, generated at runtime.
'       - Block movement has been coded again, and now it doesn't rely
'         on POINT to detect blocks touching each other, but it actually
'         evaluates the current Y position. Like it should have been from
'         the start, to be honest.
'       - Redone the merge animation. Still looks the same, but it had to
'         be redone after the new movement routines have been implemented.
'       - Added a background image to the game board ("bg.png"), but uses
'         a gray background in case it cannot be loaded for some reason.
'       - Code is a bit easier to read, after I moved most of the main
'         loop code into separate subroutines.
'       - SCORES ON THE SCREEN!

_TITLE "Cloned Shades"

'Game constants: ------------------------------------------------------
CONST True = -1
CONST False = 0
CONST BlockWidth = 150
CONST BlockHeight = 64
CONST TopAnimationSteps = 30
CONST MergeSteps = 64
CONST DefaultIncrement = 2

'Type definitions: ----------------------------------------------------
TYPE ColorRGB
    R AS LONG
    G AS LONG
    B AS LONG
END TYPE

TYPE BoardType
    State AS INTEGER
    Shade AS INTEGER
END TYPE

'Variables for game control -------------------------------------------
DIM SHARED Board(1 TO 12, 1 TO 4) AS BoardType
DIM SHARED Shades(1 TO 5) AS ColorRGB, FadeStep AS INTEGER
DIM SHARED BlockPos(1 TO 4) AS INTEGER, BackgroundColor AS LONG
DIM SHARED BlockRows(1 TO 12) AS INTEGER, BgImage AS LONG
DIM SHARED i AS INTEGER, Increment AS INTEGER
DIM SHARED CurrentRow AS INTEGER, CurrentColumn AS INTEGER
DIM SHARED BlockPut AS _BYTE, Y AS INTEGER, PrevY AS INTEGER
DIM SHARED CurrentShade AS INTEGER, NextShade AS INTEGER
DIM SHARED AlignedWithRow AS _BYTE, InDanger AS _BYTE
DIM SHARED GameOver AS _BYTE, GameEnded AS _BYTE
DIM SHARED PreviousScore AS LONG, Score AS LONG

'Variables for screen pages:
DIM SHARED InfoScreen AS LONG
DIM SHARED OverlayGraphics AS LONG
DIM SHARED GameScreen AS LONG
DIM SHARED MainScreen AS LONG

'Variable for sound:
DIM SHARED DropSound(1 TO 3) AS LONG, SoundOn AS INTEGER, Alarm AS LONG
DIM SHARED WindSound AS LONG, SplashSound(1 TO 4) AS LONG, Whistle AS LONG
DIM SHARED BgMusic(1 TO 2) AS LONG

'Screen initialization: ------------------------------------------------
InfoScreen = _NEWIMAGE(300, 400, 32)
OverlayGraphics = _NEWIMAGE(600, 800, 32)
GameScreen = _NEWIMAGE(600, 800, 32)
MainScreen = _NEWIMAGE(600, 800, 32)

BgImage = _LOADIMAGE("bg.png", 32)

SCREEN MainScreen

IF BgImage < -1 THEN _PUTIMAGE , BgImage, MainScreen

'RGB data for shades: --------------------------------------------------
RESTORE Greens
FOR i = 1 TO 5
    READ Shades(i).R
    READ Shades(i).G
    READ Shades(i).B
NEXT i

'Since now we already have read the shades' rgb data,
'let's generate the window icon:
MakeIcon

'Coordinates for block locations in the board: ------------------------
RESTORE BlockPositions
FOR i = 1 TO 4
    READ BlockPos(i)
NEXT i

RESTORE RowCoordinates
FOR i = 1 TO 12
    READ BlockRows(i)
NEXT i

'Load sounds and images: ----------------------------------------------
LoadAssets

RANDOMIZE TIMER

SoundOn = True 'Eventually this will be read from a settings file
BackgroundColor = _RGB32(200, 200, 200)
InDanger = False
GameOver = False
GameEnded = False
NextShade = INT(RND * 3) + 1 'Randomly chooses a shade for the next block
PreviousScore = -1

_DEST GameScreen
IF BgImage < -1 THEN _PUTIMAGE , BgImage, GameScreen ELSE LINE (0, 0)-(599, 799), BackgroundColor, BF
UpdateScreen

'Main game loop: ------------------------------------------------------
DO
    GenerateNewBlock
    MoveBlock
    CheckDanger
    CheckMerge
    CheckConnectedLines
LOOP UNTIL GameOver OR GameEnded
SYSTEM

Greens:
DATA 255,255,204
DATA 158,255,102
DATA 107,204,51
DATA 58,153,0
DATA 47,127,0

BlockPositions:
DATA 0,151,302,453

RowCoordinates:
DATA 735,670,605,540,475,410,345,280,215,150,85,20

'SUBs and FUNCTIONs ----------------------------------------------------

SUB GenerateNewBlock
DIM LineSize AS INTEGER
DIM LineStart AS INTEGER
DIM LineEnd AS INTEGER
DIM TargetLineStart AS INTEGER
DIM TargetLineEnd AS INTEGER
DIM LeftSideIncrement AS INTEGER
DIM RightSideIncrement AS INTEGER

'Randomly chooses where the next block will start falling down
CurrentColumn = INT(RND * 4) + 1
CurrentShade = NextShade

'Randomly chooses the next shade. It is done at this point so
'that the "next" bar will be displayed correctly across the game screen.
NextShade = INT(RND * 3) + 1

'Block's Y coordinate starts offscreen
Y = -48: PrevY = Y

'Animate the birth of a new block:
IF Whistle AND SoundOn THEN
    _SNDPLAYCOPY Whistle
END IF

LineSize = 600
LineStart = 0
LineEnd = 599
TargetLineStart = (CurrentColumn * BlockWidth) - BlockWidth
TargetLineEnd = CurrentColumn * BlockWidth
LeftSideIncrement = (TargetLineStart - LineStart) / TopAnimationSteps
RightSideIncrement = (LineEnd - TargetLineEnd) / TopAnimationSteps

FOR i = 1 TO TopAnimationSteps
    _LIMIT 60
    IF BgImage < -1 THEN _PUTIMAGE (0, 0)-(599, 15), BgImage, GameScreen, (0, 0)-(599, 15) ELSE LINE (0, 0)-(599, 15), BackgroundColor, BF
    LINE (LineStart, 0)-(LineEnd, 15), Shade~&(CurrentShade), BF
    LineStart = LineStart + LeftSideIncrement
    LineEnd = LineEnd - RightSideIncrement
    IF INKEY$ <> "" THEN EXIT FOR
    UpdateScreen
NEXT i
IF BgImage < -1 THEN _PUTIMAGE (0, 0)-(599, 15), BgImage, GameScreen, (0, 0)-(599, 15) ELSE LINE (0, 0)-(599, 15), BackgroundColor, BF
END SUB

SUB MoveBlock
FadeStep = 0
Increment = DefaultIncrement
BlockPut = False
DO
    'Before moving the block using Increment, check if the movement will
    'cause the block to move to another row. If so, check if such move will
    'cause to block to be put down.
    IF ConvertYtoRow(Y + Increment) <> ConvertYtoRow(Y) AND NOT AlignedWithRow THEN
        Y = BlockRows(ConvertYtoRow(Y))
        AlignedWithRow = True
    ELSE
        Y = Y + Increment
        AlignedWithRow = False
    END IF

    CurrentRow = ConvertYtoRow(Y)

    IF AlignedWithRow THEN
        IF CurrentRow > 1 THEN
            IF Board(CurrentRow - 1, CurrentColumn).State THEN BlockPut = True
        ELSEIF CurrentRow = 1 THEN
            BlockPut = True
        END IF
    END IF

    IF BlockPut THEN
        DropSoundI = INT(RND * 3) + 1
        IF DropSound(DropSoundI) AND SoundOn THEN
            _SNDPLAYCOPY DropSound(DropSoundI)
        END IF
        Board(CurrentRow, CurrentColumn).State = True
        Board(CurrentRow, CurrentColumn).Shade = CurrentShade
    END IF

    IF Board(12, CurrentColumn).State = True AND Board(12, CurrentColumn).Shade <> Board(11, CurrentColumn).Shade THEN
        GameOver = True
        EXIT DO
    END IF

    'Erase previous block put on screen
    IF BgImage < -1 THEN
        _PUTIMAGE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY + Increment), BgImage, GameScreen, (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY + Increment)
    ELSE
        LINE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY + Increment), BackgroundColor, BF
    END IF
    PrevY = Y

    'Show the next shade on the top bar
    IF FadeStep < 255 THEN
        FadeStep = FadeStep + 1
        LINE (0, 0)-(599, 15), _RGBA32(Shades(NextShade).R, Shades(NextShade).G, Shades(NextShade).B, FadeStep), BF
    END IF

    'Draw the current block
    LINE (BlockPos(CurrentColumn), Y)-STEP(BlockWidth, BlockHeight), Shade(CurrentShade), BF

    UpdateScreen

    k$ = INKEY$

    SELECT CASE k$
        CASE CHR$(0) + CHR$(80), CHR$(32) 'Down arrow
            Increment = BlockHeight
        CASE CHR$(0) + CHR$(75) 'Left arrow
            IF CurrentColumn > 1 THEN
                IF Board(CurrentRow, CurrentColumn - 1).State = False THEN
                    IF BgImage < -1 THEN _PUTIMAGE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BgImage, GameScreen, (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight) ELSE LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                    CurrentColumn = CurrentColumn - 1
                END IF
            END IF
        CASE CHR$(0) + CHR$(77) 'Right arrow
            IF CurrentColumn < 4 THEN
                IF Board(CurrentRow, CurrentColumn + 1).State = False THEN
                    IF BgImage < -1 THEN _PUTIMAGE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BgImage, GameScreen, (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight) ELSE LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                    CurrentColumn = CurrentColumn + 1
                END IF
            END IF
        CASE CHR$(27)
            GameEnded = True
    END SELECT
LOOP UNTIL BlockPut OR GameEnded OR GameOver
END SUB

SUB CheckMerge
'Check if a block merge will be required:
Merged = False
IF BlockPut AND CurrentRow > 1 THEN
    DO
        IF Board(CurrentRow, CurrentColumn).Shade = Board(CurrentRow - 1, CurrentColumn).Shade THEN
            'Change block's color and the one touched to a darker shade, if it's not the darkest yet
            Score = Score + CurrentShade * 2
            IF CurrentShade < 5 THEN
                Merged = True
                i = CurrentShade
                RStep = (Shades(i).R - Shades(i + 1).R) / MergeSteps
                GStep = (Shades(i).G - Shades(i + 1).G) / MergeSteps
                BStep = (Shades(i).B - Shades(i + 1).B) / MergeSteps
                YStep = (BlockHeight) / MergeSteps

                RToGo = Shades(i).R
                GToGo = Shades(i).G
                BToGo = Shades(i).B

                ShrinkingHeight = BlockHeight * 2

                IF SplashSound(CurrentShade) AND SoundOn THEN
                    _SNDPLAYCOPY SplashSound(CurrentShade)
                END IF

                FOR Merge = 0 TO MergeSteps
                    _LIMIT MergeSteps
                    RToGo = RToGo - RStep
                    GToGo = GToGo - GStep
                    BToGo = BToGo - BStep

                    ShrinkingHeight = ShrinkingHeight - YStep

                    IF BgImage < -1 THEN
                        _PUTIMAGE (BlockPos(CurrentColumn), BlockRows(CurrentRow))-STEP(BlockWidth, BlockHeight * 2 + 1), BgImage, GameScreen, (BlockPos(CurrentColumn), BlockRows(CurrentRow) - 1)-STEP(BlockWidth, BlockHeight * 2 + 1)
                    ELSE
                        LINE (BlockPos(CurrentColumn), BlockRows(CurrentRow))-STEP(BlockWidth, BlockHeight * 2 + 1), BackgroundColor, BF
                    END IF

                    'Draw the merging blocks:
                    LINE (BlockPos(CurrentColumn), BlockRows(CurrentRow) + (BlockHeight * 2) - ShrinkingHeight + 1)-STEP(BlockWidth, ShrinkingHeight), _RGB32(RToGo, GToGo, BToGo), BF
                    UpdateScreen
                NEXT Merge

                Board(CurrentRow, CurrentColumn).State = False
                Board(CurrentRow - 1, CurrentColumn).Shade = i + 1
            ELSE
                EXIT DO
            END IF
        ELSE
            EXIT DO
        END IF
        CurrentRow = CurrentRow - 1
        CurrentShade = CurrentShade + 1
        Y = BlockRows(CurrentRow)
        PrevY = Y
        CheckDanger
    LOOP UNTIL CurrentRow = 1 OR CurrentShade = 5
    RedrawBoard
END IF
END SUB

SUB CheckConnectedLines
'Check for connected lines with the same shade and
'compute the new score, besides generating the disappearing
'animation:
Matched = False
DO
    CurrentMatch = CheckMatchingLine%
    IF CurrentMatch = 0 THEN EXIT DO

    Matched = True
    Score = Score + 40
    MatchLineStart = BlockRows(CurrentMatch) + BlockHeight \ 2

    IF WindSound AND SoundOn THEN
        _SNDPLAYCOPY WindSound
    END IF

    FOR i = 1 TO BlockHeight \ 2
        _LIMIT 60
        IF BgImage < -1 THEN
            _PUTIMAGE (0, MatchLineStart - i)-(599, MatchLineStart + i), BgImage, GameScreen, (0, MatchLineStart - i)-(599, MatchLineStart + i)
        ELSE
            LINE (0, MatchLineStart - i)-(599, MatchLineStart + i), BackgroundColor, BF
        END IF
        UpdateScreen
    NEXT i

    DestroyLine CurrentMatch
    RedrawBoard

    DropSoundI = INT(RND * 3) + 1
    IF DropSound(DropSoundI) AND SoundOn THEN
        _SNDPLAYCOPY DropSound(DropSoundI)
    END IF
LOOP
END SUB

FUNCTION ConvertYtoRow (CurrentY)
'Returns the row on the board through which the block is currently
'passing. Returns 0 if it's exactly on the row's start line.

IF CurrentY >= -48 AND CurrentY <= 20 THEN
    ConvertYtoRow = 12
ELSEIF CurrentY > 20 AND CurrentY <= 85 THEN
    ConvertYtoRow = 11
ELSEIF CurrentY > 85 AND CurrentY <= 150 THEN
    ConvertYtoRow = 10
ELSEIF CurrentY > 150 AND CurrentY <= 215 THEN
    ConvertYtoRow = 9
ELSEIF CurrentY > 215 AND CurrentY <= 280 THEN
    ConvertYtoRow = 8
ELSEIF CurrentY > 280 AND CurrentY <= 345 THEN
    ConvertYtoRow = 7
ELSEIF CurrentY > 345 AND CurrentY <= 410 THEN
    ConvertYtoRow = 6
ELSEIF CurrentY > 410 AND CurrentY <= 475 THEN
    ConvertYtoRow = 5
ELSEIF CurrentY > 475 AND CurrentY <= 540 THEN
    ConvertYtoRow = 4
ELSEIF CurrentY > 540 AND CurrentY <= 605 THEN
    ConvertYtoRow = 3
ELSEIF CurrentY > 605 AND CurrentY <= 670 THEN
    ConvertYtoRow = 2
ELSEIF CurrentY > 670 AND CurrentY <= 735 THEN
    ConvertYtoRow = 1
END IF
END FUNCTION

FUNCTION Shade~& (CurrentShade)
Shade~& = _RGB32(Shades(CurrentShade).R, Shades(CurrentShade).G, Shades(CurrentShade).B)
END FUNCTION

FUNCTION CheckMatchingLine%

DIM i AS INTEGER
DIM a.s AS INTEGER, b.s AS INTEGER, c.s AS INTEGER, d.s AS INTEGER
DIM a AS INTEGER, b AS INTEGER, c AS INTEGER, d AS INTEGER

FOR i = 1 TO 12
    a.s = Board(i, 1).State
    b.s = Board(i, 2).State
    c.s = Board(i, 3).State
    d.s = Board(i, 4).State

    a = Board(i, 1).Shade
    b = Board(i, 2).Shade
    c = Board(i, 3).Shade
    d = Board(i, 4).Shade

    IF a.s AND b.s AND c.s AND d.s THEN
        IF a = b AND b = c AND c = d THEN
            CheckMatchingLine% = i
            EXIT FUNCTION
        END IF
    END IF

NEXT i
CheckMatchingLine% = 0

END FUNCTION

SUB DestroyLine (LineToDestroy AS INTEGER)

DIM i AS INTEGER
SELECT CASE LineToDestroy
    CASE 1 TO 11
        FOR i = LineToDestroy TO 11
            Board(i, 1).State = Board(i + 1, 1).State
            Board(i, 2).State = Board(i + 1, 2).State
            Board(i, 3).State = Board(i + 1, 3).State
            Board(i, 4).State = Board(i + 1, 4).State

            Board(i, 1).Shade = Board(i + 1, 1).Shade
            Board(i, 2).Shade = Board(i + 1, 2).Shade
            Board(i, 3).Shade = Board(i + 1, 3).Shade
            Board(i, 4).Shade = Board(i + 1, 4).Shade
        NEXT i
        FOR i = 1 TO 4
            Board(12, i).State = False
            Board(12, i).Shade = 0
        NEXT i
    CASE 12
        FOR i = 1 TO 4
            Board(12, i).State = False
            Board(12, i).Shade = 0
        NEXT i
END SELECT

END SUB

SUB RedrawBoard

DIM i AS INTEGER, CurrentColumn AS INTEGER
DIM StartY AS INTEGER, EndY AS INTEGER

FOR i = 1 TO 12
    FOR CurrentColumn = 4 TO 1 STEP -1
        StartY = ((13 - i) * 65) - 45
        EndY = StartY + BlockHeight

        IF Board(i, CurrentColumn).State = True THEN
            LINE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), Shade(Board(i, CurrentColumn).Shade), BF
        ELSE
            IF BgImage < -1 THEN _PUTIMAGE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), BgImage, GameScreen, (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY) ELSE LINE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), BackgroundColor, BF
        END IF
    NEXT CurrentColumn
NEXT i

END SUB

SUB ShowScore
DIM ScoreString AS STRING

ScoreString = "Score:" + STR$(Score)

_DEST InfoScreen
CLS
LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 0), BF

'_FONT 16
PrintShadow 15, 15, ScoreString

'_FONT 8
'PrintShadow 15, 32, "Stats for nerds:"

_DEST GameScreen

END SUB

SUB MakeIcon
'Generates the icon that will be placed on the window title of the game
DIM Icon AS LONG
DIM PreviousDest AS LONG
DIM i AS INTEGER
CONST IconSize = 16

Icon = _NEWIMAGE(IconSize, IconSize, 32)
PreviousDest = _DEST
_DEST Icon

FOR i = 1 TO 5
    LINE (0, i * (IconSize / 5) - (IconSize / 5))-(IconSize, i * (IconSize / 5)), Shade~&(i), BF
NEXT i

_ICON Icon
_FREEIMAGE Icon

_DEST PreviousDest
END SUB

SUB CheckDanger
'Checks if any block pile is 11 blocks high, which
'means danger, which means player needs to think faster,
'which means we'll make him a little bit more nervous by
'switching our soothing bg song to a fast paced circus
'like melody.
IF Board(11, 1).State OR Board(11, 2).State OR Board(11, 3).State OR Board(11, 4).State THEN
    IF SoundOn AND NOT InDanger THEN
        IF Alarm THEN _SNDPLAYCOPY Alarm
        IF BgMusic(1) THEN _SNDSTOP BgMusic(1)
        IF BgMusic(2) THEN _SNDLOOP BgMusic(2)
    END IF
    InDanger = True
ELSE
    IF SoundOn AND InDanger THEN
        IF BgMusic(2) THEN _SNDSTOP BgMusic(2)
        IF BgMusic(1) THEN _SNDLOOP BgMusic(1)
    END IF
    InDanger = False
END IF
END SUB

SUB LoadAssets
LoadingMessage$ = "Loading..."
COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
_PRINTSTRING (_WIDTH \ 2 - _PRINTWIDTH(LoadingMessage$) \ 2, _HEIGHT \ 2 - _FONTHEIGHT \ 2), LoadingMessage$
_SCREENMOVE _MIDDLE

WindSound = _SNDOPEN("wind.ogg", "SYNC")
Whistle = _SNDOPEN("whistle.ogg", "SYNC,VOL")
Alarm = _SNDOPEN("alarm.ogg", "SYNC,VOL")
DropSound(1) = _SNDOPEN("drop1.ogg", "SYNC")
DropSound(2) = _SNDOPEN("drop2.ogg", "SYNC")
DropSound(3) = _SNDOPEN("drop3.ogg", "SYNC")
SplashSound(1) = _SNDOPEN("water1.ogg", "SYNC")
SplashSound(2) = _SNDOPEN("water2.ogg", "SYNC")
SplashSound(3) = _SNDOPEN("water3.ogg", "SYNC")
SplashSound(4) = _SNDOPEN("water4.ogg", "SYNC")
BgMusic(1) = _SNDOPEN("Water_Prelude.ogg", "VOL")
BgMusic(2) = _SNDOPEN("quick.ogg", "SYNC,VOL")

_SNDVOL BgMusic(1), 0.3
_SNDVOL BgMusic(2), 1
_SNDVOL Whistle, 0.02
_SNDLOOP BgMusic(1)
END SUB

SUB UpdateScreen
'Display the gamescreen, overlay and scoreboard layers
ShowScore

_PUTIMAGE , GameScreen, MainScreen
_PUTIMAGE , OverlayGraphics, MainScreen
_PUTIMAGE , InfoScreen, MainScreen
_DISPLAY
END SUB

SUB PrintShadow (x%, y%, Text$)
'Shadow:
COLOR _RGBA32(150, 150, 150, 150), _RGBA32(0, 0, 0, 0)
_PRINTSTRING (x% + 1, y% + 1), Text$

'Text:
COLOR _RGB32(255, 255, 255), _RGBA32(0, 0, 0, 0)
_PRINTSTRING (x%, y%), Text$
END SUB
