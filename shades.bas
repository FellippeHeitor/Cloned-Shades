CONST True = -1
CONST False = 0
CONST BlockWidth = 150
CONST BlockHeight = 64
CONST TopAnimationSteps = 60
CONST AnimationDelay = .003

TYPE ColorRGB
    R AS INTEGER
    G AS INTEGER
    B AS INTEGER
END TYPE

TYPE BoardType
    State AS INTEGER
    Shade AS INTEGER
END TYPE

DIM SHARED Shades(1 TO 5) AS ColorRGB, FadeStep AS INTEGER
DIM SHARED BlockPos(1 TO 4) AS INTEGER, BackgroundColor AS _UNSIGNED LONG
DIM SHARED BlockRows(1 TO 12) AS INTEGER
DIM LevelDelay AS SINGLE, Increment AS INTEGER
DIM SHARED DropSound(1 TO 3) AS LONG, SoundOn AS INTEGER, Alarm AS LONG
DIM SHARED WindSound AS LONG, SplashSound(1 TO 4) AS LONG, Whistle AS LONG
DIM BgMusic AS LONG
DIM CurrentRow AS INTEGER
DIM SHARED Board(1 TO 12, 1 TO 4) AS BoardType, Score AS LONG
DIM BlockPut AS INTEGER
DIM SHARED Arial16 AS LONG, Arial32 AS LONG, BoardBG AS LONG
DIM SHARED LastScoreShown AS LONG, MainGame AS LONG, DrawnBoard AS LONG, ScoreBoard AS LONG

_TITLE "Cloned Shades"

MainGame = _NEWIMAGE(600, 800, 32)
DrawnBoard = _NEWIMAGE(600, 800, 32)

SCREEN MainGame
_PRINTMODE _KEEPBACKGROUND
_SCREENMOVE _MIDDLE

RESTORE Greens
FOR i = 1 TO 5
    READ Shades(i).R
    READ Shades(i).G
    READ Shades(i).B
NEXT i

RESTORE BlockPositions
FOR i = 1 TO 4
    READ BlockPos(i)
NEXT i

RESTORE RowCoordinates
FOR i = 1 TO 12
    READ BlockRows(i)
NEXT i

Increment = 1
LastScoreShown = -1
SoundOn = True
LevelDelay = 60
BackupLevelDelay = LevelDelay
BackgroundColor = _RGB32(200, 200, 200)
GameOver = False
GameEnded = False

WindSound = _SNDOPEN("wind.wav", "SYNC")
Arial16 = _LOADFONT("C:\windows\fonts\arial.ttf", 16)

'Show loading screen (while bg music is loading... currently takes a bit too long):
CurrentMatch = 6
MatchLineStart = ((13 - CurrentMatch) * 65) - 45
MatchLineEnd = MatchLineStart + BlockHeight

IF WindSound AND SoundOn THEN
    _SNDPLAYCOPY WindSound
END IF

IF Arial16 THEN
    _FONT Arial16, 0
END IF

FOR FadeStep = 1 TO BlockHeight
    _LIMIT 60
    LINE (0, MatchLineStart + FadeStep)-(599, MatchLineEnd - FadeStep), _RGB32(255, 255, 255), B
NEXT FadeStep
COLOR _RGB32(0, 0, 0)
Txt$ = "Loading..."
CenterY = 300 - _PRINTWIDTH(Txt$) \ 2
_PRINTSTRING (CenterY, MatchLineStart + BlockHeight / 2 - 8), Txt$

LINE (5, MatchLineEnd - 7)-(594, MatchLineEnd - 2), _RGB32(0, 0, 0), B

TotalItems = 12
PreviousPercentage = 0.1
ItemsLoaded = 2

Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

BoardBG = _LOADIMAGE("bg.png")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

Arial32 = _LOADFONT("C:\windows\fonts\arial.ttf", 32)
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

Whistle = _SNDOPEN("whistle.wav", "SYNC,VOL")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

Alarm = _SNDOPEN("alarm.wav", "SYNC,VOL")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

DropSound(1) = _SNDOPEN("drop1.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

DropSound(2) = _SNDOPEN("drop2.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

DropSound(3) = _SNDOPEN("drop3.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

BgMusic = _SNDOPEN("calm.ogg", "VOL,PAUSE")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

SplashSound(1) = _SNDOPEN("water1.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

SplashSound(2) = _SNDOPEN("water2.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

SplashSound(3) = _SNDOPEN("water3.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

SplashSound(4) = _SNDOPEN("water4.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
Percentage = ItemsLoaded / TotalItems
FOR i = PreviousPercentage TO Percentage STEP .1
    _LIMIT 30
    LINE (6, MatchLineEnd - 6)-(589 * i, MatchLineEnd - 3), _RGB32(0, 255, 0), BF
NEXT i
PreviousPercentage = Percentage

_SNDVOL BgMusic, 0.1
_SNDLOOP BgMusic

_SNDVOL Whistle, 0.01

RANDOMIZE TIMER

_DONTBLEND
_DEST DrawnBoard

IF BoardBG THEN
    _PUTIMAGE (0, 0), BoardBG
ELSE
    LINE (0, 0)-(599, 799), BackgroundColor, BF
END IF

ShowScore

NextShade = INT(RND * 3) + 1

DO
    CurrentColumn = INT(RND * 4) + 1
    CurrentShade = NextShade

    NextShade = INT(RND * 3) + 1

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
        IF LevelDelay THEN _LIMIT LevelDelay

        IF BoardBG THEN
            _PUTIMAGE (0, 0), BoardBG, , (0, 0)-(599, 15)
        ELSE
            LINE (0, 0)-(599, 15), BackgroundColor, BF
        END IF
        LINE (LineStart, 0)-(LineEnd, 15), Shade(CurrentShade), BF
        LineStart = LineStart + LeftSideIncrement
        LineEnd = LineEnd - RightSideIncrement
        IF INKEY$ <> "" THEN EXIT FOR
    NEXT i

    IF BoardBG THEN
        _PUTIMAGE (0, 0), BoardBG, , (0, 0)-(599, 15)
    ELSE
        LINE (0, 0)-(599, 15), BackgroundColor, BF
    END IF

    FadeStep = 0
    LevelDelay = BackupLevelDelay
    BlockPut = False
    DO
        IF LevelDelay THEN _LIMIT LevelDelay
        Y = Y + Increment
        IF Y > -48 AND Y < 21 THEN
            CurrentRow = 12
        ELSEIF Y > 20 AND Y < 85 THEN
            CurrentRow = 11
        ELSEIF Y > 85 AND Y < 150 THEN
            CurrentRow = 10
        ELSEIF Y > 150 AND Y < 215 THEN
            CurrentRow = 9
        ELSEIF Y > 215 AND Y < 280 THEN
            CurrentRow = 8
        ELSEIF Y > 280 AND Y < 345 THEN
            CurrentRow = 7
        ELSEIF Y > 345 AND Y < 410 THEN
            CurrentRow = 6
        ELSEIF Y > 410 AND Y < 475 THEN
            CurrentRow = 5
        ELSEIF Y > 475 AND Y < 540 THEN
            CurrentRow = 4
        ELSEIF Y > 540 AND Y < 605 THEN
            CurrentRow = 3
        ELSEIF Y > 605 AND Y < 670 THEN
            CurrentRow = 2
        ELSEIF Y > 670 AND Y < 735 THEN
            CurrentRow = 1
        END IF

        Dropped = False
        SELECT CASE Y
            CASE 20, 85, 150, 215, 280, 345, 410, 475, 540, 605, 670
                'Hit a block
                IF Board(CurrentRow - 1, CurrentColumn).State = True THEN Dropped = True
            CASE 735
                'Hit the ground
                Dropped = True
        END SELECT

        IF NOT Dropped THEN
            IF Y + BlockHeight = 800 THEN Dropped = True
        END IF

        IF Dropped THEN
            DropSoundI = INT(RND * 3) + 1
            IF DropSound(DropSoundI) AND SoundOn THEN
                _SNDPLAYCOPY DropSound(DropSoundI)
            END IF
            Board(CurrentRow, CurrentColumn).State = True
            Board(CurrentRow, CurrentColumn).Shade = CurrentShade
            BlockPut = True
        ELSEIF Board(12, CurrentColumn).State = True THEN
            CLS
            PRINT: PRINT "Game Over"
            GameOver = True
            _SNDSTOP BgMusic
        END IF

        IF BoardBG THEN
            _PUTIMAGE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BoardBG, , (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY)
        ELSE
            LINE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BackgroundColor, BF
        END IF

        PrevY = Y

        IF FadeStep < 256 THEN
            FadeStep = FadeStep + 1
            LINE (0, 0)-(599, 15), _RGBA(Shades(NextShade).R, Shades(NextShade).G, Shades(NextShade).B, FadeStep), BF
        END IF

        'Draw the current block
        'ShowScore
        LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), Shade(CurrentShade), BF

        _PUTIMAGE , DrawnBoard, MainBoard

        k$ = INKEY$

        SELECT CASE k$
            CASE CHR$(0) + CHR$(80), CHR$(32) 'Down arrow
                LevelDelay = 0
            CASE CHR$(0) + CHR$(75) 'Left arrow
                IF CurrentColumn > 1 THEN
                    IF Board(CurrentRow, CurrentColumn - 1).State = False THEN
                        IF BoardBG THEN
                            _PUTIMAGE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BoardBG, , (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight)
                        ELSE
                            LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                        END IF
                        CurrentColumn = CurrentColumn - 1
                    END IF
                END IF
            CASE CHR$(0) + CHR$(77) 'Right arrow
                IF CurrentColumn < 4 THEN
                    IF Board(CurrentRow, CurrentColumn + 1).State = False THEN
                        IF BoardBG THEN
                            _PUTIMAGE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BoardBG, , (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight)
                        ELSE
                            LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                        END IF
                        CurrentColumn = CurrentColumn + 1
                    END IF
                END IF
            CASE CHR$(27)
                GameEnded = True
        END SELECT
    LOOP UNTIL BlockPut OR GameEnded OR GameOver

    IF GameEnded THEN SYSTEM

    IF BlockPut THEN AddScore 2

    'Check if a block merge will be required:
    Merged = False
    IF BlockPut AND CurrentRow > 1 THEN
        DO
            IF Board(CurrentRow, CurrentColumn).Shade = Board(CurrentRow - 1, CurrentColumn).Shade THEN
                'Change block's color and the one touched to a darker shade, if any
                IF NOT GameOver AND CurrentShade < 5 THEN
                    Merged = True
                    AddScore 4
                    i = CurrentShade
                    RStep = (Shades(i).R - Shades(i + 1).R) / 255
                    GStep = (Shades(i).G - Shades(i + 1).G) / 255
                    BStep = (Shades(i).B - Shades(i + 1).B) / 255
                    YStep = (BlockHeight) / 255

                    RToGo = Shades(i).R
                    GToGo = Shades(i).G
                    BToGo = Shades(i).B

                    ShrinkingHeight = BlockHeight * 2

                    IF SplashSound(CurrentShade) AND SoundOn THEN
                        _SNDPLAYCOPY SplashSound(CurrentShade)
                    END IF

                    FOR Merge = 0 TO 255
                        _LIMIT 220
                        RToGo = RToGo - RStep
                        GToGo = GToGo - GStep
                        BToGo = BToGo - BStep
                        Y = Y + YStep
                        ShrinkingHeight = ShrinkingHeight - YStep

                        IF BoardBG THEN
                            _PUTIMAGE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BoardBG, , (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY)
                        ELSE
                            LINE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BackgroundColor, BF
                        END IF
                        PrevY = Y

                        LOCATE 5, 1
                        'Draw the merging blocks:
                        LINE (BlockPos(CurrentColumn), Y + 1)-(BlockPos(CurrentColumn) + BlockWidth, Y + ShrinkingHeight + 1), _RGB32(RToGo, GToGo, BToGo), BF
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
        LOOP UNTIL CurrentRow = 1 OR CurrentShade = 5
    END IF

    'Check for connected lines with the same shade and
    'compute the new score, besides generating the disappearing
    'animation:
    Matched = False
    DO
        CurrentMatch = CheckMatchingLine
        IF CurrentMatch = 0 THEN EXIT DO

        Matched = True
        AddScore 200
        MatchLineStart = ((13 - CurrentMatch) * 65) - 45
        MatchLineEnd = MatchLineStart + BlockHeight

        IF WindSound AND SoundOn THEN
            _SNDPLAYCOPY WindSound
        END IF

        LINE (0, MatchLineStart)-(599, MatchLineEnd), _RGBA(0, 0, 0, FadeStep), B
        FOR FadeStep = 1 TO BlockHeight
            _LIMIT 60
            LINE (0, MatchLineStart + FadeStep)-(599, MatchLineEnd - FadeStep), _RGB32(0, 0, 0), B
        NEXT FadeStep

        DestroyLine CurrentMatch
        RedrawBoard

        DropSoundI = INT(RND * 3) + 1
        IF DropSound(DropSoundI) AND SoundOn THEN
            _SNDPLAYCOPY DropSound(DropSoundI)
        END IF
    LOOP

    IF NOT Matched AND NOT Merged THEN
        IF CurrentRow = 12 THEN
            IF Alarm AND SoundOn THEN
                _SNDPLAYCOPY Alarm
            END IF
        END IF
    END IF
LOOP UNTIL GameOver OR GameEnded
SYSTEM

Yellow:
DATA 15,68,91,43,6

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

FUNCTION Shade~& (CurrentShade)
Shade~& = _RGBA(Shades(CurrentShade).R, Shades(CurrentShade).G, Shades(CurrentShade).B, 255)
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
            IF BoardBG THEN
                _PUTIMAGE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), BoardBG, , (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY)
            ELSE
                LINE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), BackgroundColor, BF
            END IF
        END IF
    NEXT CurrentColumn
NEXT i

END SUB

SUB ShowBoardInfo

DIM i AS INTEGER, CurrentColumn AS INTEGER
DIM StartY AS INTEGER, EndY AS INTEGER

FOR i = 1 TO 12
    FOR CurrentColumn = 4 TO 1 STEP -1
        StartY = ((13 - i) * 65) - 45
        EndY = StartY + BlockHeight

        IF Board(i, CurrentColumn).State = True THEN
            COLOR _RGB32(255, 255, 255)
            _PRINTSTRING (BlockPos(CurrentColumn) + 10, StartY), STR$(Board(i, CurrentColumn).Shade)
        ELSE
            COLOR _RGB32(0, 0, 0)
            _PRINTSTRING (BlockPos(CurrentColumn) + 10, StartY), "Empty"
        END IF
    NEXT CurrentColumn
NEXT i

END SUB


SUB AddScore (Points)
Score = Score + Points
ShowScore
END SUB

SUB ShowScore

COLOR _RGB32(100, 100, 100)
ScoreTxt$ = LTRIM$(STR$(Score))
TxtWidth = _PRINTWIDTH(ScoreTxt$) * 3
IF Arial32 THEN TxtHeight = _FONTHEIGHT(Arial32)
CenterY = 300 - TxtWidth \ 2
ScoreHeight = 40

'IF BoardBG THEN
'    _PUTIMAGE (5, ScoreHeight)-(CenterY + TxtWidth, ScoreHeight + TxtHeight), BoardBG, , (5, ScoreHeight)-(CenterY + TxtWidth, ScoreHeight + TxtHeight)
'ELSE
'    LINE (5, ScoreHeight)-(CenterY + TxtWidth, ScoreHeight + TxtHeight), BackgroundColor, BF
'END IF

ScoreBoard = _COPYIMAGE(DrawnBoard)
_DEST ScoreBoard

IF Arial32 THEN
    _FONT Arial32
END IF
_PRINTSTRING (CenterY, ScoreHeight), ScoreTxt$
_PRINTSTRING (5, ScoreHeight), "Level 1"

_DEST MainGame
_PUTIMAGE , ScoreBoard
_FREEIMAGE ScoreBoard
_DEST DrawnBoard
END SUB
