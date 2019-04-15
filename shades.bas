_TITLE "Cloned Shades"
SCREEN _NEWIMAGE(600, 800, 32)
_SCREENMOVE _MIDDLE
_PRINTMODE _KEEPBACKGROUND

CONST True = -1
CONST False = 0
CONST BlockWidth = 150
CONST BlockHeight = 64
CONST TopAnimationSteps = 60
CONST AnimationDelay = .005

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
DIM i AS INTEGER, LevelDelay AS SINGLE, Increment AS INTEGER
DIM SHARED DropSound(1 TO 3) AS LONG, SoundOn AS INTEGER, Alarm AS LONG
DIM SHARED WindSound AS LONG, SplashSound(1 TO 4) AS LONG, Whistle AS LONG
DIM BgMusic(1 TO 2) AS LONG
DIM CurrentRow AS INTEGER
DIM SHARED Board(1 TO 12, 1 TO 4) AS BoardType
DIM BlockPut AS INTEGER

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
SoundOn = True
LevelDelay = .005
BackupLevelDelay = LevelDelay
BackgroundColor = _RGB32(200, 200, 200)
GameOver = False
GameEnded = False

WindSound = _SNDOPEN("wind.wav", "SYNC")

'Show loading screen (while bg music is loading... currently takes too long):
CurrentMatch = 6
MatchLineStart = ((13 - CurrentMatch) * 65) - 45
MatchLineEnd = MatchLineStart + BlockHeight

IF WindSound AND SoundOn THEN
    _SNDPLAYCOPY WindSound
END IF

FOR FadeStep = 1 TO BlockHeight
    _LIMIT 60
    LINE (0, MatchLineStart + FadeStep)-(599, MatchLineEnd - FadeStep), _RGB32(255, 255, 255), B
NEXT FadeStep
COLOR _RGB32(0, 0, 0)
_PRINTSTRING (250, MatchLineStart + BlockHeight / 2 - 8), "Loading..."

LINE (5, MatchLineEnd - 7)-(594, MatchLineEnd - 2), _RGB32(0, 0, 0), B

TotalItems = 11
ItemsLoaded = 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

Whistle = _SNDOPEN("whistle.wav", "SYNC,VOL")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

Alarm = _SNDOPEN("alarm.wav", "SYNC,VOL")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

DropSound(1) = _SNDOPEN("drop1.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

DropSound(2) = _SNDOPEN("drop2.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

DropSound(3) = _SNDOPEN("drop3.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

BgMusic(1) = _SNDOPEN("calm.mp3", "VOL,PAUSE,SETPOS")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

SplashSound(1) = _SNDOPEN("water1.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

SplashSound(2) = _SNDOPEN("water2.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

SplashSound(3) = _SNDOPEN("water3.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

SplashSound(4) = _SNDOPEN("water4.wav", "SYNC")
ItemsLoaded = ItemsLoaded + 1
LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

'BgMusic(2) = _SNDOPEN("danger.mp3", "VOL,PAUSE,SETPOS")
'ItemsLoaded = ItemsLoaded + 1
'LINE (6, MatchLineEnd - 6)-(593 * (ItemsLoaded / TotalItems), MatchLineEnd - 3), _RGB32(0, 255, 0), BF

_SNDVOL BgMusic(1), 0.1
_SNDVOL BgMusic(2), 0.1
_SNDLOOP BgMusic(1)

_SNDVOL Whistle, 0.01

RANDOMIZE TIMER

LINE (0, 0)-(599, 799), BackgroundColor, BF

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
        _LIMIT 60
        LINE (0, 0)-(599, 15), BackgroundColor, BF
        LINE (LineStart, 0)-(LineEnd, 15), Shade(CurrentShade), BF
        LineStart = LineStart + LeftSideIncrement
        LineEnd = LineEnd - RightSideIncrement
        IF INKEY$ <> "" THEN EXIT FOR
    NEXT i
    LINE (0, 0)-(599, 15), BackgroundColor, BF

    FadeStep = 0
    LevelDelay = BackupLevelDelay
    BlockPut = False
    DO
        Y = Y + Increment
        LOCATE 10, 1

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

        IF (POINT(BlockPos(CurrentColumn), Y + BlockHeight + 1) <> BackgroundColor) OR (Y + BlockHeight_1 = 800) THEN
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
        LINE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BackgroundColor, BF
        PrevY = Y

        IF FadeStep < 256 THEN
            FadeStep = FadeStep + 1
            LINE (0, 0)-(599, 15), _RGBA(Shades(NextShade).R, Shades(NextShade).G, Shades(NextShade).B, FadeStep), BF
        END IF

        'Draw the current block
        LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), Shade(CurrentShade), BF

        _DELAY LevelDelay

        k$ = INKEY$

        SELECT CASE k$
            CASE CHR$(0) + CHR$(80), CHR$(32) 'Down arrow
                LevelDelay = 0
            CASE CHR$(0) + CHR$(75) 'Left arrow
                IF CurrentColumn > 1 THEN
                    IF Board(CurrentRow, CurrentColumn - 1).State = False THEN
                        LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                        CurrentColumn = CurrentColumn - 1
                    END IF
                END IF
            CASE CHR$(0) + CHR$(77) 'Right arrow
                IF CurrentColumn < 4 THEN
                    IF Board(CurrentRow, CurrentColumn + 1).State = False THEN
                        LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
                        CurrentColumn = CurrentColumn + 1
                    END IF
                END IF
            CASE CHR$(27)
                GameEnded = True
        END SELECT
    LOOP UNTIL BlockPut OR GameEnded OR GameOver

    'Check if a block merge will be required:
    Merged = False
    IF BlockPut AND CurrentRow > 1 THEN
        DO
            IF Board(CurrentRow, CurrentColumn).Shade = Board(CurrentRow - 1, CurrentColumn).Shade THEN
                'Change block's color and the one touched to a darker shade, if any
                IF CurrentShade < 5 THEN
                    Merged = True
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
                        _LIMIT 120
                        RToGo = RToGo - RStep
                        GToGo = GToGo - GStep
                        BToGo = BToGo - BStep
                        Y = Y + YStep
                        ShrinkingHeight = ShrinkingHeight - YStep

                        LINE (BlockPos(CurrentColumn), PrevY)-(BlockPos(CurrentColumn) + BlockWidth, PrevY), BackgroundColor, BF
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

    'ShowBoardInfo

LOOP UNTIL GameOver OR GameEnded

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
            LINE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), BackgroundColor, BF
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

