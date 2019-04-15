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
'
' - Beta 4
'       - Adaptative resolution when the desktop isn't at least 900px tall.
'       - New shades, which are alternated every time a new game is started.
'       - Visual intro, mimicking the original game's.
'       - Improved game performance by selectively limiting the layering of
'         images in SUB UpdateScreen.
'       - Added a "danger mode" visual indication, by turning on a TIMER that
'         overlays a shade of red over the game play, similar to a security
'         alarm.
'       - Added a menu to start the game or change setting or leave.
'       - Settings are now saved to "shades.dat", and include switches for
'         sound and music, as well as a highscore.
'       - Added an end screen, that shows the score, number of merges and
'         number of lines destroyed during game.
'
' - Beta 5
'       - Fixed game starting with the blocks that were put during the menu
'         demonstration.
'       - Fixed the 'danger' warning coming back from the previous game.
'       - Added an option to select shades (GREEN, ORANGE, BLUE, PINK) or
'         to have it AUTOmatically rotate everytime you start the game.
'         (thanks to Clippy for suggesting it)
'       - Added a confirmation message before clearing highscore.
'       - Added a confirmation message before closing the game with ESC.
'       - Fixed a bug with page _DEST that caused scores to be put in
'         OverlayGraphics page instead of InfoScreen while InDanger
'         triggered the ShowAlert sub.
'
_TITLE "Cloned Shades"

'Game constants -------------------------------------------------------
'General Use:
CONST False = 0
CONST True = NOT False

'Game definitions:
CONST BlockWidth = 150
CONST BlockHeight = 64
CONST InitialIncrement = 1

'Animations:
CONST TopAnimationSteps = 15
CONST MergeSteps = 32

'Colors:
CONST BackgroundColor = _RGB32(170, 170, 170)
CONST MaxShades = 4

'Menu actions:
CONST PLAYGAME = 1
CONST SETTINGSMENU = 2
CONST LEAVEGAME = 3
CONST SWITCHSOUND = 1
CONST SWITCHMUSIC = 2
CONST COLORSWITCH = 3
CONST RESETHIGHSCORES = 4
CONST MAINMENU = 5

'Misc:
CONST FileID = "CLONEDSHADES"

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

TYPE SettingsFile
    ID AS STRING * 13
    ColorMode AS INTEGER '0 = Automatic, 1 = Green, 2 = Orange, 3 = Blue, 4 = Pink
    SoundOn AS _BYTE
    MusicOn AS _BYTE
    Highscore AS LONG
END TYPE

'Variables ------------------------------------------------------------
'Variables for game control:
DIM SHARED Board(1 TO 12, 1 TO 4) AS BoardType
DIM SHARED Shades(1 TO 5) AS ColorRGB, FadeStep AS INTEGER
DIM SHARED BlockPos(1 TO 4) AS INTEGER
DIM SHARED BlockRows(1 TO 12) AS INTEGER, BgImage AS LONG
DIM SHARED i AS INTEGER, Increment AS INTEGER
DIM SHARED CurrentRow AS INTEGER, CurrentColumn AS INTEGER
DIM SHARED BlockPut AS _BIT, Y AS INTEGER, PrevY AS INTEGER
DIM SHARED CurrentShade AS INTEGER, NextShade AS INTEGER
DIM SHARED AlignedWithRow AS _BIT, InDanger AS _BIT
DIM SHARED GameOver AS _BIT, GameEnded AS _BIT
DIM SHARED PreviousScore AS LONG, Score AS LONG
DIM SHARED GlobalShade AS INTEGER, DemoMode AS _BIT
DIM SHARED AlertTimer AS INTEGER, TotalMerges AS LONG
DIM SHARED TotalLines AS LONG, Setting AS INTEGER
DIM SHARED InGame AS _BYTE

'Variables for screen pages:
DIM SHARED InfoScreen AS LONG
DIM SHARED OverlayGraphics AS LONG
DIM SHARED GameScreen AS LONG
DIM SHARED MainScreen AS LONG
DIM SHARED UIWidth AS INTEGER
DIM SHARED UIHeight AS INTEGER

'Variable for sound:
DIM SHARED DropSound(1 TO 3) AS LONG, Alarm AS LONG
DIM SHARED WindSound AS LONG, SplashSound(1 TO 4) AS LONG, Whistle AS LONG
DIM SHARED BgMusic(1 TO 2) AS LONG

'Other variables
DIM SHARED InMenu AS _BIT, QuitGame AS _BIT
DIM SHARED Settings AS SettingsFile
DIM SettingChoice AS INTEGER

'Screen initialization: ------------------------------------------------
'Default window size is 600x800. If the desktop resolution is smaller
'than 900px tall, resize the UI while keeping the aspect ratio.
IF _HEIGHT(_SCREENIMAGE) < 900 THEN
    UIHeight = _HEIGHT(_SCREENIMAGE) - 300
    UIWidth = UIHeight * .75
ELSE
    UIWidth = 600
    UIHeight = 800
END IF

InfoScreen = _NEWIMAGE(300, 400, 32)
OverlayGraphics = _NEWIMAGE(150, 200, 32)
GameScreen = _NEWIMAGE(600, 800, 32)
MainScreen = _NEWIMAGE(UIWidth, UIHeight, 32)

BgImage = _LOADIMAGE("bg.png", 32)
IF BgImage < -1 THEN _DONTBLEND BgImage

SCREEN MainScreen

IF BgImage < -1 THEN _PUTIMAGE , BgImage, MainScreen

'Coordinates for block locations in the board: ------------------------
RESTORE BlockPositions
FOR i = 1 TO 4
    READ BlockPos(i)
NEXT i

RESTORE RowCoordinates
FOR i = 1 TO 12
    READ BlockRows(i)
NEXT i

InDanger = False
GameOver = False
GameEnded = False

'Read settings from file "shades.dat", if it exists: ------------------
IF _FILEEXISTS("shades.dat") THEN
    OPEN "shades.dat" FOR BINARY AS #1
    GET #1, , Settings
    CLOSE #1
END IF

IF Settings.ID <> FileID + CHR$(5) THEN
    'Invalid settings file, use defaults
    Settings.ID = FileID + CHR$(5)
    Settings.ColorMode = 0
    Settings.SoundOn = True
    Settings.MusicOn = True
    Settings.Highscore = 0
END IF

'RGB data for shades: --------------------------------------------------
SelectGlobalShade

PrepareIntro

'Load sounds and images: ----------------------------------------------
LoadAssets

'Since now we already have read the shades' rgb data,
'let's generate the window icon:
MakeIcon

Intro

NextShade = INT(RND * 3) + 1 'Randomly chooses a shade for the next block

AlertTimer = _FREETIMER
ON TIMER(AlertTimer, .005) ShowAlert
TIMER(AlertTimer) OFF

_DEST GameScreen
IF BgImage < -1 THEN _PUTIMAGE , BgImage, GameScreen ELSE CLS , BackgroundColor
UpdateScreen

RANDOMIZE TIMER

'Main game loop: ------------------------------------------------------
DO
    WHILE INKEY$ <> "": WEND 'Clears keyboard buffer to avoid unwanted ESCs
    SelectGlobalShade
    ERASE Board
    REDIM Choices(1 TO 3) AS STRING
    Choices(1) = "Play game"
    Choices(2) = "Settings"
    Choices(3) = "Quit"

    Choice = Menu(1, 3, Choices())
    SELECT CASE Choice
        CASE PLAYGAME
            ERASE Board
            Score = 0
            PreviousScore = -1
            TotalMerges = 0
            TotalLines = 0
            NextShade = INT(RND * 3) + 1
            RedrawBoard
            InDanger = False
            InGame = True
            IF Settings.MusicOn THEN _SNDLOOP BgMusic(1)
            DO
                GenerateNewBlock
                MoveBlock
                CheckDanger
                CheckMerge
                CheckConnectedLines
            LOOP UNTIL GameOver OR GameEnded
            InGame = False
            IF BgMusic(1) THEN _SNDSTOP BgMusic(1)
            IF BgMusic(2) THEN _SNDSTOP BgMusic(2)
            IF GameOver THEN
                IF Settings.Highscore < Score THEN Settings.Highscore = Score
                ShowEndScreen
            END IF
            GameOver = False
            GameEnded = False
        CASE SETTINGSMENU
            SettingChoice = 1
            DO
                REDIM Choices(1 TO 5) AS STRING
                IF Settings.SoundOn THEN Choices(1) = "Sound: ON" ELSE Choices(1) = "Sound: OFF"
                IF Settings.MusicOn THEN Choices(2) = "Music: ON" ELSE Choices(2) = "Music: OFF"
                SELECT CASE Settings.ColorMode
                    CASE 0: Choices(3) = "Color: AUTO"
                    CASE 1: Choices(3) = "Color: GREEN"
                    CASE 2: Choices(3) = "Color: ORANGE"
                    CASE 3: Choices(3) = "Color: BLUE"
                    CASE 4: Choices(3) = "Color: PINK"
                END SELECT
                Choices(4) = "Reset Highscores": IF Settings.Highscore = 0 THEN Choices(4) = Choices(4) + CHR$(0)
                Choices(5) = "Return"

                SettingChoice = Menu(SettingChoice, 5, Choices())
                SELECT CASE SettingChoice
                    CASE SWITCHSOUND
                        Settings.SoundOn = NOT Settings.SoundOn
                    CASE SWITCHMUSIC
                        Settings.MusicOn = NOT Settings.MusicOn
                    CASE COLORSWITCH
                        Settings.ColorMode = Settings.ColorMode + 1
                        IF Settings.ColorMode > 4 THEN Settings.ColorMode = 0
                        SelectGlobalShade
                    CASE RESETHIGHSCORES
                        REDIM Choices(1 TO 2) AS STRING
                        Choices(1) = "Reset"
                        Choices(2) = "Cancel"
                        IF Menu(1, 2, Choices()) = 1 THEN Settings.Highscore = 0
                END SELECT
            LOOP UNTIL SettingChoice = MAINMENU
        CASE LEAVEGAME
            QuitGame = True
    END SELECT
LOOP UNTIL QuitGame

ON ERROR GOTO DontSave
OPEN "shades.dat" FOR BINARY AS #1
PUT #1, , Settings
CLOSE #1

DontSave:
SYSTEM

Greens:
DATA 245,245,204
DATA 158,255,102
DATA 107,204,51
DATA 58,153,0
DATA 47,127,0

Oranges:
DATA 255,193,153
DATA 255,162,102
DATA 255,115,26
DATA 230,89,0
DATA 128,49,0

Blues:
DATA 204,229,255
DATA 128,190,255
DATA 26,138,255
DATA 0,87,179
DATA 0,50,102

Pinks:
DATA 255,179,255
DATA 255,128,255
DATA 255,26,255
DATA 179,0,178
DATA 77,0,76

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

IF DemoMode THEN EXIT SUB

'Animate the birth of a new block:
IF Whistle AND Settings.SoundOn THEN
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
    _LIMIT 120
    IF BgImage < -1 THEN _PUTIMAGE (0, 0)-(599, 15), BgImage, GameScreen, (0, 0)-(599, 15) ELSE LINE (0, 0)-(599, 15), BackgroundColor, BF
    LINE (LineStart, 0)-(LineEnd, 15), Shade&(CurrentShade), BF
    LineStart = LineStart + LeftSideIncrement
    LineEnd = LineEnd - RightSideIncrement
    IF INKEY$ <> "" THEN EXIT FOR
    UpdateScreen
NEXT i
IF BgImage < -1 THEN _PUTIMAGE (0, 0)-(599, 15), BgImage, GameScreen, (0, 0)-(599, 15) ELSE LINE (0, 0)-(599, 15), BackgroundColor, BF
END SUB

SUB MoveBlock
'DIM MX AS INTEGER, MY AS INTEGER, MB AS INTEGER 'Mouse X, Y and Button
'DIM PreviousMX 'Previously detected mouse position
'DIM HighlightedCol AS INTEGER

DIM k$

FadeStep = 0
Increment = InitialIncrement
IF NOT DemoMode THEN BlockPut = False
'HighLightCol CurrentColumn
'HighlightedCol = CurrentColumn
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

    'Mouse routines:
    'WHILE _MOUSEINPUT: WEND
    'MX = _MOUSEX: MY = _MOUSEY: MB = _MOUSEBUTTON(1)
    'IF MX <> PreviousMX AND (MY > 0 AND MY < _HEIGHT(0)) THEN
    '    IF ConvertXtoCol(MX) <> HighlightedCol AND ConvertXtoCol(MX) > 0 THEN
    '        HighLightCol ConvertXtoCol(MX)
    '        HighlightedCol = ConvertXtoCol(MX)
    '        IF Board(CurrentRow, HighlightedCol).State = False THEN
    '            IF BgImage < -1 THEN _PUTIMAGE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BgImage, GameScreen, (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight) ELSE LINE (BlockPos(CurrentColumn), Y)-(BlockPos(CurrentColumn) + BlockWidth, Y + BlockHeight), BackgroundColor, BF
    '            CurrentColumn = HighlightedCol
    '        END IF
    '    END IF
    '    PreviousMX = MX
    'END IF

    'IF MB THEN Increment = BlockHeight

    CurrentRow = ConvertYtoRow(Y)

    IF AlignedWithRow THEN
        IF CurrentRow > 1 THEN
            IF Board(CurrentRow - 1, CurrentColumn).State THEN BlockPut = True
        ELSEIF CurrentRow = 1 THEN
            BlockPut = True
        END IF
    END IF

    IF BlockPut THEN
        Score = Score + 2
        DropSoundI = INT(RND * 3) + 1
        IF DropSound(DropSoundI) AND Settings.SoundOn AND NOT DemoMode THEN
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

    'Show the next shade on the top bar unless in DemoMode
    IF FadeStep < 255 AND NOT DemoMode THEN
        FadeStep = FadeStep + 1
        LINE (0, 0)-(599, 15), _RGBA32(Shades(NextShade).R, Shades(NextShade).G, Shades(NextShade).B, FadeStep), BF
    END IF

    'Draw the current block
    LINE (BlockPos(CurrentColumn), Y)-STEP(BlockWidth, BlockHeight), Shade&(CurrentShade), BF

    UpdateScreen

    IF NOT DemoMode AND Increment < BlockHeight THEN k$ = INKEY$

    SELECT CASE k$
        CASE CHR$(0) + CHR$(80) 'Down arrow
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
            REDIM Choices(1 TO 2) AS STRING
            Choices(1) = "Resume"
            Choices(2) = "Quit"
            IF Menu(1, 2, Choices()) = 2 THEN GameEnded = True
            RedrawBoard
            'CASE " "
            '    GameOver = True
    END SELECT
    IF DemoMode THEN EXIT SUB
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
                TotalMerges = TotalMerges + 1
                i = CurrentShade
                RStep = (Shades(i).R - Shades(i + 1).R) / MergeSteps
                GStep = (Shades(i).G - Shades(i + 1).G) / MergeSteps
                BStep = (Shades(i).B - Shades(i + 1).B) / MergeSteps
                YStep = (BlockHeight) / MergeSteps

                RToGo = Shades(i).R
                GToGo = Shades(i).G
                BToGo = Shades(i).B

                ShrinkingHeight = BlockHeight * 2

                IF SplashSound(CurrentShade) AND Settings.SoundOn AND NOT DemoMode THEN
                    _SNDPLAYCOPY SplashSound(CurrentShade)
                END IF

                FOR Merge = 0 TO MergeSteps
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
                    LINE (BlockPos(CurrentColumn), BlockRows(CurrentRow) + (BlockHeight * 2) - ShrinkingHeight - 1)-STEP(BlockWidth, ShrinkingHeight + 2), _RGB32(RToGo, GToGo, BToGo), BF
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

    IF WindSound AND Settings.SoundOn AND NOT DemoMode THEN
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
    TotalLines = TotalLines + 1
    RedrawBoard

    DropSoundI = INT(RND * 3) + 1
    IF DropSound(DropSoundI) AND Settings.SoundOn AND NOT DemoMode THEN
        _SNDPLAYCOPY DropSound(DropSoundI)
    END IF
    IF DemoMode THEN DemoMode = False
LOOP
END SUB

FUNCTION ConvertYtoRow (CurrentY)
'Returns the row on the board through which the block is currently
'passing.

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

FUNCTION ConvertXtoCol (CurrentX)
'Returns the column on the board being currently hovered

IF CurrentX >= BlockPos(1) AND CurrentX < BlockPos(2) THEN
    ConvertXtoCol = 1
ELSEIF CurrentX >= BlockPos(2) AND CurrentX < BlockPos(3) THEN
    ConvertXtoCol = 2
ELSEIF CurrentX >= BlockPos(3) AND CurrentX < BlockPos(4) THEN
    ConvertXtoCol = 3
ELSEIF CurrentX >= BlockPos(4) THEN
    ConvertXtoCol = 4
END IF
END FUNCTION


FUNCTION Shade& (CurrentShade)
Shade& = _RGB32(Shades(CurrentShade).R, Shades(CurrentShade).G, Shades(CurrentShade).B)
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

IF BgImage < -1 THEN _PUTIMAGE , BgImage, GameScreen ELSE CLS , BackgroundColor

FOR i = 1 TO 12
    FOR CurrentColumn = 4 TO 1 STEP -1
        StartY = BlockRows(i)
        EndY = StartY + BlockHeight

        IF Board(i, CurrentColumn).State = True THEN
            LINE (BlockPos(CurrentColumn), StartY)-(BlockPos(CurrentColumn) + BlockWidth, EndY), Shade&(Board(i, CurrentColumn).Shade), BF
        END IF
    NEXT CurrentColumn
NEXT i

END SUB

SUB ShowScore
DIM ScoreString AS STRING

IF Score = PreviousScore THEN EXIT SUB
PreviousScore = Score

ScoreString = "Score:" + STR$(Score)

_DEST InfoScreen
CLS , _RGBA32(0, 0, 0, 0)

'_FONT 16
PrintShadow 15, 15, ScoreString, _RGB32(255, 255, 255)

_FONT 8
IF Score < Settings.Highscore THEN
    PrintShadow 15, 32, "Highscore: " + TRIM$(Settings.Highscore), _RGB32(255, 255, 255)
ELSEIF Score > Settings.Highscore AND Settings.Highscore > 0 THEN
    PrintShadow 15, 32, "You beat the highscore!", _RGB32(255, 255, 255)
END IF
_FONT 16
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
    LINE (0, i * (IconSize / 5) - (IconSize / 5))-(IconSize, i * (IconSize / 5)), Shade&(i), BF
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
    IF Settings.SoundOn AND NOT InDanger AND NOT DemoMode THEN
        IF Alarm THEN _SNDPLAYCOPY Alarm
        IF Settings.MusicOn THEN
            IF BgMusic(1) THEN _SNDSTOP BgMusic(1)
            IF BgMusic(2) THEN _SNDLOOP BgMusic(2)
        END IF
        TIMER(AlertTimer) ON
    END IF
    InDanger = True
ELSE
    IF Settings.MusicOn AND InDanger AND NOT DemoMode THEN
        IF BgMusic(2) THEN _SNDSTOP BgMusic(2)
        IF BgMusic(1) THEN _SNDLOOP BgMusic(1)
        TIMER(AlertTimer) OFF
        _DEST OverlayGraphics
        CLS , _RGBA32(0, 0, 0, 0)
        _DEST GameScreen
    END IF
    InDanger = False
END IF
END SUB

SUB LoadAssets
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
END SUB

SUB UpdateScreen
'Display the gamescreen, overlay and score layers
IF NOT DemoMode THEN ShowScore

_PUTIMAGE , GameScreen, MainScreen
IF InMenu OR InDanger THEN _PUTIMAGE , OverlayGraphics, MainScreen
IF NOT InMenu THEN _PUTIMAGE , InfoScreen, MainScreen
_DISPLAY
END SUB

SUB PrintShadow (x%, y%, Text$, ForeColor&)
'Shadow:
COLOR _RGBA32(170, 170, 170, 170), _RGBA32(0, 0, 0, 0)
_PRINTSTRING (x% + 1, y% + 1), Text$

'Text:
COLOR ForeColor&, _RGBA32(0, 0, 0, 0)
_PRINTSTRING (x%, y%), Text$
END SUB

SUB SelectGlobalShade
IF Settings.ColorMode = 0 THEN
    GlobalShade = (GlobalShade) MOD MaxShades + 1
ELSE
    GlobalShade = Settings.ColorMode
END IF
SELECT CASE GlobalShade
    CASE 1: RESTORE Greens
    CASE 2: RESTORE Oranges
    CASE 3: RESTORE Blues
    CASE 4: RESTORE Pinks
END SELECT

FOR i = 1 TO 5
    READ Shades(i).R
    READ Shades(i).G
    READ Shades(i).B
NEXT i

END SUB

SUB PrepareIntro
'The intro shows the board about to be cleared,
'which then happens after assets are loaded. The intro
'is generated using the game engine.

'DemoMode prevents sounds to be played
DemoMode = True

_DEST InfoScreen
_FONT 16
LoadingMessage$ = "Cloned Shades"
PrintShadow _WIDTH \ 2 - _PRINTWIDTH(LoadingMessage$) \ 2, _HEIGHT \ 2 - _FONTHEIGHT, LoadingMessage$, _RGB32(255, 255, 255)

_FONT 8
LoadingMessage$ = "loading..."
PrintShadow _WIDTH \ 2 - _PRINTWIDTH(LoadingMessage$) \ 2, _HEIGHT \ 2, LoadingMessage$, _RGB32(255, 255, 255)

_FONT 16
_DEST GameScreen

'Setup the board to show an "about to merge" group of blocks
'which will end up completing a dark line at the bottom.
Board(1, 1).State = True
Board(1, 1).Shade = 5
Board(1, 2).State = True
Board(1, 2).Shade = 5
Board(1, 3).State = True
Board(1, 3).Shade = 4
Board(1, 4).State = True
Board(1, 4).Shade = 5
Board(2, 3).State = True
Board(2, 3).Shade = 3
Board(3, 3).State = True
Board(3, 3).Shade = 2
Board(4, 3).State = True
Board(4, 3).Shade = 2

CurrentColumn = 3
CurrentRow = 4
CurrentShade = 2
Y = BlockRows(CurrentRow)
PrevY = Y
BlockPut = True

RedrawBoard
Board(4, 3).State = False

_SCREENMOVE _MIDDLE

UpdateScreen
END SUB

SUB Intro
'The current board setup must have been prepared using PrepareIntro first.

'Use the game engine to show the intro:
CheckMerge
CheckConnectedLines

'Clear the "loading..." text
_DEST InfoScreen
CLS , _RGBA32(0, 0, 0, 0)
_DEST GameScreen

END SUB

SUB HighLightCol (Col AS INTEGER)

_DEST OverlayGraphics
CLS , _RGBA32(0, 0, 0, 0)
LINE (BlockPos(Col), 16)-STEP(BlockWidth, _HEIGHT(0)), _RGBA32(255, 255, 255, 150), BF
_DEST GameScreen

END SUB

SUB ShowAlert
STATIC FadeLevel
DIM DangerMessage$
DIM PreviousDest AS LONG

IF InMenu THEN EXIT SUB

IF FadeLevel > 100 THEN FadeLevel = 0
FadeLevel = FadeLevel + 1
PreviousDest = _DEST
_DEST OverlayGraphics
CLS , _RGBA32(255, 0, 0, FadeLevel)
DangerMessage$ = "DANGER!"
PrintShadow _WIDTH \ 2 - _PRINTWIDTH(DangerMessage$) \ 2, _HEIGHT \ 2 - _FONTHEIGHT \ 2, DangerMessage$, _RGB32(255, 255, 255)
_DEST PreviousDest
END SUB

FUNCTION Menu (CurrentChoice AS INTEGER, MaxChoice AS INTEGER, Choices() AS STRING)
'Displays Choices() on the screen and lets the player choose one.
'Uses OverlayGraphics page to display options.
'Player must use arrow keys to make a choice then ENTER.

DIM Choice AS INTEGER
DIM ChoiceWasMade AS _BIT
DIM k$, i AS INTEGER
DIM ChooseColorTimer AS INTEGER
DIM ItemShade AS LONG

DemoMode = True
InMenu = True
Choice = CurrentChoice

IF NOT InGame THEN
    ChooseColorTimer = _FREETIMER
    ON TIMER(ChooseColorTimer, 3.5) SelectGlobalShade
    TIMER(ChooseColorTimer) ON
END IF

IF NOT InGame THEN BlockPut = True
DO
    _LIMIT 30

    'Use the game engine while the menu is displayed, except while InGame:
    IF NOT InGame THEN
        IF BlockPut THEN GenerateNewBlock: BlockPut = False
        IF NOT BlockPut THEN MoveBlock
        IF BlockPut THEN CheckMerge
        IF BlockPut THEN CheckConnectedLines
    END IF

    GOSUB ShowCurrentChoice
    k$ = INKEY$
    SELECT CASE k$
        CASE CHR$(0) + CHR$(80) 'Down arrow
            DO
                Choice = (Choice) MOD MaxChoice + 1
            LOOP WHILE RIGHT$(Choices(Choice), 1) = CHR$(0)
        CASE CHR$(0) + CHR$(72) 'Up arrow
            DO
                Choice = (Choice + MaxChoice - 2) MOD MaxChoice + 1
            LOOP WHILE RIGHT$(Choices(Choice), 1) = CHR$(0)
        CASE CHR$(13) 'Enter
            ChoiceWasMade = True
        CASE CHR$(27) 'ESC
            ChoiceWasMade = True
            Choice = MaxChoice
    END SELECT
LOOP UNTIL ChoiceWasMade

IF NOT InGame THEN TIMER(ChooseColorTimer) FREE
InMenu = False
DemoMode = False
_DEST OverlayGraphics
CLS , _RGBA32(255, 255, 255, 100)
_DEST GameScreen

Menu = Choice
EXIT FUNCTION

ShowCurrentChoice:
_DEST OverlayGraphics
CLS , _RGBA32(255, 255, 255, 100)

'Choices ending with CHR$(0) are shown as unavailable/grey.
FOR i = 1 TO MaxChoice
    IF i = Choice THEN
        IF RIGHT$(Choices(i), 1) = CHR$(0) THEN ItemShade = BackgroundColor ELSE ItemShade = Shade&(5)
        PrintShadow (_WIDTH(OverlayGraphics) \ 2) - (_PRINTWIDTH("> " + Choices(i)) \ 2), (_HEIGHT(OverlayGraphics) \ 2) - (_FONTHEIGHT \ 2) - ((MaxChoice - i) * _FONTHEIGHT - _FONTHEIGHT), ">" + Choices(i), ItemShade
    ELSE
        IF RIGHT$(Choices(i), 1) = CHR$(0) THEN ItemShade = BackgroundColor ELSE ItemShade = Shade&(4)
        PrintShadow (_WIDTH(OverlayGraphics) \ 2) - (_PRINTWIDTH(Choices(i)) \ 2), (_HEIGHT(OverlayGraphics) \ 2) - (_FONTHEIGHT \ 2) - ((MaxChoice - i) * _FONTHEIGHT - _FONTHEIGHT), Choices(i), ItemShade
    END IF

NEXT i
_DEST GameScreen
UpdateScreen
RETURN

END FUNCTION

SUB ShowEndScreen
DIM Message$(1 TO 10), k$, i AS INTEGER
DIM MessageColor AS LONG

_DEST OverlayGraphics
CLS , _RGBA32(255, 255, 255, 150)

Message$(1) = "GAME OVER"
Message$(3) = "Your score:"
Message$(4) = TRIM$(Score)
Message$(5) = "Merged blocks:"
Message$(6) = TRIM$(TotalMerges)
Message$(7) = "Lines destroyed:"
Message$(8) = TRIM$(TotalLines)
Message$(10) = "Press ENTER..."

MessageColor = Shade&(5)
FOR i = 1 TO UBOUND(message$)
    IF i > 1 THEN _FONT 8: MessageColor = _RGB32(255, 255, 255)
    IF i = UBOUND(message$) THEN _FONT 16: MessageColor = Shade&(5)
    PrintShadow (_WIDTH(OverlayGraphics) \ 2) - (_PRINTWIDTH(Message$(i)) \ 2), i * 16, Message$(i), MessageColor
NEXT i

_DEST GameScreen
_PUTIMAGE , GameScreen, MainScreen
_PUTIMAGE , OverlayGraphics, MainScreen,
_DISPLAY

WHILE INKEY$ <> "": WEND
k$ = "": WHILE k$ <> CHR$(13): _LIMIT 30: k$ = INKEY$: WEND

END SUB

FUNCTION TRIM$ (Number)
TRIM$ = LTRIM$(RTRIM$(STR$(Number)))
END FUNCTION

