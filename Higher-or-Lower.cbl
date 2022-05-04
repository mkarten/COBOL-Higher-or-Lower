      ******************************************************************
      * Author: Luca Morgado (mkarten)
      * Date: 04-MAY-2022
      * Purpose: Game
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. HIGHER-OR-LOWER.
       AUTHOR. LUCA MORGADO.
       DATE-WRITTEN. 04-MAY-2022.
       DATE-COMPILED. 04-MAY-2022.
       SECURITY. OpenSource.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01  WS-USERNAME PIC X(7).
       78  WS-NEW-LINE VALUE X"0D".
       01  WS-RUN PIC 9 VALUE 1.
       01  WS-DISPLAY-SCORE PIC ZZZ9.
       01  WS-ACTUAL-SCORE PIC 9999.
       01  WS-MENU-INPUT PIC X(20).
       01  WS-GAME-INPUT PIC X(5).
       01  WS-GAME-DIFFICULTY PIC 9.
       01  WS-RAND PIC ZZZ9.
       01  WS-NB-TEST-VALUE PIC X(5).
       01  WS-IS-NB-FLAG PIC 9 VALUE 0.
       01  WS-NB PIC ZZZ9.
       01  WS-SEED PIC 9(08) VALUE 1.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           MOVE 0 TO WS-ACTUAL-SCORE
           COMPUTE WS-RAND = FUNCTION RANDOM(WS-SEED)
           PERFORM GAME-INTRO.
           PERFORM MAIN-GAME-LOOP UNTIL WS-RUN = 0.
           STOP RUN.

       GAME-INTRO.
           MOVE 1 TO WS-RUN.
           PERFORM DRAW-INTRO-SCREEN.
           DISPLAY WS-NEW-LINE.
           DISPLAY "Select your username (should be 7 letters or less)".
           ACCEPT WS-USERNAME.
           INSPECT WS-USERNAME REPLACING ALL WS-NEW-LINE BY SPACE.
           DISPLAY "Well Hello " WS-USERNAME " and welcome to "
           "HIGHER OR LOWER !".
           DISPLAY WS-NEW-LINE.
           PERFORM INTRO-MENU.

       INTRO-MENU.
           PERFORM DRAW-GAME-BAR.
           MOVE WS-ACTUAL-SCORE TO WS-DISPLAY-SCORE
           DISPLAY "Your Score :" WS-DISPLAY-SCORE
           DISPLAY "1 - Difficulty 1 (0 to 10) grants 1 point"
           DISPLAY "2 - Difficulty 2 (0 to 100) grants 5 point"
           DISPLAY "3 - Difficulty 3 (0 to 1000) grants 10 point"
           DISPLAY "4 - Exit the game"
           PERFORM DRAW-GAME-BAR.
           PERFORM MENU-INPUT.
           ACCEPT WS-SEED FROM TIME
           PERFORM GET-RANDOM-NUMBER.

       MENU-INPUT.
           ACCEPT WS-MENU-INPUT
           INSPECT WS-MENU-INPUT REPLACING ALL WS-NEW-LINE BY SPACE.
           EVALUATE WS-MENU-INPUT
               WHEN "1"
                   DISPLAY WS-NEW-LINE "Setted difficulty to 1"
                   MOVE 1 TO WS-GAME-DIFFICULTY
               WHEN "2"
                   DISPLAY WS-NEW-LINE "Setted difficulty to 2"
                   MOVE 2 TO WS-GAME-DIFFICULTY
               WHEN "3"
                   DISPLAY WS-NEW-LINE "Setted difficulty to 3"
                   MOVE 3 TO WS-GAME-DIFFICULTY
               WHEN "4"
                   PERFORM QUIT-GAME
               WHEN OTHER
                   DISPLAY "Invalid input please try again"
                   PERFORM MENU-INPUT
           END-EVALUATE.
           PERFORM RESET-INPUTS.

       DRAW-GAME-BAR.
           DISPLAY
           "-----------------------------------------------------------"
           .

       MAIN-GAME-LOOP.
           PERFORM GAME-INPUT.
           DISPLAY WS-RAND.
           MOVE FUNCTION NUMVAL(WS-GAME-INPUT) TO WS-NB
           EVALUATE WS-GAME-INPUT
               WHEN "exit"
                   PERFORM QUIT-GAME
               WHEN OTHER
                   IF WS-IS-NB-FLAG = 1 THEN
                       IF WS-NB = WS-RAND THEN
                           DISPLAY "you won" WS-NEW-LINE
                           EVALUATE WS-GAME-DIFFICULTY
                               WHEN 1
                                   ADD 1 TO WS-ACTUAL-SCORE
                               WHEN 2
                                   ADD 5 TO WS-ACTUAL-SCORE
                               WHEN 3
                                   ADD 10 TO WS-ACTUAL-SCORE
                           END-EVALUATE
                           PERFORM INTRO-MENU
                       ELSE IF WS-NB > WS-RAND THEN
                           DISPLAY "LOWER"
                       ELSE
                           DISPLAY "HIGHER"
                       END-IF
                   ELSE
                       DISPLAY "Invalid input please try again"
                   END-IF
           END-EVALUATE.
           PERFORM RESET-INPUTS.

       GAME-INPUT.
           DISPLAY "Guess the number !".
           ACCEPT WS-GAME-INPUT.
           INSPECT WS-GAME-INPUT REPLACING ALL WS-NEW-LINE BY SPACES.
           MOVE FUNCTION LOWER-CASE(WS-GAME-INPUT) TO WS-GAME-INPUT.
           MOVE WS-GAME-INPUT TO WS-NB-TEST-VALUE.
           INSPECT WS-NB-TEST-VALUE REPLACING ALL SPACES BY ZEROES.
           IF WS-NB-TEST-VALUE IS NUMERIC THEN
               MOVE 1 TO WS-IS-NB-FLAG
           END-IF.


       RESET-INPUTS.
           MOVE SPACES TO WS-MENU-INPUT.
           MOVE SPACES TO WS-GAME-INPUT.
           MOVE 0 TO WS-IS-NB-FLAG.

       QUIT-GAME.
           DISPLAY WS-NEW-LINE "Exiting the game"
           STOP RUN.

       GET-RANDOM-NUMBER.
           EVALUATE WS-GAME-DIFFICULTY
               WHEN 1
                   COMPUTE WS-RAND = FUNCTION RANDOM(WS-SEED) * 10 + 1
               WHEN 2
                   COMPUTE WS-RAND = FUNCTION RANDOM(WS-SEED) * 100 + 1
               WHEN 3
                   COMPUTE WS-RAND = FUNCTION RANDOM(WS-SEED) * 1000 + 1
           END-EVALUATE.


       DRAW-INTRO-SCREEN.
           DISPLAY
           " _    _   _____    _____   _    _   ______   _____  ".
           DISPLAY
           "| |  | | |_   _|  / ____| | |  | | |  ____| |  __ \ ".
           DISPLAY
           "| |__| |   | |   | |  __  | |__| | | |__    | |__) |".
           DISPLAY
           "|  __  |   | |   | | |_ | |  __  | |  __|   |  _  / ".
           DISPLAY
           "| |  | |  _| |_  | |__| | | |  | | | |____  | | \ \ ".
           DISPLAY
           "|_|  |_| |_____|  \_____| |_|__|_| |______| |_|  \_\".
           DISPLAY
           "                 / __ \  |  __ \                    ".
           DISPLAY
           "                | |  | | | |__) |                   ".
           DISPLAY
           "                | |  | | |  _  /                    ".
           DISPLAY
           "                | |__| | | | \ \                    ".
           DISPLAY
           " _         ____  \____/  |_|  \_\ ______   _____    ".
           DISPLAY
           "| |       / __ \  \ \        / / |  ____| |  __ \   ".
           DISPLAY
           "| |      | |  | |  \ \  /\  / /  | |__    | |__) |  ".
           DISPLAY
           "| |      | |  | |   \ \/  \/ /   |  __|   |  _  /   ".
           DISPLAY
           "| |____  | |__| |    \  /\  /    | |____  | | \ \   ".
           DISPLAY
           "|______|  \____/      \/  \/     |______| |_|  \_\  ".
       END PROGRAM HIGHER-OR-LOWER.
