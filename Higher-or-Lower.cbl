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
       01  WS-HIGH-SCORE PIC ZZ9.
       01  WS-ACTUAL-SCORE PIC ZZ9.
       01  WS-MENU-INPUT PIC X(20).
       01  WS-GAME-INPUT PIC X(20).
       01  WS-GAME-DIFFICULTY PIC 9.
       01  WS-RANDOM-NB PIC ZZZ9.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
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
           DISPLAY "1 - Difficulty 1 (0 to 10) grants 1 point"
           DISPLAY "2 - Difficulty 2 (0 to 100) grants 2 point"
           DISPLAY "3 - Difficulty 3 (0 to 1000) grants 3 point"
           DISPLAY "4 - Exit the game"
           PERFORM DRAW-GAME-BAR.
           PERFORM MENU-INPUT.
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
                   DISPLAY WS-NEW-LINE "Exiting the game"
                   PERFORM QUIT-GAME
               WHEN OTHER
                   DISPLAY "Invalid input please try again"
                   PERFORM GET-RANDOM-NUMBER
                   DISPLAY WS-RANDOM-NB
                   PERFORM MENU-INPUT
           END-EVALUATE.
           PERFORM RESET-INPUTS.

       DRAW-GAME-BAR.
           DISPLAY
           "-----------------------------------------------------------"
           .

       MAIN-GAME-LOOP.
           PERFORM GAME-INPUT.
           PERFORM RESET-INPUTS.

       GAME-INPUT.
           DISPLAY "Guess the number !".
           ACCEPT WS-GAME-INPUT.

       RESET-INPUTS.
           MOVE SPACES TO WS-MENU-INPUT.
           MOVE SPACES TO WS-GAME-INPUT.

       QUIT-GAME.
           STOP RUN.

       GET-RANDOM-NUMBER.
           EVALUATE WS-GAME-DIFFICULTY
               WHEN 1
                   COMPUTE WS-RANDOM-NB = FUNCTION RANDOM () * 10 + 1
               WHEN 2
                   COMPUTE WS-RANDOM-NB = FUNCTION RANDOM () * 100 + 1
               WHEN 3
                   COMPUTE WS-RANDOM-NB = FUNCTION RANDOM () * 1000 + 1
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
