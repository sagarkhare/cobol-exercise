cobol
      IDENTIFICATION DIVISION.
       PROGRAM-ID. USERMGMT.
       AUTHOR. LEGACY-SYSTEMS-DEPT.
       DATE-WRITTEN. 1987-03-15.
      *****************************************************************
      * USER MANAGEMENT SYSTEM - MAINFRAME VERSION                    *
      * HANDLES USER REGISTRATION, LOGIN, AND PASSWORD CHANGES        *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * USER DATABASE - FIXED ARRAY (MAX 100 USERS)
       01  USER-TABLE.
           05  USER-ENTRY OCCURS 100 TIMES INDEXED BY USER-IDX.
               10  USER-NAME           PIC X(20).
               10  USER-PASSWORD       PIC X(20).
               10  USER-ACTIVE         PIC 9 VALUE 0.
      
      * SESSION TOKENS - FIXED ARRAY (MAX 50 SESSIONS)
       01  SESSION-TABLE.
           05  SESSION-ENTRY OCCURS 50 TIMES INDEXED BY SESS-IDX.
               10  SESSION-TOKEN       PIC 9(6).
               10  SESSION-USER        PIC X(20).
               10  SESSION-ACTIVE      PIC 9 VALUE 0.
       
      * COUNTERS
       01  USER-COUNT              PIC 999 VALUE 0.
       01  SESSION-COUNT           PIC 999 VALUE 0.
       
      * INPUT VARIABLES
       01  WS-USERNAME             PIC X(20).
       01  WS-PASSWORD             PIC X(20).
       01  WS-OLD-PASSWORD         PIC X(20).
       01  WS-NEW-PASSWORD         PIC X(20).
       01  WS-TOKEN                PIC 9(6).
       01  WS-MENU-CHOICE          PIC 9.
       
      * STATUS FLAGS
       01  WS-USER-FOUND           PIC 9 VALUE 0.
       01  WS-SUCCESS-FLAG         PIC 9 VALUE 0.
       01  WS-RANDOM-NUM           PIC 9(6).
       
      * TEMPORARY VARIABLES
       01  WS-LOOP-CTR             PIC 999.
       01  WS-TEMP-USER            PIC X(20).
       
       PROCEDURE DIVISION.
       
       MAIN-ROUTINE.
           PERFORM INITIALIZE-SYSTEM
           PERFORM DISPLAY-WELCOME
           PERFORM MENU-LOOP UNTIL WS-MENU-CHOICE = 4
           STOP RUN.
       
       INITIALIZE-SYSTEM.
           MOVE 0 TO USER-COUNT
           MOVE 0 TO SESSION-COUNT
           PERFORM VARYING USER-IDX FROM 1 BY 1 
               UNTIL USER-IDX > 100
               MOVE SPACES TO USER-NAME(USER-IDX)
               MOVE SPACES TO USER-PASSWORD(USER-IDX)
               MOVE 0 TO USER-ACTIVE(USER-IDX)
           END-PERFORM
           PERFORM VARYING SESS-IDX FROM 1 BY 1
               UNTIL SESS-IDX > 50
               MOVE 0 TO SESSION-TOKEN(SESS-IDX)
               MOVE SPACES TO SESSION-USER(SESS-IDX)
               MOVE 0 TO SESSION-ACTIVE(SESS-IDX)
           END-PERFORM.
       
       DISPLAY-WELCOME.
           DISPLAY "========================================".
           DISPLAY "   USER MANAGEMENT SYSTEM v1.0         ".
           DISPLAY "   LEGACY MAINFRAME APPLICATION        ".
           DISPLAY "========================================".
       
       MENU-LOOP.
           DISPLAY " ".
           DISPLAY "MAIN MENU:".
           DISPLAY "1. REGISTER NEW USER".
           DISPLAY "2. LOGIN".
           DISPLAY "3. CHANGE PASSWORD".
           DISPLAY "4. EXIT".
           DISPLAY "ENTER CHOICE (1-4): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM REGISTER-USER
               WHEN 2
                   PERFORM LOGIN-USER
               WHEN 3
                   PERFORM CHANGE-PASSWORD
               WHEN 4
                   DISPLAY "SYSTEM SHUTDOWN..."
               WHEN OTHER
                   DISPLAY "INVALID CHOICE. TRY AGAIN."
           END-EVALUATE.
       
       REGISTER-USER.
           DISPLAY "--- USER REGISTRATION ---".
           DISPLAY "ENTER USERNAME: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "ENTER PASSWORD: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
      * CHECK IF USER ALREADY EXISTS
           MOVE 0 TO WS-USER-FOUND
           PERFORM VARYING USER-IDX FROM 1 BY 1
               UNTIL USER-IDX > USER-COUNT OR WS-USER-FOUND = 1
               IF USER-NAME(USER-IDX) = WS-USERNAME
                   MOVE 1 TO WS-USER-FOUND
               END-IF
           END-PERFORM
           
           IF WS-USER-FOUND = 1
               DISPLAY "ERROR: USERNAME ALREADY EXISTS!"
           ELSE
               IF USER-COUNT < 100
                   ADD 1 TO USER-COUNT
                   SET USER-IDX TO USER-COUNT
                   MOVE WS-USERNAME TO USER-NAME(USER-IDX)
                   MOVE WS-PASSWORD TO USER-PASSWORD(USER-IDX)
                   MOVE 1 TO USER-ACTIVE(USER-IDX)
                   DISPLAY "SUCCESS: USER REGISTERED!"
               ELSE
                   DISPLAY "ERROR: USER DATABASE FULL!"
               END-IF
           END-IF.
       
       LOGIN-USER.
           DISPLAY "--- USER LOGIN ---".
           DISPLAY "ENTER USERNAME: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "ENTER PASSWORD: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
      * VERIFY CREDENTIALS
           MOVE 0 TO WS-USER-FOUND
           PERFORM VARYING USER-IDX FROM 1 BY 1
               UNTIL USER-IDX > USER-COUNT OR WS-USER-FOUND = 1
               IF USER-NAME(USER-IDX) = WS-USERNAME
                   IF USER-PASSWORD(USER-IDX) = WS-PASSWORD
                       IF USER-ACTIVE(USER-IDX) = 1
                           MOVE 1 TO WS-USER-FOUND
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           
           IF WS-USER-FOUND = 1
               PERFORM GENERATE-TOKEN
               IF SESSION-COUNT < 50
                   ADD 1 TO SESSION-COUNT
                   SET SESS-IDX TO SESSION-COUNT
                   MOVE WS-RANDOM-NUM TO SESSION-TOKEN(SESS-IDX)
                   MOVE WS-USERNAME TO SESSION-USER(SESS-IDX)
                   MOVE 1 TO SESSION-ACTIVE(SESS-IDX)
                   DISPLAY "SUCCESS: LOGIN APPROVED"
                   DISPLAY "YOUR SESSION TOKEN: " SESSION-TOKEN(SESS-IDX)
               ELSE
                   DISPLAY "ERROR: SESSION TABLE FULL!"
               END-IF
           ELSE
               DISPLAY "ERROR: INVALID CREDENTIALS!"
           END-IF.
       
       CHANGE-PASSWORD.
           DISPLAY "--- CHANGE PASSWORD ---".
           DISPLAY "ENTER SESSION TOKEN: " WITH NO ADVANCING
           ACCEPT WS-TOKEN
           DISPLAY "ENTER OLD PASSWORD: " WITH NO ADVANCING
           ACCEPT WS-OLD-PASSWORD
           DISPLAY "ENTER NEW PASSWORD: " WITH NO ADVANCING
           ACCEPT WS-NEW-PASSWORD
           
      * VALIDATE SESSION TOKEN
           MOVE 0 TO WS-USER-FOUND
           MOVE SPACES TO WS-TEMP-USER
           PERFORM VARYING SESS-IDX FROM 1 BY 1
               UNTIL SESS-IDX > SESSION-COUNT OR WS-USER-FOUND = 1
               IF SESSION-TOKEN(SESS-IDX) = WS-TOKEN
                   IF SESSION-ACTIVE(SESS-IDX) = 1
                       MOVE SESSION-USER(SESS-IDX) TO WS-TEMP-USER
                       MOVE 1 TO WS-USER-FOUND
                   END-IF
               END-IF
           END-PERFORM
           
           IF WS-USER-FOUND = 0
               DISPLAY "ERROR: INVALID SESSION TOKEN!"
           ELSE
      * FIND USER AND VERIFY OLD PASSWORD
               MOVE 0 TO WS-SUCCESS-FLAG
               PERFORM VARYING USER-IDX FROM 1 BY 1
                   UNTIL USER-IDX > USER-COUNT OR WS-SUCCESS-FLAG = 1
                   IF USER-NAME(USER-IDX) = WS-TEMP-USER
                       IF USER-PASSWORD(USER-IDX) = WS-OLD-PASSWORD
                           MOVE WS-NEW-PASSWORD TO USER-PASSWORD(USER-IDX)
                           MOVE 1 TO WS-SUCCESS-FLAG
                           DISPLAY "SUCCESS: PASSWORD CHANGED!"
                       ELSE
                           DISPLAY "ERROR: OLD PASSWORD INCORRECT!"
                           MOVE 1 TO WS-SUCCESS-FLAG
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.
       
       GENERATE-TOKEN.
      * SIMPLE PSEUDO-RANDOM NUMBER (NOT CRYPTOGRAPHICALLY SECURE!)
           COMPUTE WS-RANDOM-NUM = 
               FUNCTION RANDOM * 900000 + 100000.