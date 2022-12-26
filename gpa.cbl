       IDENTIFICATION DIVISION.

       PROGRAM-ID. STUDGPA.
       AUTHOR.     YOUR Brandon Paxton.


       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.


           SELECT STUDENT-FILE
               ASSIGN STUFILE
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS SM-STUDENT-ID
               FILE STATUS IS WS-STUD-STATUS.


           SELECT GPA-REPORT
               ASSIGN GPARPT.



       DATA DIVISION.

       FILE SECTION.

       FD  STUDENT-FILE.
       
            COPY STUDLABT IN KC2477.SHARED.COBOL.STUDGPA
       
       
          
          


       FD  GPA-REPORT.

       01  PRINT-REC      PIC X(132).

       WORKING-STORAGE SECTION.

       01  WORK-FIELDS.
           05  STUMAST-EOF-SWITCH      PIC X(01)   VALUE "N".
               88  STUMAST-EOF                     VALUE "Y".
           05  WS-STUD-STATUS          PIC XX.
           05  SPACE-CONTROL           PIC 99 VALUE 1.
           05  WS-POINTS-EARNED        PIC 999V99.
           05  WS-TOTAL-CREDITS        PIC 999V99.
           05  WS-TOTAL-GPA            PIC 999V99.
           05  WS-STUD-AGE             PIC 999.
           
           05 RECORD-FOUND              PIC X.
           


       01  CURRENT-DATE-AND-TIME.
           05  CD-CURRENT-DATE.
              10  CD-YEAR             PIC 9(04).
              10  CD-MONTH            PIC 9(02).
              10  CD-DAY              PIC 9(02).
           05  CD-CURRENT-TIME.
              10  CD-HOURS            PIC 9(02).
              10  CD-MINUTES          PIC 9(02).
           05                         PIC X(09).
           
           
           
         01 POINTS-EARNED                              
                  05             pic 99 value 95.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'A+'
                                                 
                  05             pic 99 value 90.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'A'
                  
                    05             pic 99 value 85.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'b+'
                                                 
                  05             pic 99 value 80.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'B'
                  
                    05             pic 99 value 75.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'C+'
                                                 
                  05             pic 99 value 70.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'C'
                  
                  05             pic 99 value 65.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'D+'
                                                 
                  05             pic 99 value 60.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'D'
                  
                  05             pic 99 value 59.              
                  05             pic 9V9 value 4.0.
                  05             PIC XX   VALUE 'F'
                                               
        01 POINTS-EARNED-tbl redefines POINTS-EARNED.      
                    05 GRADE    occurs 9 times            
                       indexed by group-x.       
                            10 GRADE        pic 99.              
                            10 POINTS       pic 9V9.  
                            10 LETTER      pic XX.      




        01  AGE-TO-GPA   
                05                         PIC 99 VALUE 25.
                05                         PIC 9V9 VALUE 3.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 3.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 1.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 1.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 0.9.
                05                         PIC 999 VALUE 0.
                
                05                         PIC 99 VALUE 34.
                05                         PIC 9V9 VALUE 3.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 3.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 1.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 1.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 0.9.
                05                         PIC 999 VALUE 0.
                
                05                         PIC 99 VALUE 35.
                05                         PIC 9V9 VALUE 3.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 3.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 2.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9  VALUE 1.5.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 1.0.
                05                         PIC 999 VALUE 0.
                05                         PIC 9V9   VALUE 0.9.
                05                         PIC 999 VALUE 0.
        01 AGE-GPA-TABLE REDEFINES AGE-TO-GPA
                      05 AGE      OCCURS 3 TIMES
                                   INDEXED BY AGE-X.
                         10 HIGH-AGE  PIC 99.
                         10 GPA-LEVEL  OCCURS 7 TIMES
                                       INDEXED BY GPA-X.
                             15  LOW-GPA    PIC 9V9.
                             15  TALLY      PIC 999.

       01  HEADING-LINE-1.
           05                 PIC X(06)   VALUE "DATE: ".
           05  HL1-MONTH      PIC 9(02).
           05                 PIC X(01)   VALUE "/".
           05  HL1-DAY        PIC 9(02).
           05                 PIC X(01)   VALUE "/".
           05  HL1-YEAR       PIC 9(04).
           05                 PIC X(08)   VALUE SPACE.
           05                 PIC X(37)
           05          VALUE "GPA RANGE WITHIN STUDENT AGE CATEGORY".
           05                 PIC X(08)   VALUE SPACE.
           05                 PIC X(07)   VALUE "TIME: ".
           05  HL2-HOURS      PIC 9(02).
           05                 PIC X(01)   VALUE ":".
           05  HL2-MINUTES    PIC 9(02).
           05                 PIC X(51)  VALUE SPACE.

       01  HEADING-LINE-2.
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(03)  VALUE "AGE".
           05             PIC X(34)  VALUE SPACE.
           05             PIC X(13)  VALUE "# OF STUDENTS".
           05             PIC X(79)  VALUE SPACE.

       01  HEADING-LINE-3.
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(08)  VALUE "CATEGORY".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "3.5 - 4.0".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "3.0 - 3.4".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "2.5 - 2.9".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "2.0 - 2.4".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "1.5 - 1.9".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "1.0 - 1.4".
           05             PIC X(03)  VALUE SPACE.
           05             PIC X(09)  VALUE "BELOW 1.0".
           05             PIC X(37)  VALUE SPACE.


       01  DETAIL-LINE-CAPTIONS.
           05                   PIC X(11) VALUE '    <25 YRS'.
           05                   PIC X(11) VALUE '25 - 34 YRS'.
           05                   PIC X(11) VALUE '   35 > YRS'.
       01  DETAIL-LINE-CAPTIONS-R REDEFINES DETAIL-LINE-CAPTIONS.
           05     DETAIL-CAPTION    PIC X(11) OCCURS 3 TIMES.



       01  DETAIL-LINE.
           05  DL-GPA-RANGE-CAPTION  PIC X(11).
           05  GPA-RANGE-FLDS  OCCURS 7 TIMES
                               INDEXED BY DETLINDX.
              10                     PIC X(06).
              10  DL-NUMBER-STUDENTS PIC ZZ9.
           05                        PIC X(58)  VALUE SPACE.

       01  STUDENT-TOTAL-LINE.
           05                      PIC X(07)  VALUE SPACE.
           05                      PIC X(07)  VALUE "TOTALS:".
           05  TOT-RANGE-FLDS  OCCURS 7 TIMES
                               INDEXED BY TOTLINDX.
              10                     PIC X(05).
              10  TL-NUMBER-STUDENTS PIC ZZZ9.
           05                        PIC X(55)  VALUE SPACE.



       PROCEDURE DIVISION.

            PERFORM 100-HSK.
            
        PERFORM UNTIL STUMAST-EOF                  
       READ TRANSACTION-IN                          
       IF WS-STUD-STATUS = '10'                    
          SET STUMAST-EOF TO TRUE       
       ELSE                                         
         IF WS-STUD-STATUS = '00'                  
          PERFORM 110-PROCESS                   
       ELSE                                         
         MOVE 'NO' TO ARE-THERE-MORE-RECORDS        
         DISPLAY '******************************'   
         DISPLAY ' 000-MAIN-MODULE'                 
         DISPLAY ' ERROR IN READING THE TRANS FILE' 
         DISPLAY ' FILE STATUS IS ', WS-STUD-STATUS
         DISPLAY '*******************************'  
        END-IF                                      
      END-IF                                        
     END-PERFORM                                    
                          
     STOP RUN.                                      







       100-HSK.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CD-MONTH   TO HL1-MONTH.
           MOVE CD-DAY     TO HL1-DAY.
           MOVE CD-YEAR    TO HL1-YEAR.
           MOVE CD-HOURS   TO HL2-HOURS.
           MOVE CD-MINUTES TO HL2-MINUTES.

           OPEN INPUT STUDENT-FILE
                OUTPUT GPA-REPORT.
                
            PERFORM 220-PRINT-HEADING-LINES.
                
                
                
                
        110-PROCESS.
        
        MOVE ZEROS TO WS-POINTS-EARNED.
        MOVE ZEROS TO WS-TOTAL-CREDITS.
        MOVE ZEROS TO WS-STUD-AGE.

        
            
            PERFORM  VARYING SMSTINDX FROM 1 BY 1
                  UNTIL SMSTINDX >= SM-NUM-CRS-COMPLTD
                     
                SET group-x TO 1

                    SEARCH GRADE  VARYING group-x
                            AT END
                                MOVE N TO RECORD-FOUND
                        WHEN SM-PERCENTAGE-EARNED > GRADE
                            ADD (SM-COURSE-CREDITS (SMSTINDX) * POINTS (GROUP-X) ) TO WS-POINTS-EARNED
                            ADD (SM-COURSE-CREDITS (SMSTINDX) TO WS-TOTAL-CREDITS
                        
                        
                        
                        
                    END-SEARCH	 
            END-PERFORM.
            
            COMPUTE WS-TOTAL-GPA = (WS-POINTS-EARNED / WS-TOTAL-CREDITS).
            
            
            CALL AGESUBP USING SM-DATE-OF-BIRTH 
                                WS-STUD-AGE.
                                
                                
               SET AGE-X TO 1                 
             SEARCH AGE VARYING AGE-X
                        AT END
                            MOVE N TO RECORD-FOUND
                        WHEN WS-STUD-AGE <= HIGH-AGE (AGE-X)

                                SET GPA-X TO 1
                                SEARCH GPA-LEVEL VARYING GPA-X
                                      AT END                                 
                                         MOVE N TO RECORD-FOUND
                                      WHEN WS-TOTAL-GPA >= LOW-GPA (AGE-X GPA-X)
                                        ADD 1 TO TALLY
                                END-SEARCH
              END-SEARCH.                                     
                                          
                



       220-PRINT-HEADING-LINES.

           MOVE HEADING-LINE-1 TO PRINT-REC
           WRITE PRINT-REC AFTER PAGE

           MOVE HEADING-LINE-2 TO PRINT-REC
           WRITE PRINT-REC AFTER 2

           MOVE HEADING-LINE-3 TO PRINT-REC
           WRITE PRINT-REC


           MOVE 2 TO SPACE-CONTROL.
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           


