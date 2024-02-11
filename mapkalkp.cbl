       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. MAPKALKP.                                            
      * program mapkalkp is meant to process logic of simple calculator 
      * running on cics                                                 
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY MAPKALK.                                                
           COPY DFHAID.   

       01 WS-COMMAREA.                                                  
           05 INPUT-VAR PIC X(10).                                      
           05 OUTPUT-VAR USAGE IS COMP-2.                               
           05 SUBPROG-STATUS PIC X.                                     
                                                                        
       01 FS-VFILE.                                                     
      * layout of the file 
           05 FS-KEY PIC X.                                             
           05 FS-VAR PIC X(10).                                         
       
       01 MAP-DATA.                                                     
           05 ANSWER PIC X(10) VALUE SPACES.                   

       01 TEMP-VAR USAGE IS COMP-2.         

       01 RESPCODE PIC S9(8) COMP.                                     
       
       01 INFO-FOR-USER.                                               
           05 MSG-WHEN-EROR-EXIT PIC X(12) VALUE 'ABNORMAL END'.       
           05 MSG-WHEN-EXIT PIC X(10) VALUE 'NORMAL END'.              
           05 INVALID-KEY-MSG   PIC X(20) VALUE 'THAT WAS INVALID KEY'.
           05 FILE-ERROR-MSG PIC X(13) VALUE 'ERROR IN FILE'.   
           05 NOT-VALID-DATA-MSG PIC X(18) VALUE 'INPUT IS NOT VALID'.       
       
       01 RESULT-FORMAT. 
      * variables used to format output data to the screen(map)    
           05 Z-VAR PIC -Z(9).Z(9).                                    
           05 OUTPUT-FORMAT PIC X(20) VALUE SPACES.                    
           05 COUNT1 PIC 99 VALUE 0.                                   
           05 COUNT2 PIC 99 VALUE 0.                                   
      * variables used to identify if the number is less than zero
       01 IF-MINUS PIC X VALUE 'N'.                                    
           88 MINUS VALUE 'Y'.                                     

      * used in result-formating para
       01 IF-ITS-DONE PIC X VALUE 'N'.                                 
           88 ITS-DONE VALUE 'Y'.    

      * identification what operation will be computed
       01 WHAT-OPERATION PIC X VALUE '0'.     

       PROCEDURE DIVISION.                                             
       MAIN.                                                           
           IF EIBCALEN = 0 THEN                                        
           PERFORM PROGRAM-RUN-FIRST-TIME                              
       
           ELSE                                                        
       
             EVALUATE EIBAID                                           
             WHEN DFHPF3        PERFORM EXIT-PROG-PARA                 
             WHEN DFHPF4        PERFORM ADDITION-PARA                  
             WHEN DFHPF5        PERFORM SUBTRACTION-PARA               
             WHEN DFHPF6        PERFORM MULTIPLICATION-PARA            
             WHEN DFHPF7        PERFORM DIVIDE-PARA                    
             WHEN DFHPF1        PERFORM CLEAR-THE-MAP-A-MEMORY         
             WHEN DFHPF2        PERFORM CLEAR-THE-SCRREN-ONLY          
             WHEN OTHER                                                
 
               PERFORM USER-INVALID-KEY-PARA                           
 
             END-EVALUATE  
           END-IF                                                   
           EXEC CICS
           RETURN TRANSID('KALK') COMMAREA(WS-COMMAREA)   
           END-EXEC                                                 
           GOBACK.                                                  
       SEND-THE-WHOLE-MAP.                                          
      * used only once used to send the screen to the user 
           EXEC CICS                                                
           SEND MAP('MAP1') MAPSET('MAPKALK')                       
           FROM(MAP1O)                                              
           RESP(RESPCODE)                                           
           END-EXEC                                                 
           IF RESPCODE = DFHRESP(NORMAL)                            
           THEN                                                     
           CONTINUE                                                 
           ELSE                                                     
           PERFORM ERROR-PARA                                       
           END-IF                                                   
           EXIT.                

       SEND-THE-DATA-ONLY.                                          
      * used to send data and messages to the user

           EXEC CICS                                                
      
           SEND MAP('MAP1') MAPSET('MAPKALK')                       
           FROM(MAP1O)                                              
           DATAONLY                                                 
           FREEKB                                                   
           ERASEAUP                                                 
           RESP(RESPCODE)                                           
      
           END-EXEC                                                 
           IF RESPCODE = DFHRESP(NORMAL)                            
           THEN                                                     
             CONTINUE                                                 
           ELSE                                                     
             PERFORM ERROR-PARA                                       
           END-IF                                                   
           EXIT.                                                    
       PROGRAM-RUN-FIRST-TIME.    
      * sending screen to userr

      * writing 'NOTHING' into vsam file                              
      * there we will be storing the last given number or result
      * only one record of that vsam file is used in program               
           MOVE LOW-VALUES TO MAP1O                                   
           MOVE '1' TO FS-KEY                                         
             EXEC CICS                                                
             READ                                                     
             FILE('VFILE')                                            
             INTO(FS-VFILE)                                           
             RESP(RESPCODE)                                           
             RIDFLD(FS-KEY)                                           
             UPDATE                                                   
             END-EXEC                                                 
            EVALUATE RESPCODE                                         
            WHEN DFHRESP(NORMAL)                                      
             MOVE 'NOTHING' TO FS-VAR                                 
             EXEC CICS                                                
             REWRITE     
             FILE('VFILE')                                        
             FROM(FS-VFILE)                                       
             RESP(RESPCODE)                                       
             END-EXEC                                             
            WHEN OTHER                                            
             MOVE FILE-ERROR-MSG TO MSGO                         
            END-EVALUATE                                          
           PERFORM SEND-THE-WHOLE-MAP                             
           EXIT.                                                  
       EXIT-PROG-PARA.                                            
      * normal termination of the transaction                    
           EXEC CICS                                              
           SEND TEXT FROM(MSG-WHEN-EXIT)                          
           ERASE                                                  
           END-EXEC                                               
           EXEC CICS                                              
           RETURN    
           END-EXEC                                                     
           GOBACK.                                                      
       ERROR-PARA.                                                      
      * termination because of an error                    
           EXEC CICS                                                    
           SEND TEXT FROM(MSG-WHEN-EROR-EXIT)                           
           ERASE                                                        
           END-EXEC                                                     
           EXEC CICS                                                    
           RETURN                                                       
           END-EXEC                                                     
           GOBACK.               
      * paragraphs below are called when user chooses what operation
      * he will process

       ADDITION-PARA.                                                   
           MOVE '+' TO WHAT-OPERATION                                   
           PERFORM CALCULATE-PARA                                       
           EXIT.                                                        
       SUBTRACTION-PARA.                                                
           MOVE '-' TO WHAT-OPERATION                                 
           PERFORM CALCULATE-PARA                                     
           EXIT.                                                      
       MULTIPLICATION-PARA.                                           
           MOVE '*' TO WHAT-OPERATION                                 
           PERFORM CALCULATE-PARA                                     
           EXIT.                                                      
       DIVIDE-PARA.                                                   
           MOVE '/' TO WHAT-OPERATION                                 
           PERFORM CALCULATE-PARA                                     
           EXIT.                                                      
       CALCULATE-PARA.              
      * geting input from user

           PERFORM GET-THE-DATA                                       
      * we are gonna check if the input data is correct in subprog    
      * 'N' means our input is correct                            
      * '9' means it is not                                           
           MOVE 'N' TO SUBPROG-STATUS                                 
           MOVE ANSWER TO INPUT-VAR                                     
           MOVE 0 TO OUTPUT-VAR                                         
           EXEC CICS                                                    
           LINK PROGRAM('SUBPROG') COMMAREA(WS-COMMAREA)               
           END-EXEC                                                     
           IF SUBPROG-STATUS = 'N'                                      
      * now we will check what is in the vsam file                      
      * if there is 'NOTHING' then we will store there                  
      * number we got from user                                         
      * if there is something else(number from erlier)                  
      * we are gonna to calculate those 2 numbers and put output to user
           MOVE '1' TO FS-KEY                                           
           EXEC CICS                                                    
            READ                                                         
            FILE('VFILE')                                                
            INTO(FS-VFILE)                                               
            RIDFLD(FS-KEY)                       
            RESP(RESPCODE)                                           
            UPDATE                                                   
           END-EXEC                                                 
           EVALUATE RESPCODE                                        
           WHEN DFHRESP(NORMAL)                                     
              IF FS-VAR = 'NOTHING' THEN                            
              MOVE INPUT-VAR TO FS-VAR                              
               EXEC CICS                                            
               REWRITE                                              
               FILE('VFILE')                                        
               RESP(RESPCODE)                                       
               FROM(FS-VFILE)                                       
               END-EXEC                                             
                IF RESPCODE = DFHRESP(NORMAL)                       
                THEN CONTINUE                                       
                ELSE                                                
                 MOVE FILE-ERROR-MSG TO MSGO         
                END-IF                                               
              ELSE                                                   
      * IN THE FILE IS OUR PREVIOUS NUMBER                           
      
      * moving last given number or result to temp-var
                COMPUTE TEMP-VAR = FUNCTION NUMVAL(FS-VAR)           
   
                EVALUATE WHAT-OPERATION                              
      * what type of operation should be computed?

                WHEN '+'                                             
      
                  COMPUTE OUTPUT-VAR = OUTPUT-VAR + TEMP-VAR         
                  ON SIZE ERROR                                      
                  MOVE 0 TO OUTPUT-VAR                               
                  NOT ON SIZE ERROR                                  
                  CONTINUE                                           
                  END-COMPUTE                                        
      
                WHEN '-'                                             
      
                  COMPUTE OUTPUT-VAR = TEMP-VAR - OUTPUT-VAR         
                  ON SIZE ERROR                                      
                  MOVE 0 TO OUTPUT-VAR                               
                  NOT ON SIZE ERROR                                  
                  CONTINUE                                            
                  END-COMPUTE                                         
      
                WHEN '*'                                              
      
                  COMPUTE OUTPUT-VAR = OUTPUT-VAR * TEMP-VAR          
                  ON SIZE ERROR                                       
                  MOVE 0 TO OUTPUT-VAR                                
                  NOT ON SIZE ERROR                                   
                  CONTINUE                                            
                  END-COMPUTE                                         
      
                WHEN '/'                                              
      
                  COMPUTE OUTPUT-VAR = TEMP-VAR / OUTPUT-VAR          
                  ON SIZE ERROR                                       
                  MOVE 0 TO OUTPUT-VAR                                
                  NOT ON SIZE ERROR                                   
                  CONTINUE                                            
                  END-COMPUTE                                         
      
                WHEN OTHER                                            
      * THAT CANNOT HAPPEN SO:                                       
                  CONTINUE                                           
       
                END-EVALUATE                                         
      * moving the result to output(map)
      * we need to format that result

                PERFORM RESULT-FORMATING                             
                MOVE 'RESULT' TO MSGO                                
      
      * saving result to a file                                      
      * to work as our last given number                             
      
                 MOVE ANSWER TO FS-VAR    
                                            
                 EXEC CICS                                           
                 REWRITE                                             
                 FILE('VFILE')                                       
                 FROM(FS-VFILE)                                      
                 RESP(RESPCODE)                                      
                 END-EXEC                                            
                 
                  IF RESPCODE = DFHRESP(NORMAL)                      
                  THEN                                               
                  CONTINUE               

                  ELSE                                              
                    MOVE FILE-ERROR-MSG TO MSGO                    
                  END-IF                                            
              
              END-IF                                                
           WHEN OTHER  
      * respcode of reading the file is not normal                                                  
              MOVE FILE-ERROR-MSG TO MSGO                     
           END-EVALUATE                                             
           ELSE        
      *     SUBPROG-STATUS IS NOT EQUAL TO 'N'

             MOVE NOT-VALID-DATA-MSG TO MSGO                       
           END-IF                                                   
             PERFORM SEND-THE-DATA-ONLY                             
           EXIT.                                      



       CLEAR-THE-MAP-A-MEMORY.                                      
      * clearing the map and clearing the memory of last number     
      * rewriting file and setting fs-var to nothing                
           MOVE LOW-VALUES TO MAP1O                                 
           MOVE 1 TO FS-KEY                                            
           EXEC CICS                                                   
           READ                                                        
            FILE('VFILE')                                              
            RIDFLD(FS-KEY)                                             
            INTO(FS-VFILE)                                             
            RESP(RESPCODE)                                             
            UPDATE                                                     
           END-EXEC                                                    
             IF RESPCODE = DFHRESP(NORMAL)                             
             THEN                                                      
               MOVE 'NOTHING' TO FS-VAR                                
               EXEC CICS                                               
               REWRITE FILE('VFILE')                                   
               FROM(FS-VFILE)                                          
               RESP(RESPCODE)                                          
               END-EXEC                                                
                 IF RESPCODE = DFHRESP(NORMAL)                       
                 THEN                                                
                  CONTINUE                                           
                 ELSE            
      * error while trying to rewrite the file             
                   MOVE FILE-ERROR-MSG TO MSGO                      
                 END-IF                                              
             ELSE  
      * error in reading the file             
               MOVE FILE-ERROR-MSG TO MSGO         
             END-IF                                                  
           MOVE ' ' TO MSGO                                          
           PERFORM SEND-THE-DATA-ONLY                                
           EXIT.                                                     
       CLEAR-THE-SCRREN-ONLY.                                        
      * para clears only the screen it doesn't clear the number stored  
      * in the file(previous number)                                 
           MOVE LOW-VALUES TO MAP1O                                  
           PERFORM SEND-THE-DATA-ONLY                                
           EXIT.                                                   
       USER-INVALID-KEY-PARA.                                      
      * sending a message to user because of invalid key           
           MOVE INVALID-KEY-MSG TO MSGO                            
           PERFORM SEND-THE-DATA-ONLY                              
           EXIT.                                                   
       GET-THE-DATA.                                               
      * getting input from user from screen

           MOVE LOW-VALUES TO MAP1I                                
           EXEC CICS                                               
      
           RECEIVE MAP('MAP1') MAPSET('MAPKALK')                   
           INTO(MAP1I)                                             
           RESP(RESPCODE)                                          
      
           END-EXEC                                                
           EVALUATE RESPCODE                                       
           WHEN DFHRESP(NORMAL)                                    
      * RECEIVING WAS SUCCESFULL                                   
             MOVE MFILDI TO ANSWER                                 
           WHEN DFHRESP(MAPFAIL)                                       
      * ZERO IS ASSUMED                                                
             MOVE 0 TO ANSWER                                          
                                                                       
           WHEN OTHER                                                  
             PERFORM ERROR-PARA                                        
           END-EVALUATE                                                
           EXIT.                                                       
       RESULT-FORMATING.                                               
      * reformating COMP-2 variable to aplpanumeric pic x(10) variable 
      * output-var to answer                                           
      * answer to mfildo                                               
           IF OUTPUT-VAR = 0 THEN                                      
             MOVE 0 TO ANSWER                                          
             MOVE ANSWER TO MFILDO                                     
           ELSE                                                        
           MOVE OUTPUT-VAR TO Z-VAR                                    
           MOVE Z-VAR TO OUTPUT-FORMAT                                 
           IF OUTPUT-FORMAT(1:1) = '-'                                 
           THEN                                                        
            MOVE 'Y' TO IF-MINUS                                       
            MOVE SPACE TO OUTPUT-FORMAT(1:1)                           
           END-IF                                                      
           MOVE 1 TO COUNT1                                            
           PERFORM UNTIL ITS-DONE                                      
             IF OUTPUT-FORMAT(COUNT1:1) = SPACE THEN                   
              ADD 1 TO COUNT1                                          
             ELSE                                                      
              MOVE 'Y' TO IF-ITS-DONE                                  
              COMPUTE COUNT2 = 20 - COUNT1                             
              MOVE OUTPUT-FORMAT(COUNT1:COUNT2) TO OUTPUT-FORMAT(2:9)  
      * REMOVING TRAILING ZEROS                                        
              MOVE FUNCTION REVERSE(OUTPUT-FORMAT) TO OUTPUT-FORMAT    
              INSPECT OUTPUT-FORMAT REPLACING LEADING ZEROS BY SPACES  
              MOVE FUNCTION REVERSE(OUTPUT-FORMAT) TO OUTPUT-FORMAT  
             END-IF                                                  
           END-PERFORM                                               
           IF MINUS THEN                                             
           MOVE '-' TO OUTPUT-FORMAT(1:1)                            
           ELSE                                                      
           CONTINUE                                                  
           END-IF                                                    
           MOVE OUTPUT-FORMAT TO ANSWER                              
           MOVE ANSWER TO MFILDO                                     
           END-IF                                                    
           EXIT.                                                     
                        
                                             
                                             
                                  
                                    
                                            
