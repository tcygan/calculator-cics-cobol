       IDENTIFICATION DIVISION.                                     
       PROGRAM-ID. SUBPROG.   
      * program is subrutine of mapkalkp
      * checking if the given input is correct number
      * subprog-status is equal to 'N' when input is correct
      * else subprog-status is equal to '9'
       DATA DIVISION.                                               
       WORKING-STORAGE SECTION.                                     
       01 TEMP-VAR PIC X(10).                                       
       LINKAGE SECTION.                                             
       01 DFHCOMMAREA.                                              
           05 INPUT-VAR PIC X(10).                                  
           05 OUTPUT-VAR USAGE IS COMP-2.                           
           05 SUBPROG-STATUS PIC X.                                 
       PROCEDURE DIVISION USING DFHCOMMAREA.                        
       MAIN.                                                        
           IF FUNCTION TEST-NUMVAL(INPUT-VAR) = 0 
           THEN  

           COMPUTE OUTPUT-VAR = FUNCTION NUMVAL(INPUT-VAR)          
           ON SIZE ERROR                                            
               MOVE '9' TO SUBPROG-STATUS                         
           NOT ON SIZE ERROR             
               MOVE 'N' TO SUBPROG-STATUS                     
           END-COMPUTE                                  
           
           ELSE 
           
           MOVE '9' TO SUBPROG-STATUS
           
           END-IF

           EXEC CICS RETURN END-EXEC                    
           GOBACK.                                      
