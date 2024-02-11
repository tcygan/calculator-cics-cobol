MAPKALK DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,           X
               STORAGE=AUTO,DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,    X
               CTRL=(FREEKB,FRSET)                                      
MAP1    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                             
        DFHMDF POS=(1,10),ATTRB=ASKIP,LENGTH=26,                       X
               INITIAL='WELCOME TO CICS CALCULATOR'                     
        DFHMDF POS=(2,19),ATTRB=ASKIP,LENGTH=14,                       X
               INITIAL='**************'                                 
        DFHMDF POS=(3,19),ATTRB=ASKIP,LENGTH=1,INITIAL='*'              
MFILD   DFHMDF POS=(3,21),ATTRB=(UNPROT,IC,NUM,FSET),INITIAL='',       X
               LENGTH=10,JUSTIFY=RIGHT                                  
        DFHMDF POS=(3,32),ATTRB=PROT,INITIAL='*',LENGTH=1               
        DFHMDF POS=(4,19),ATTRB=ASKIP,LENGTH=14,                       X
               INITIAL='**************'                                 
MSG     DFHMDF POS=(5,1),ATTRB=PROT,INITIAL=' ',LENGTH=79               
        DFHMDF POS=(7,1),ATTRB=PROT,INITIAL='F4 -> +',LENGTH=7          
        DFHMDF POS=(8,1),ATTRB=PROT,INITIAL='F5 -> -',LENGTH=7          
        DFHMDF POS=(9,1),ATTRB=PROT,INITIAL='F6 -> *',LENGTH=7          
        DFHMDF POS=(10,1),ATTRB=PROT,INITIAL='F7 -> /',LENGTH=7         
        DFHMDF POS=(11,1),ATTRB=PROT,INITIAL='F3 -> EXIT',             X
               LENGTH=10                                                
        DFHMDF POS=(12,1),ATTRB=PROT,INITIAL='F2 -> CLEAR THE SCRREN', X
               LENGTH=22                                                
        DFHMDF POS=(13,1),ATTRB=PROT,INITIAL='F1 -> CLEAR MEMORY',     X
               LENGTH=18                                                
        DFHMSD TYPE=FINAL                                               
        END                                                             	