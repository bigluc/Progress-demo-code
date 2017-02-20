DEFINE VARIABLE whQueryFile   AS HANDLE     NO-UNDO.
DEFINE VARIABLE whQuery       AS HANDLE     NO-UNDO.
DEFINE VARIABLE whBuffer      AS HANDLE     NO-UNDO.
DEFINE VARIABLE wcfieldname   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wcquerystring AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wiaction      AS INTEGER    NO-UNDO.
DEFINE VARIABLE wcsequence    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wlok          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wiid          AS INTEGER    NO-UNDO.
DEFINE VARIABLE wiidmin       AS INTEGER    NO-UNDO.
DEFINE VARIABLE wiidmax       AS INTEGER    NO-UNDO.
DEFINE VARIABLE wcindex       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wisequence    AS INTEGER    NO-UNDO.

OUTPUT TO D:\users\loo\testxml\sequencedelta.txt. 

CREATE QUERY whQueryFile. 
whQueryFile:SET-BUFFERS(BUFFER _file:HANDLE). 
whQueryFile:QUERY-PREPARE("FOR EACH _file NO-LOCK WHERE _file-number > 0 AND _file-number < 1000  " ) .
IF whQueryFile:QUERY-OPEN
THEN DO : 
  REPEAT WITH FRAME y:
    whQueryFile:GET-NEXT(). 
    IF whQueryFile:QUERY-OFF-END THEN LEAVE.
    IF true 
    THEN DO : 
      ASSIGN 
        wcfieldname   = "i" + _file._file-name + "id"
        wcquerystring = "FOR EACH " + _file._file-name + " NO-LOCK BREAK By " + wcfieldname 
        wcsequence    = _file._file-name + "seq".
        

      FIND driver._sequence WHERE _seq-name = wcsequence NO-LOCK NO-ERROR.
      IF NOT AVAIL _sequence THEN
      DISP  " Sequence " wcsequence " NOT found " WITH FRAME a.
      ELSE DO :
        wlok = NO.
        CREATE BUFFER whBuffer FOR TABLE _file._file-name.
        IF VALID-HANDLE(whBuffer:BUFFER-FIELD(wcfieldname)) 
        THEN DO :
          whbuffer:FIND-FIRST() NO-ERROR. 
          IF whbuffer:AVAIL THEN wiidmin =  INTEGER(whBuffer:BUFFER-FIELD(wcfieldname):BUFFER-VALUE).
          whbuffer:FIND-LAST() NO-ERROR. 
          IF whbuffer:AVAIL THEN wiidmax = INTEGER(whBuffer:BUFFER-FIELD(wcfieldname):BUFFER-VALUE).
          wisequence = DYNAMIC-CURRENT-VALUE( wcsequence , "driver").
/*            IF wisequence > wiidmin AND  _seq-inc = -1 AND wisequence < 0 */
/*            THEN   DO :                                                   */
           
            DISPLAY  _file._file-name FORMAT "X(20)"  _seq-inc FORMAT "->>>9" wiidmin wiidmax wcsequence wisequence SKIP(1)  . 
/*             DYNAMIC-CURRENT-VALUE( wcsequence , "driver") = wiidmin . */
/*            END. */
        END.
        ELSE DISP wcfieldname " NOT found " WITH FRAM c .
        DELETE OBJECT whBuffer NO-ERROR .
      END.
    END.
  END. 
  whQueryFile:QUERY-CLOSE().
  MESSAGE  'OUTER' VIEW-AS ALERT-BOX.
END.
OUTPUT CLOSE .

FINALLY :
  DELETE OBJECT whBuffer    NO-ERROR.
  DELETE OBJECT whQuery     NO-ERROR.
  DELETE OBJECT whQueryFile NO-ERROR.
END.





/*

DEFINE VARIABLE whQueryFile   AS HANDLE     NO-UNDO.
DEFINE VARIABLE whQuery       AS HANDLE     NO-UNDO.
DEFINE VARIABLE whBuffer      AS HANDLE     NO-UNDO.
DEFINE VARIABLE wcfieldname   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wcquerystring AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wiaction      AS INTEGER    NO-UNDO.
DEFINE VARIABLE wcsequence    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wlok          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wiid          AS INTEGER    NO-UNDO.
DEFINE VARIABLE wiidmin       AS INTEGER    NO-UNDO.
DEFINE VARIABLE wiidmax       AS INTEGER    NO-UNDO.

OUTPUT TO D:\users\loo\testxml\sequencedelta.txt. 

CREATE QUERY whQueryFile. 
whQueryFile:SET-BUFFERS(BUFFER _file:HANDLE). 
whQueryFile:QUERY-PREPARE("FOR EACH _file NO-LOCK WHERE _file-number > 0 AND _file-number < 1000  AND (  _file-name = 'artpfe' OR _file-name = 'actbu' ) " ) .
IF whQueryFile:QUERY-OPEN
THEN DO : 
  loop:
  REPEAT WITH FRAME y:
    whQueryFile:GET-NEXT(). 
    IF whQueryFile:QUERY-OFF-END THEN LEAVE.
    IF _file-name = 'artpfe' OR _file-name = 'actbu'
    THEN DO : 
      ASSIGN 
        wcfieldname   = "i" + _file._file-name + "id"
        wcquerystring = "FOR EACH " + _file._file-name + " NO-LOCK BREAK By " + wcfieldname 
        wcsequence    = _file._file-name + "seq".

      FIND driver._sequence WHERE _seq-name = wcsequence NO-LOCK NO-ERROR.
      IF NOT AVAIL _sequence THEN
      DISP  " Sequence " wcsequence " NOT found " WITH FRAME a.
      ELSE DO :

        wlok = NO.
        CREATE QUERY whQuery.
        CREATE BUFFER whBuffer FOR TABLE _file._file-name.
        whQuery:SET-BUFFERS(whBuffer). 
        whQuery:QUERY-PREPARE(wcquerystring).
        IF whQuery:QUERY-OPEN
        THEN DO : 
          REPEAT WITH FRAME y:
            whQuery:GET-NEXT(). 
            IF whQuery:QUERY-OFF-END THEN LEAVE.
            IF VALID-HANDLE(whBuffer:BUFFER-FIELD(wcfieldname)) 
            THEN DO : 
              wlok = YES.
              wiid = INTEGER(whBuffer:BUFFER-FIELD(wcfieldname):BUFFER-VALUE).
              wiidmin = IF wiidmin > wiid THEN wiid ELSE wiidmin.
              wiidmax = IF wiidmax < wiid THEN wiid ELSE wiidmin.
            END.
            ELSE DISP wcfieldname " NOT found " WITH FRAM c .
          
          END. 
          IF wlok 
          THEN DO :  
            DISPLAY  _file._file-name  _seq-inc wiidmin wiidmax DYNAMIC-CURRENT-VALUE( wcsequence , "driver") SKIP(1)  . 
            
          END.
          whQuery:QUERY-CLOSE().
      
        END.
        DELETE OBJECT whBuffer NO-ERROR .
        DELETE OBJECT whQuery NO-ERROR.
 
      END.
 
    END.
   
   
    /* */
  END. 
 
  whQueryFile:QUERY-CLOSE().
 .
END.

OUTPUT CLOSE .

FINALLY :
  DELETE OBJECT whBuffer    NO-ERROR.
  DELETE OBJECT whQuery     NO-ERROR.
  DELETE OBJECT whQueryFile NO-ERROR.
END.
*/

