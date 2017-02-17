
DEFINE VARIABLE whQueryFile   AS HANDLE     NO-UNDO.
DEFINE VARIABLE whQuery       AS HANDLE     NO-UNDO.
DEFINE VARIABLE whBuffer      AS HANDLE     NO-UNDO.
DEFINE VARIABLE wcfieldname   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wcquerystring AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wiaction      AS INTEGER    NO-UNDO.
DEFINE VARIABLE wcsequence    AS CHARACTER  NO-UNDO.

CREATE QUERY whQueryFile. 
whQueryFile:SET-BUFFERS(BUFFER _file:HANDLE). 
whQueryFile:QUERY-PREPARE("FOR EACH _file WHERE _file-number > 0 AND _file-number < 1000 AND _file._file-name = 'artpfe'"   ).
IF whQueryFile:QUERY-OPEN
THEN DO : 
  REPEAT WITH FRAME y:
    whQueryFile:GET-NEXT(). 
    IF whQueryFile:QUERY-OFF-END THEN LEAVE.
    IF _file._file-name = "artpfe" 
    THEN DO : 
      ASSIGN 
        wcfieldname   = "i" + _file._file-name + "id"
        wcquerystring = "FOR EACH " + _file._file-name + " BREAK By " + wcfieldname 
        wcsequence    = _file._file-name + "seq".

      FIND driver._sequence WHERE _seq-name = wcsequence NO-LOCK NO-ERROR.
      IF NOT AVAIL _sequence THEN
      MESSAGE  " Sequence " wcsequence " NOT found " VIEW-AS ALERT-BOX.
      ELSE DO :
        DISPLAY _file._file-name _file-num  wcfieldname  wcsequence . 
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
              DISPLAY whBuffer:BUFFER-FIELD(wcfieldname):BUFFER-VALUE DYNAMIC-CURRENT-VALUE( wcsequence , "driver")  .
              IF whQuery:FIRST-OF(0) THEN LEAVE.
            END.
            ELSE DISP wcfieldname " NOT found ".
          END. 
          whQueryFile:QUERY-CLOSE().
          IF whQuery:FIRST-OF(0) THEN LEAVE.
        END.
        DELETE OBJECT whBuffer NO-ERROR .
        DELETE OBJECT whQuery NO-ERROR.
        END.
    END.
    /* */
  END. 
  whQueryFile:QUERY-CLOSE().
END.

FINALLY :
  DELETE OBJECT whBuffer    NO-ERROR.
  DELETE OBJECT whQuery     NO-ERROR.
  DELETE OBJECT whQueryFile NO-ERROR.
END.
