{generic-s-driver.i} /* Standaard Driver incl. */ 
/*------------------------------------------------------------------------
    File        : dv-s-table-del.p
    Purpose     : delete records - table name and purge condition as input parameters

    Syntax      : run dv-s-table-del.p (INPUT <table name>,
                                        INPUT <condition>,
                                        INPUT <batch yes/no>)
              eg. RUN dv-s-table-del.p (INPUT "sSession",
                                        INPUT "sSession.dDateCre LT DATETIME(TODAY - 30)",
                                        INPUT NO)


/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcTableName AS CHARACTER  NO-UNDO. 
DEFINE INPUT  PARAMETER ipcCond      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplDelete    AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opiCount     AS INTEGER    NO-UNDO.

DEFINE VARIABLE hQuery  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE     NO-UNDO.

DEFINE VARIABLE cWhere     AS CHARACTER  NO-UNDO.
/* pt 15761 */
DEFINE VARIABLE wcPath              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wiErrorCode         AS INTEGER    NO-UNDO.

/* B dmichiels DRV-8768 */
/* DEFINE VARIABLE wcPathOutputFile    AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE wcLog               AS CHARACTER  NO-UNDO. */
/*                                                            */
/* DEFINE STREAM strLog.                                      */

DEFINE VARIABLE wcRecDel            AS CHARACTER NO-UNDO.
/* E dmichiels DRV-8768 */

/* pt 15761 */

/* ***************************  Main Block  *************************** */

DO ON ERROR UNDO, LEAVE:
  /* Check dms parameters */
  IF SEARCH("dv-s-getparamdms.r") = ? THEN
  DO:
    UNDO, throw NEW Progress.Lang.AppError("Cannot find dv-s-getparamdms").
  END.

  IF OPSYS = "WIN32" THEN
    ASSIGN wcPath = "d:\devsyslog\"
           .
  ELSE DO:
    RUN dv-s-getparamdms.p (INPUT  'SYSLG',         /* cCdParam     */
                            INPUT  0,               /* iSocId       */
                            INPUT  0,               /* iBuId        */
                            INPUT  0,               /* iActiviteId  */
                            INPUT  0,               /* iMrqId       */
                            OUTPUT wcPath,          /* cValeurParam */
                            OUTPUT wiErrorCode).    /* iErrorCode   */
  
    IF wiErrorCode <> 0 AND wiErrorCode <> ? THEN 
    DO:
      UNDO, throw NEW Progress.Lang.AppError("Error in dv-s-getparamdms: " + STRING(wiErrorCode)).
    END.
  END.

  /* Open log file */
  OUTPUT TO VALUE(wcPath + "/purge.log") APPEND.

  ETIME(TRUE). /* pt15761 */
  
  ASSIGN cWhere = "FOR EACH " + ipcTableName + " WHERE " + ipcCond.
  
  CREATE BUFFER hBuffer FOR TABLE ipcTableName.
  CREATE QUERY hQuery.
  
  DEFINE VARIABLE wicounter AS INTEGER    NO-UNDO.
  
  hQuery:SET-BUFFERS(hBuffer).
  
  
  /* B GEEGUN 20080918 - PT3567 */
  IF hQuery:QUERY-PREPARE(cWhere) THEN
  DO:
    hQuery:CACHE = 20.
    hQuery:QUERY-OPEN().
    CASE iplDelete:
      WHEN NO THEN DO:
        hQuery:GET-FIRST(NO-LOCK).
        DO WHILE NOT hQuery:QUERY-OFF-END:
          IF opiCount < 2147483647
          THEN opiCount = opiCount + 1.
          hQuery:GET-NEXT(NO-LOCK).
        END.
      END.
      /* pt.12435 b */
      WHEN YES
      THEN DO : 
        REPEAT :
          hQuery:GET-FIRST(NO-LOCK)  .
          IF hQuery:QUERY-OFF-END 
          THEN LEAVE.
          DO TRANSACTION wicounter = 1 TO 1000 :
            IF wicounter = 1 
            THEN DO : 
              hQuery:GET-FIRST(EXCLUSIVE-LOCK) .
              IF hBuffer:AVAIL
              THEN DO : 
                hBuffer:BUFFER-DELETE(). 
                IF opiCount < 2147483647
                THEN opiCount = opiCount + 1.
              END.   
            END.
            ELSE DO : 
              hQuery:GET-NEXT(EXCLUSIVE-LOCK) .
              IF hBuffer:AVAIL
              THEN DO : 
                hBuffer:BUFFER-DELETE(). 
                IF opiCount < 2147483647
                THEN opiCount = opiCount + 1.
              END.
            END.
          END.
          IF ETIME >= 7200000 THEN
          DO:
            PUT UNFORMATTED "Purge " + ipcTableName + " :2 hour limit reached." SKIP.
            LEAVE.  /* pt15761 */
          END.
        END.
      END.
      /*     WHEN YES THEN DO TRANSACTION:                                                           */
      /*       hQuery:GET-FIRST(EXCLUSIVE-LOCK).                                                     */
      /*       DO WHILE NOT hQuery:QUERY-OFF-END:                                                    */
      /*         hBuffer:BUFFER-DELETE().                                                            */
      /*         hBuffer:BUFFER-RELEASE().                                                           */
      /*         hQuery:GET-NEXT(EXCLUSIVE-LOCK).                                                    */
      /*         opiCount = opiCount + 1.                                                            */
      /*       END.                                                                                  */
      /*       MESSAGE PROGRAM-NAME(1) "testeva: Number OF" ipcTableName "records DELETED:" opiCount */
      /*       VIEW-AS ALERT-BOX.                                                                    */
      /*     END.                                                                                    */
      /* pt.12435 e */
    END CASE
      .
  
    hQuery:QUERY-CLOSE().

    /* B dmichiels DRV-8768 */
/*     IF iplDelete THEN                                                                          */
/*       PUT STRING(NOW) " Number of " ipcTableName "records deleted:" opiCount SKIP.             */
/*     ELSE                                                                                       */
/*       PUT STRING(NOW) " Number of " ipcTableName "records detected fo removal:" opiCount SKIP. */
    
    IF iplDelete THEN
      ASSIGN wcRecDel = "Number of " + ipcTableName + " records deleted:"
           .
    ELSE
      ASSIGN wcRecDel = "Number of " + ipcTableName + " records detected FOR removal:"
           .

    PUT STRING(NOW) FORMAT "X(30)" wcRecDel FORMAT "X(80)" opiCount SKIP.
    /* E dmichiels DRV-8768 */

  END.
  /* E GEEGUN 20080918 - PT3567 */

/* pt15761 */
  CATCH eAnyError AS Progress.Lang.ERROR:  
    MESSAGE "Purge table error:"  
              eAnyError:GetMessage(1) eAnyError:GetMessageNum(1) 
          VIEW-AS ALERT-BOX BUTTONS OK. 
    DELETE OBJECT eAnyError.
  END CATCH.

  FINALLY:
    /* This code is always executed, even when an error is encountered */    
    OUTPUT CLOSE.
    IF VALID-HANDLE(hbuffer) THEN
      DELETE OBJECT hBuffer.
    IF VALID-HANDLE(hQuery) THEN
      DELETE OBJECT hQuery.
  END FINALLY.
/* pt15761 */
END. 


