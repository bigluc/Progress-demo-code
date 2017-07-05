
{generic-s-driver.i} /* Standaard Driver incl. */ 
/*------------------------------------------------------------------------

  Program   : dep-lib.p
  
  Function  : Deployment functions & tools

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:           Bart Mille

  Created:          20/04/2010
  
  Changements:
  PT: 15373 - bmille - added delete routine
  
------------------------------------------------------------------------*/
&GLOBAL-DEFINE smtpip   10.193.7.30
&GLOBAL-DEFINE cp       IBM850
&GLOBAL-DEFINE dom      dieteren.be
&GLOBAL-DEFINE mailsend D:\dev\dwp\tools\mailsend\mailsend.exe
&GLOBAL-DEFINE deb      1
&GLOBAL-DEFINE SetupXml 3
&SCOPED-DEFINE JiraURL -WSDL http://jira-01.app.dieteren.be:8080/rpc/soap/jirasoapservice-v2?wsdl


/*&SCOPED-DEFINE JiraURL -WSDL http://jiraqa.app.dieteren.be:8080/rpc/soap/jirasoapservice-v2?wsdl*/



{dwutbatcb1.i}
{proceduredll.i}
{dep-tt.i}
{dv-p-dumptablett.i}

DEF VAR iCnt             AS INT    NO-UNDO.
DEF VAR lError           AS LOG    NO-UNDO.
DEF VAR errObj           AS Progress.Lang.AppError NO-UNDO.
DEF STREAM strComp.
/*DEF VAR rJiraSession AS CLASS JiraSession NO-UNDO.*/


FUNCTION fGetOsError RETURNS CHAR(INPUT iErr AS INT):
  CASE iErr:  
    WHEN 1 THEN    
      RETURN "Access denied".  
    WHEN 2 THEN      
      RETURN "The file or directory you want to access does not exist.".
    OTHERWISE    
      RETURN "OS Error #" + STRING(OS-ERROR,"99").  
  END CASE.
END FUNCTION.


PROCEDURE pSendMail:
/* ------------------------------------------- */
/* Function : Send an email                    */
/* ------------------------------------------- */
DEF INPUT PARAM ipcEmail    AS CHAR NO-UNDO.
DEF INPUT PARAM ipcMsg      AS LONGCHAR NO-UNDO.
DEF INPUT PARAM ipcTitle    AS CHAR NO-UNDO.
DEF INPUT PARAM ipcFrom     AS CHAR NO-UNDO.
DEF INPUT PARAM ipcContent  AS CHAR NO-UNDO.

DEF VAR wcCmd    AS CHAR  NO-UNDO.
DEFINE VARIABLE wcodepage     AS CHARACTER  NO-UNDO INIT '{&cp}'.
DEFINE VARIABLE wcSMTPIP      AS CHARACTER  NO-UNDO INIT '{&smtpip}'.
DEFINE VARIABLE wcEmailDom    AS CHARACTER  NO-UNDO INIT '{&dom}'.

ASSIGN wcCmd = "{&mailsend}" + " -d " + wcEmailDom + " "                               /* Domaine dieteren.be                   */
       wcCmd = wcCmd + "-smtp " + wcSMTPIP + " "                                      /* Adresse IP du serveur mail            */
       wcCmd = wcCmd + "-t " + ipcEmail    + " "                                      /* Adresse email du destinataire         */
       wcCmd = wcCmd + "-sub " + CHR(34) + ipcTitle + CHR(34) + " "                   /* Sujet de l'email                      */
       wcCmd = wcCmd + "+cc "                                                         /* Pas de carbon copy                    */
       wcCmd = wcCmd + "+bc "                                                         /* Pas de hidden carbon copy             */
       wcCmd = wcCmd + "-f " + ipcFrom + " "                         /* Adresse email de l'envoyeur           */
/*     wcCmd = wcCmd + "-a " + CHR(34) + wcPathMsgPT + ",text/plain" + CHR(34) + " "  /* Path vers le fichier attaché          */*/
       wcCmd = wcCmd + "-cs {&cp} "                                                   /* Character set pour le fichier attache */
       wcCmd = wcCmd + "<" + ipcContent                                               /* Contenu pour l'email     */
       .
OS-COMMAND SILENT VALUE(wcCmd). 

END PROCEDURE.

PROCEDURE pCompileFolder:
/* --------------------------------------------- */
/* Function: Compile a folder + put result in tt */
/* --------------------------------------------- */
  DEF INPUT PARAM cFolder AS CHAR NO-UNDO.
  DEF INPUT PARAM cDest   AS CHAR NO-UNDO.
  DEF INPUT PARAM cType   AS CHAR NO-UNDO.
  DEF INPUT PARAM cLog    AS CHAR NO-UNDO.
  DEF INPUT PARAM TABLE FOR ttCompileLog.

  DEF VAR cFile       AS CHAR NO-UNDO.
  DEF VAR cExt        AS CHAR NO-UNDO.
  DEF VAR cError      AS CHAR NO-UNDO.

  IF cDest = "" THEN
    ASSIGN
      cDest = cFolder
      .
  /* Create destination folder 
  ASSIGN lError = FALSE.
  OS-CREATE-DIR VALUE("{&DestTrig}").
  lError = OS-ERROR <> 0.
  IF lError THEN
    DO:
      ASSIGN
        cWarn = "Error during create trigger folder."
        .

      UNDO, THROW NEW Progress.Lang.AppError(cWarn,1).
    END.*/
  
  INPUT STREAM strcomp FROM OS-DIR(cFolder).
  OUTPUT TO VALUE(cLog) APPEND.

  PUT UNFORMATTED cFolder ":" SKIP.
  REPEAT:
    IMPORT STREAM strComp cFile.
    IF LENGTH(cFile) < 3 THEN
        NEXT.
    IF (NOT cFile MATCHES "*~~.p") AND (NOT cFile MATCHES "*~~.w") THEN
      NEXT.

    IF cFile MATCHES "*~~.p" THEN
        COMPILE VALUE(cFolder + "\" + cFile) SAVE INTO VALUE(cDest) /*LISTING VALUE(cDest + "\compile.log") APPEND*/ NO-ERROR.
  
    IF ERROR-STATUS:NUM-MESSAGES > 1 THEN
    DO:      
      ASSIGN cError = "".
      PUT UNFORMATTED " " SKIP .
      PUT UNFORMATTED SKIP cfile ":" SKIP .
      DO iCnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT UNFORMATTED ERROR-STATUS:GET-MESSAGE(iCnt) SKIP.
        ASSIGN
          cError = cError + ERROR-STATUS:GET-MESSAGE(iCnt) + CHR(10)
          .
      END.
      ASSIGN lError = TRUE.
      CREATE ttCompileLog.
      ASSIGN
        ttCompileLog.cFile = cFile
        ttCompileLog.cType = cType
        ttCompileLog.cDesc = cError
        .

      CASE cType:
        WHEN "S" THEN
        DO:
          FIND FIRST dwrpscre NO-LOCK
            WHERE dwrpscre.rappl = "DV"
            AND   dwrpscre.cscre = SUBSTRING(RIGHT-TRIM(cFile, ".p") ,6) NO-ERROR.
          IF AVAIL dwrpscre THEN
            ASSIGN
              ttCompileLog.cUserId = dwrpscre.rusermodi
              ttCompileLog.dDateMod = dwrpscre.dmodi
              ttCompileLog.dDateCre = dwrpscre.dcrea
              ttCompileLog.cUserCre = dwrpscre.rusercrea
              ttCompileLog.cProg = dwrpscre.ntitl
              ttCompileLog.cSide = (IF cFile BEGINS("dv-s") THEN "S" ELSE "C")
              .
        END.
        WHEN "P" THEN
        DO:
          FIND FIRST dwrpprog NO-LOCK
             WHERE dwrpprog.rappl = "DV"
             AND   dwrpprog.cprog = cFile NO-ERROR.
          IF AVAIL dwrpprog THEN
            ASSIGN
              ttCompileLog.cUserId = dwrpprog.rusermodi
              ttCompileLog.dDateMod = dwrpprog.dmodi
              ttCompileLog.dDateCre = dwrpprog.dcrea
              ttCompileLog.cUserCre = dwrpprog.rusercrea
              ttCompileLog.cProg = dwrpprog.nprog
              ttCompileLog.cSide = dwrpprog.cSide
              .
        END.
            
      END CASE.
     
    END.
    PROCESS EVENTS.
  END.
  PUT " " SKIP.
  OUTPUT CLOSE.
 /* IF lError THEN
  DO:
    errObj = NEW Progress.Lang.AppError("Compilation error. See logfile:" + cLog , 1).    
    RETURN ERROR errObj.
  END.*/
END PROCEDURE.

PROCEDURE pMissingRcode:
/* --------------------------------------------- */
/* Function: Search for missing .p's or .r's     */
/* --------------------------------------------- */
  DEF INPUT PARAM cFolder AS CHAR NO-UNDO.
  DEF INPUT PARAM TABLE FOR ttCompileLog.

  DEF VAR cFile       AS CHAR NO-UNDO.
  DEF VAR cError      AS CHAR NO-UNDO.
 
  INPUT STREAM strcomp FROM OS-DIR(cFolder).

  REPEAT:
    IMPORT STREAM strComp cFile.
    IF LENGTH(cFile) < 3 THEN
        NEXT.
    IF (NOT cFile MATCHES "*~~.p") AND (NOT cFile MATCHES "*~~.w") THEN
      NEXT.

    IF SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".r") = ? THEN
    DO:
      IF SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".r") = ? THEN
      DO:
        CREATE ttCompileLog.
        ASSIGN
          ttCompileLog.cFile = cFile
          ttCompileLog.cType = "F"
          ttCompileLog.cDesc = "No corresponding .r found in " + cFolder
          .
      END.
      ELSE
      DO:
        CREATE ttCompileLog.
        ASSIGN
          ttCompileLog.cType = "F"
          ttCompileLog.cFile = cFile
          ttCompileLog.cDesc = "Corresponding .r located in the wrong folder"
          .
      END.
    END.
  END.
  INPUT CLOSE.
END PROCEDURE.

PROCEDURE pMissingPcode:
/* --------------------------------------------- */
/* Function: Search for missing .p's or .r's     */
/* --------------------------------------------- */
  DEF INPUT PARAM cFolder AS CHAR NO-UNDO.
  DEF INPUT PARAM TABLE FOR ttCompileLog.

  DEF VAR cFile       AS CHAR NO-UNDO.
  DEF VAR cError      AS CHAR NO-UNDO.
 
  INPUT STREAM strcomp FROM OS-DIR(cFolder).

  REPEAT:
    IMPORT STREAM strComp cFile.
    IF LENGTH(cFile) < 3 THEN
        NEXT.
    IF (NOT cFile MATCHES "*~~.r") THEN
      NEXT.

    IF SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".p") = ? AND
       SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".w") = ? THEN
    DO:
      IF SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".w") = ? AND 
         SEARCH(cFolder + "\" + SUBSTRING(cFile, 1, LENGTH(cFile) - 2) + ".p") = ? THEN
      DO:
        CREATE ttCompileLog.
        ASSIGN
          ttCompileLog.cFile = cFile
          ttCompileLog.cType = "F"
          ttCompileLog.cDesc = "No corresponding .p/.w found in " + cFolder
          .
      END.
      ELSE
      DO:
        CREATE ttCompileLog.
        ASSIGN
          ttCompileLog.cFile = cFile
          ttCompileLog.cType = "F"
          ttCompileLog.cDesc = "Corresponding .p/.w located in the wrong folder"
          .
      END.
    END.
  END.
  INPUT CLOSE.
END PROCEDURE.


PROCEDURE pSearchCode:
/* ------------------------------------------- */
/* Search for a specific expression in code    */
/* ------------------------------------------- */
  DEF INPUT PARAM cSide     AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilter   AS CHAR NO-UNDO.
  DEF INPUT PARAM cErr     AS CHAR NO-UNDO.
  DEF INPUT PARAM TABLE FOR ttCompilelog.

  DEF VAR cLong AS LONGCHAR NO-UNDO.
  DEF VAR cNew AS LONGCHAR NO-UNDO.
  DEF VAR cTmp AS LONGCHAR NO-UNDO.
  DEF VAR cmtBegin  AS INT NO-UNDO.
  DEF VAR cmtEnd    AS INT NO-UNDO.
  DEF VAR iCnt      AS INT NO-UNDO.
  DEF VAR ifound    AS INT NO-UNDO.
  DEF VAR lStop     AS LOG NO-UNDO INIT FALSE.

  FOR EACH dwrpprog NO-LOCK
    WHERE dwrpprog.rappl = "DV"
    AND (IF cSide <> ? AND cSide <> "" THEN dwrpprog.cSide BEGINS(cSide) ELSE TRUE) :

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrplineprog NO-LOCK
      OF dwrpprog:
      ASSIGN
        cLong = clong + dwrplineprog.nline
        .
      IF INDEX(dwrplineprog.nline, cFilter) <> 0 THEN
        ifound = 1.
    END.   

    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
  
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*").
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
                    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END.

    IF INDEX(cLong, cFilter) <> 0 THEN
    DO:
      CREATE ttCompileLog.
      ASSIGN
        ttCompileLog.cType = "P"
        ttCompileLog.cSide = dwrpprog.cside
        ttCompileLog.cUserId = dwrpprog.rusermodi
        ttCompileLog.dDateMod = dwrpprog.dmodi
        ttCompileLog.dDateCre = dwrpprog.dcrea
        ttCompileLog.cUserCre = dwrpprog.rusercrea
        ttCompileLog.cProg = dwrpprog.nprog
        ttCompileLog.cFile = dwrpprog.cprog
        ttCompileLog.cDesc = cErr + CHR(10)
        .

    END.

  END.

  FOR EACH dwrphook NO-LOCK
     WHERE dwrphook.rappl = "DV" :

      FIND FIRST dwrpeven NO-LOCK
        WHERE dwrpeven.reven = dwrphook.reven NO-ERROR.
      IF AVAIL dwrpeven THEN
      DO:
        IF cSide <> ? AND cSide <> "" THEN
        DO:
          IF NOT(cSide BEGINS dwrpeven.rSide) THEN
            NEXT.
        END.
      END.

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrpchunhook NO-LOCK
      OF dwrphook:
      ASSIGN
        cLong = clong + dwrpchunhook.echun
        .
      IF INDEX(dwrpchunhook.echun, cFilter) <> 0 THEN
        ifound = 1.
    END.
    
    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
  
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*").
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
                    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END.

    IF INDEX(cLong, cFilter) <> 0 THEN
    DO:
      CREATE ttCompileLog.
      FIND FIRST dwrpscre NO-LOCK
        WHERE dwrpscre.rappl = "DV"
        AND   dwrpscre.rscre = INT(ENTRY(1,dwrphook.skey)) NO-ERROR.
      IF AVAIL dwrpscre THEN
        ASSIGN
          ttCompileLog.cUserId = dwrpscre.rusermodi
          ttCompileLog.dDateMod = dwrpscre.dmodi
          ttCompileLog.dDateCre = dwrpscre.dcrea
          ttCompileLog.cUserCre = dwrpscre.rusercrea
          ttCompileLog.cProg = dwrpscre.ntitl
          ttCompileLog.cFile = dwrpscre.cscre
          ttCompileLog.cSide = IF AVAIL dwrpeven THEN dwrpeven.rside ELSE cSide
          ttCompileLog.cDesc = cErr + CHR(10)
          ttCompileLog.cType = "S"
          ttCompileLog.cHook = (IF AVAIL dwrpeven THEN dwrpeven.neven ELSE "")
          .

    END.

  END.

&IF {&deb} = 1 &THEN
  TEMP-TABLE ttcompilelog:WRITE-XML("FILE", "d:\temp\log2.xml", YES, "windows-1252", ?, YES, NO).
&ENDIF
END PROCEDURE.

PROCEDURE pCountCode:
/* ------------------------------------------- */
/*                                             */
/* ------------------------------------------- */
  DEF INPUT PARAM cSide     AS CHAR NO-UNDO.
  DEF INPUT PARAM cFilter   AS CHAR NO-UNDO.
  DEF INPUT PARAM cErr      AS CHAR NO-UNDO.
  DEF INPUT PARAM iLimit    AS INT  NO-UNDO.
  DEF INPUT PARAM TABLE FOR ttCompilelog.


  DEF VAR cLong AS LONGCHAR NO-UNDO.
  DEF VAR cNew AS LONGCHAR NO-UNDO.
  DEF VAR cTmp AS LONGCHAR NO-UNDO.
  DEF VAR cmtBegin  AS INT NO-UNDO.
  DEF VAR cmtEnd    AS INT NO-UNDO.
  DEF VAR iCnt      AS INT NO-UNDO.
  DEF VAR ifound    AS INT NO-UNDO.
  DEF VAR lStop     AS LOG NO-UNDO INIT FALSE.

  FOR EACH dwrpprog NO-LOCK
    WHERE dwrpprog.rappl = "DV"
    AND (IF cSide <> ? AND cSide <> "" THEN dwrpprog.cSide BEGINS(cSide) ELSE TRUE) :

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrplineprog NO-LOCK
      OF dwrpprog:
      ASSIGN
        cLong = clong + dwrplineprog.nline
        .
      IF INDEX(dwrplineprog.nline, cFilter) <> 0 THEN
        ifound = 1.
    END.   
    
    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
  
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*").
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
                    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END.

    IF INDEX(cLong, cFilter) <> 0 THEN
    DO:
      iCnt = 0.
      DO WHILE INDEX(cLong, cFilter) <> 0 OR ERROR-STATUS:ERROR :
        clong = SUBSTRING(cLong, INDEX(cLong, cFilter) + LENGTH(cFilter)) NO-ERROR.
        iCnt = iCnt + 1.
      END.

      IF iCnt > iLimit THEN
      DO:
        CREATE ttCompileLog.
        ASSIGN
          ttCompileLog.cType = "P"
          ttCompileLog.cSide = dwrpprog.cside
          ttCompileLog.cUserId = dwrpprog.rusermodi
          ttCompileLog.dDateMod = dwrpprog.dmodi
          ttCompileLog.dDateCre = dwrpprog.dcrea
          ttCompileLog.cUserCre = dwrpprog.rusercrea
          ttCompileLog.cProg = dwrpprog.nprog
          ttCompileLog.cFile = dwrpprog.cprog
          ttCompileLog.cDesc = cErr + ": " + STRING(iCnt) 
          .
      END.
    END.

  END.

  FOR EACH dwrphook NO-LOCK
     WHERE dwrphook.rappl = "DV" :

      FIND FIRST dwrpeven NO-LOCK
        WHERE dwrpeven.reven = dwrphook.reven NO-ERROR.
      IF AVAIL dwrpeven THEN
      DO:
        IF cSide <> ? AND cSide <> "" THEN
        DO:
          IF NOT(cSide BEGINS dwrpeven.rSide) THEN
            NEXT.
        END.

      END.

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrpchunhook NO-LOCK
      OF dwrphook:
      ASSIGN
        cLong = clong + dwrpchunhook.echun
        .
      IF INDEX(dwrpchunhook.echun, cFilter) <> 0 THEN
        ifound = 1.
    END.
    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
  
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*").
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
                    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END.

    IF INDEX(cLong, cFilter) <> 0 THEN
    DO:
      iCnt = 0.
      DO WHILE INDEX(cLong, cFilter) <> 0 OR ERROR-STATUS:ERROR :
        clong = SUBSTRING(cLong, INDEX(cLong, cFilter) + LENGTH(cFilter)) NO-ERROR.
        iCnt = iCnt + 1.
      END.

      IF iCnt > iLimit THEN
      DO:
        CREATE ttCompileLog.
        FIND FIRST dwrpscre NO-LOCK
          WHERE dwrpscre.rappl = "DV"
          AND   dwrpscre.rscre = INT(ENTRY(1,dwrphook.skey)) NO-ERROR.
        IF AVAIL dwrpscre THEN
          ASSIGN
            ttCompileLog.cType = "S"
            ttCompileLog.cUserId = dwrpscre.rusermodi
            ttCompileLog.dDateMod = dwrpscre.dmodi
            ttCompileLog.dDateCre = dwrpscre.dcrea
            ttCompileLog.cUserCre = dwrpscre.rusercrea
            ttCompileLog.cProg = dwrpscre.ntitl
            ttCompileLog.cFile = dwrpscre.cscre
            ttCompileLog.cSide = IF AVAIL dwrpeven THEN dwrpeven.rside ELSE cSide
            ttCompileLog.cDesc = cErr + ": " + STRING(iCnt) 
            ttCompileLog.cHook = (IF AVAIL dwrpeven THEN dwrpeven.neven ELSE "")
            .
      END.
    END.

  END.

END PROCEDURE.
                   
PROCEDURE pSearchCase:
/* ------------------------------------------- */
/*                                             */
/* ------------------------------------------- */
DEF INPUT PARAM cSide     AS CHAR NO-UNDO.
DEF INPUT PARAM cFilter   AS CHAR NO-UNDO.
DEF INPUT PARAM cErr     AS CHAR NO-UNDO.
DEF INPUT PARAM TABLE FOR ttCompilelog.

  DEF VAR cLong AS LONGCHAR NO-UNDO.
  DEF VAR cNew AS LONGCHAR NO-UNDO.
  DEF VAR cTmp AS LONGCHAR NO-UNDO.
  DEF VAR cmtBegin  AS INT NO-UNDO.
  DEF VAR cmtEnd    AS INT NO-UNDO.
  DEF VAR cTst      AS CHAR NO-UNDO.
  DEF VAR c         AS CHAR NO-UNDO.
  DEF VAR iCnt      AS INT NO-UNDO.
  DEF VAR iCnt2     AS INT NO-UNDO.
  DEF VAR ifound    AS INT NO-UNDO.
  DEF VAR lStop     AS LOG NO-UNDO INIT FALSE.

  FOR EACH dwrphook NO-LOCK
     WHERE dwrphook.rappl = "DV"  /*AND ENTRY(1,dwrphook.skey) = "747"*/ :

      FIND FIRST dwrpeven NO-LOCK
        WHERE dwrpeven.reven = dwrphook.reven NO-ERROR.
      IF AVAIL dwrpeven THEN
      DO:
        IF cSide <> ? AND cSide <> "" THEN
        DO:
          IF NOT(cSide BEGINS dwrpeven.rSide) THEN
            NEXT.
        END.

      END.

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrpchunhook NO-LOCK
      OF dwrphook:
      ASSIGN
        cLong = clong + dwrpchunhook.echun
        .
      IF INDEX(dwrpchunhook.echun, cFilter) <> 0 THEN
        ifound = 1.
    END.
    
    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
  
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*"). 
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
                    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END. 

    do while INDEX(cLong, ".p" ) <> 0 :
    /*DO:*/
      ASSIGN iFound = INDEX(cLong, ".p")           
        iCnt = 0
        .
       
      DO WHILE(SUBSTRING(cLong, iFound - iCnt,1) <> " ") :
        ASSIGN 
          iCnt = iCnt + 1
          cTst = SUBSTRING(cLong, iFound - iCnt,1)
          .
        IF   cTst = "("
          OR ctst = ","
          OR cTst = "~""
          OR cTst = "'"
          OR cTst = "}" THEN
          LEAVE.

      END.
      assign c = SUBSTRING(cLong, iFound - iCnt, iCnt + 2).
      /*message c.*/
      
      IF NOT(cTst = "(" OR ctst = "," OR cTst = "~"" OR cTst = "'" OR cTst = "}") 
        AND NOT (STRING( SUBSTRING(cLong, iCnt + 3, 1)) <> " " ) 
        and (not (trim(c) begins("tt"))) THEN
      /*IF STRING( SUBSTRING(cLong, iCnt + 3, 1)) <> " " THEN NEXT.   */
      do:
        IF ENCODE(STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2))) <>
          ENCODE(LOWER(STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2)))) THEN
        DO:
          /*if not (trim(c) begins("tt")) then
            message c view-as alert-box.*/
          FIND FIRST dwrpscre NO-LOCK
            WHERE dwrpscre.rappl = "DV"
            AND   dwrpscre.rscre = INT(ENTRY(1,dwrphook.skey)) NO-ERROR.
          IF AVAIL dwrpscre THEN
          do:
            find first dwrpeven no-lock where dwrpeven.reven = dwrphook.reven no-error.
            create ttcompileLog.
            ASSIGN
              ttCompileLog.cUserId = dwrpscre.rusermodi
              ttCompileLog.dDateMod = dwrpscre.dmodi
              ttCompileLog.dDateCre = dwrpscre.dcrea
              ttCompileLog.cUserCre = dwrpscre.rusercrea
              ttCompileLog.cProg = dwrpscre.ntitl
              ttCompileLog.cFile = dwrpscre.cscre
              ttCompileLog.cSide = IF AVAIL dwrpeven THEN dwrpeven.rside ELSE cSide
              ttCompileLog.cType = "S"
              ttCompileLog.cDesc = cErr + chr(10) + STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2))
              ttCompileLog.cHook = (IF AVAIL dwrpeven THEN dwrpeven.neven ELSE "")
             .
          END.
        END.
      end.
      assign clong = SUBSTRING(cLong, iFound + 2).
    END.
  end.


  FOR EACH dwrpprog NO-LOCK
    WHERE dwrpprog.rappl = "DV"
    AND (IF cSide <> ? AND cSide <> "" THEN dwrpprog.cSide BEGINS(cSide) ELSE TRUE) :

    ASSIGN cLong = ""
           lstop = FALSE
           ifound = 0.

    FOR EACH dwrplineprog NO-LOCK
      OF dwrpprog:
      ASSIGN
        cLong = clong + dwrplineprog.nline
        .
      IF INDEX(dwrplineprog.nline, ".p ") <> 0 THEN
        ifound = 1.
    END.   

    loop:  
    DO WHILE NOT(lStop) :
      cmtEnd = INDEX(cLong, "*/").
    
      IF cmtEnd = 0 THEN
      DO:
        lStop = TRUE.
        LEAVE loop.
      END.
      cNew = SUBSTRING(cLong, 1, cmtend ).
      cmtBegin = R-INDEX(cNew, "*"). 
                   /* if cmtbegin > cmtend then message cmtbegin cmtend view-as alert-box.*/
    
      ASSIGN    
        cTmp = SUBSTRING(cNew, 1, cmtbegin - 1) + SUBSTRING(cLong, cmtEnd + 2)
        cLong = cTmp
        .
    END. 
    
    do while INDEX(cLong, ".p" ) <> 0 :
    /*DO:*/
      ASSIGN iFound = INDEX(cLong, ".p")           
        iCnt = 0
        .
    
      DO WHILE(SUBSTRING(cLong, iFound - iCnt,1) <> " ") :
        ASSIGN 
          iCnt = iCnt + 1
          cTst = SUBSTRING(cLong, iFound - iCnt,1)
          .
        IF   cTst = "("
          OR ctst = ","
          OR cTst = "~""
          OR cTst = "'"
          OR cTst = "}" THEN
          LEAVE.
    
      END.
      assign c = SUBSTRING(cLong, iFound - iCnt, iCnt + 2).
      /*message c.*/
    
      IF NOT(cTst = "(" OR ctst = "," OR cTst = "~"" OR cTst = "'" OR cTst = "}") 
        AND NOT (STRING( SUBSTRING(cLong, iCnt + 3, 1)) <> " " ) 
        and (not (trim(c) begins("tt"))) THEN
      /*IF STRING( SUBSTRING(cLong, iCnt + 3, 1)) <> " " THEN NEXT.   */
      do:
        IF ENCODE(STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2))) <>
          ENCODE(LOWER(STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2)))) THEN
        DO:
          CREATE ttCompileLog.
          ASSIGN
            ttCompileLog.cSide = dwrpprog.cside
            ttCompileLog.cType = "P "
            ttCompileLog.cUserId = dwrpprog.rusermodi
            ttCompileLog.dDateMod = dwrpprog.dmodi
            ttCompileLog.dDateCre = dwrpprog.dcrea
            ttCompileLog.cUserCre = dwrpprog.rusercrea
            ttCompileLog.cProg = dwrpprog.nprog
            ttCompileLog.cFile = dwrpprog.cprog
            ttCompileLog.cDesc = cErr + chr(10) + STRING( SUBSTRING(cLong, iFound - iCnt, iCnt + 2))
            .
        END.
      end.
      assign clong = SUBSTRING(cLong, iFound + 2).
    END.

  END.
end procedure.



PROCEDURE pModifyPath:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cappl    AS CHAR      NO-UNDO.
DEF INPUT PARAM      cFrom    AS CHAR      NO-UNDO.
DEF INPUT PARAM      cTo      AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError   AS CHAR      NO-UNDO.

FIND FIRST dwapappl EXCLUSIVE-LOCK
  WHERE dwapappl.rappl = cAppl NO-ERROR.
IF AVAIL dwapappl THEN
DO:
  ASSIGN
    dwapappl.ndireclie     = REPLACE(dwapappl.ndireclie    , cFrom, cTo)
    dwapappl.ndirelibrclie = REPLACE(dwapappl.ndirelibrclie, cFrom, cTo)
    dwapappl.ndireserv     = REPLACE(dwapappl.ndireserv    , cFrom, cTo)
    dwapappl.ndirelibrserv = REPLACE(dwapappl.ndirelibrserv, cFrom, cTo)
    dwapappl.ndirelibrshar = REPLACE(dwapappl.ndirelibrshar, cFrom, cTo)
    .
  RELEASE dwapappl.
END.
ELSE
  ASSIGN cError = "No record lock"
    .
END PROCEDURE.


PROCEDURE pRemoveProgtest:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cappl    AS CHAR      NO-UNDO.
DEF INPUT PARAM      cList    AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError   AS CHAR      NO-UNDO.

DEF VAR cType   AS CHAR  NO-UNDO.
DEF VAR cName   AS CHAR  NO-UNDO.

IF SEARCH(cList) <> ? AND SEARCH(cList) <> "" THEN
  DO:
    INPUT FROM VALUE(SEARCH(cList)).
    REPEAT:
      IMPORT cType cName.
      IF cType <> ? AND cName <> ? THEN
      DO:
        CASE cType:
          WHEN "S" THEN
          DO:        
            FIND FIRST dwrpscre EXCLUSIVE-LOCK
              WHERE dwrpscre.cscre = cName NO-ERROR.
            IF AVAIL dwrpscre THEN
              DELETE dwrpscre.
          END.
          WHEN "P" THEN
          DO:        
            FIND FIRST dwrpprog EXCLUSIVE-LOCK
              WHERE dwrpprog.cprog = cName NO-ERROR.
            IF AVAIL dwrpprog THEN
              DELETE dwrpprog.
          END.
        END CASE.
      END.
      ELSE
      DO:
        ASSIGN cError = "Corrupt progtest list".
        LEAVE.
      END.
    END.
    INPUT CLOSE.
  END.
ELSE
  DO:
    ASSIGN
      cError = "Invalid progtest list location"
      .    
  END.


END PROCEDURE.

PROCEDURE pDWPDeployment:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  cappl    AS CHAR      NO-UNDO.
DEF OUTPUT PARAM cError   AS CHAR      NO-UNDO.


DEF VAR ninfo#      AS CHAR   NO-UNDO.
DEF VAR nwarn#      AS CHAR   NO-UNDO.
DEF VAR nerro#      AS CHAR   NO-UNDO.
DEF VAR dwplib      AS HANDLE NO-UNDO.

IF SEARCH("dwrpcachl1.r") <> ? THEN
DO:
  ASSIGN
    grlang# = "NL"
    giversdepl# = ?
    .
/*
/*UN dwpstart.p PERSISTENT SET ghstardwp#.*/
  RUN dwutsharl1.p PERSISTENT SET dwpbroker.
  MESSAGE "1"
    VIEW-AS ALERT-BOX.
  RUN dwutcliel1.r PERSISTENT SET ghlibrclie#.
  MESSAGE "2"
    VIEW-AS ALERT-BOX.

             MESSAGE "3"
               VIEW-AS ALERT-BOX.
             RUN dwrpcachl1.r PERSISTENT SET ghlibrcach# NO-ERROR.
             MESSAGE "4"
               VIEW-AS ALERT-BOX.
  RUN dwp_createRuntimeDeployment IN dwplib (INPUT 3,
                                                INPUT TRUE,
                                                INPUT TRUE,  /* Copy/Compile       */
                                                INPUT FALSE, /* Create DataBase    */
                                                INPUT TRUE,  /* Generate constants */
                                                INPUT FALSE, /* Generate prolib    */
                                                OUTPUT ninfo#,
                                                OUTPUT nwarn#,
                                                OUTPUT cError).*/
END.
ELSE
  ASSIGN
    cError = "Could not locate dwp libraries (deployment routines)"
    .

END PROCEDURE.

/* Can not be executed with a repository connected -> moved to screen */
/* PROCEDURE pFullDeploy:                                                           */
/* /*------------------------------------------------------------------------------ */
/*   Purpose:                                                                       */
/*   Parameters:  <none>                                                            */
/*   Notes:                                                                         */
/* ------------------------------------------------------------------------------*/ */
/* DEF INPUT PARAM      cappl    AS CHAR      NO-UNDO.                              */
/* DEF OUTPUT PARAM     cError   AS CHAR      NO-UNDO.                              */
/*                                                                                  */
/* IF SEARCH("d:\config\deployment\DEVtoDPC2.bat":U) <> ? THEN                      */
/* DO:                                                                              */
/*   OS-COMMAND SILENT VALUE(SEARCH("d:\config\deployment\DEVtoDPC2.bat")).         */
/*   IF SEARCH("d:\qadpc2\error.txt":U) <> ? THEN                                   */
/*   DO:                                                                            */
/*     INPUT FROM VALUE(SEARCH("d:\qadpc2\error.txt":U)).                           */
/*     REPEAT:                                                                      */
/*       IMPORT DELIMITER ",":U cError NO-ERROR.                                    */
/*       IF ERROR-STATUS:ERROR THEN                                                 */
/*         MESSAGE "Error reading logfile":U VIEW-AS ALERT-BOX ERROR.               */
/*         MESSAGE cError                                                           */
/*            VIEW-AS ALERT-BOX ERROR.                                              */
/*     END.                                                                         */
/*     INPUT CLOSE.                                                                 */
/*     OS-DELETE VALUE(SEARCH("d:\qadpc2\error.txt":U)).                            */
/*     IF OS-ERROR <> 0 THEN                                                        */
/*       MESSAGE "Error deleting file ":U SEARCH("error.txt":U)                     */
/*          VIEW-AS ALERT-BOX ERROR.                                                */
/*   END.                                                                           */
/* END.                                                                             */
/* ELSE                                                                             */
/*   ASSIGN                                                                         */
/*     cError = "Batch file missing".                                               */
/*                                                                                  */
/* END PROCEDURE.                                                                   */


PROCEDURE pImport:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cfiles   AS CHAR      NO-UNDO.
DEF INPUT PARAM      lcomp    AS LOG       NO-UNDO.
DEF OUTPUT PARAM     cError   AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cWarn    AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cInfo    AS CHAR      NO-UNDO.

DEF VAR cFile      AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.


DO i = 1 TO NUM-ENTRIES(cFiles) :

  ASSIGN
    cFile = SEARCH(ENTRY(i, cFiles))
    .
  
  IF cFile <> ? THEN
  DO:
    /*RUN dwp_importFile in dwpbroker (INPUT cFile, 
                                     INPUT lComp, /* Compile after import -> usually false because compile is done when all files are imported */
                                     OUTPUT cError, 
                                     OUTPUT cWarn, 
                                     OUTPUT cInfo).*/
    RUN dwrpimpob2.p (INPUT cFile,
                      INPUT FALSE,
                      OUTPUT cError,
                      OUTPUT cWarn,
                      OUTPUT cInfo).

    IF cError <> "" THEN
    DO:
      ASSIGN
        cError = cFile + " contains an error"
        .
      LEAVE.
    END.
  END.
  ELSE
  DO:
    ASSIGN
      cError = IF cFile <> ? THEN cFile + " not found" ELSE "File not found"
      .
    LEAVE.
  END.

END.

END PROCEDURE.


PROCEDURE pDelFromRepository:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cListScre   AS CHAR      NO-UNDO.
DEF INPUT PARAM      cListProg   AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError   AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cInfo    AS CHAR      NO-UNDO.

DEF VAR cFile      AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.


DO i = 1 TO NUM-ENTRIES(cListScre) :

/*   FIND FIRST dwrpscre NO-LOCK */
/*                               */
END.

END PROCEDURE.


PROCEDURE pCleanup:
/*------------------------------------------------------------------------------
  Purpose: Cleanup a folder based     
  Parameters:  cfolder: folder to clean, cFilter: ; separated list
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cFolder     AS CHAR      NO-UNDO.
DEF INPUT PARAM      cFilter     AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

DEF VAR cFile      AS CHAR NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.

IF cFilter = "" THEN
  ASSIGN
    cError = "Cleanup parameters not correct"
    .

ASSIGN cFolder = RIGHT-TRIM(cFolder, "\/").
FILE-INFO:FILE-NAME = cFolder.

IF FILE-INFO:FILE-TYPE MATCHES "*D*" THEN
DO i = 1 TO NUM-ENTRIES(cFilter, ";"):
  OS-COMMAND SILENT DEL VALUE(cFolder + "\" + ENTRY(i, cFilter, ";")) /F.
END.
ELSE
  ASSIGN
    cError = "Cleanup folder not correct"
    .

END PROCEDURE.


PROCEDURE pCleanManual:
/*------------------------------------------------------------------------------
  Purpose: Cleanup a folder based     
  Parameters:  cfolder: folder to clean, cFilter: ; separated list
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cFolder     AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

OS-COMMAND SILENT explorer VALUE(cFolder).

END PROCEDURE.

PROCEDURE pGetFreeSpace:
/*------------------------------------------------------------------------------
  Purpose:  64bit value!!
    Notes:  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM cPath        AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM iSpace       AS INT64 NO-UNDO.

  DEF VAR mpFreeBytes      AS MEMPTR NO-UNDO.
  DEF VAR mpTotalBytes     AS MEMPTR NO-UNDO.
  DEF VAR mpTotalFreeBytes AS MEMPTR NO-UNDO.
  DEF VAR retval           AS INT    NO-UNDO.

  ASSIGN
    cPath = TRIM(cPath,"/\") + "/"
    .
  SET-SIZE(mpFreeBytes) = 8.
  SET-SIZE(mpTotalBytes) = 8.
  SET-SIZE(mpTotalFreeBytes) = 8.

  RUN GetDiskFreeSpaceExA (INPUT string(cPath + CHR(0)),
                           OUTPUT mpFreeBytes,
                           OUTPUT mpTotalBytes,
                           OUTPUT mpTotalFreeBytes,
                           OUTPUT retval).
  IF retval <> 1 THEN
    DO:
      iSpace = ?.
      RETURN.
    END.

  ASSIGN iSpace = GET-INT64(mpTotalFreeBytes,1).

  /* Cleanup */
  SET-SIZE(mpFreeBytes) = 0.
  SET-SIZE(mpTotalBytes) = 0.
  SET-SIZE(mpTotalFreeBytes) = 0.

END PROCEDURE.


PROCEDURE pRunScript:
/*------------------------------------------------------------------------------
  Purpose: execute a local batch file   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cValue      AS CHAR      NO-UNDO.
DEF INPUT PARAM      cParam      AS CHAR      NO-UNDO.
DEF INPUT PARAM      cErrorLog   AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.
/* set srcpath=D:\qadpc\prepdeploy\client       */
/* set srvpath=D:\qadpc\prepdeploy\server       */
/* set destpath=d:\qadpc\deploy\content         */
/* set log=d:\qadpc\deploy\content\createpl.log */

IF SEARCH(cErrorLog) <> ? THEN
DO:
  OS-DELETE VALUE(SEARCH(cErrorLog)).
  IF OS-ERROR <> 0 THEN
  DO:      
    ASSIGN cError = fGetOsError(OS-ERROR).
    RETURN.
  END.
END.

IF SEARCH(cValue) <> ? THEN
DO:
  OS-COMMAND SILENT VALUE(cValue + " " + cParam + " " + cErrorLog).
  IF SEARCH(cErrorlog) <> ? THEN
  DO:
    INPUT FROM VALUE(SEARCH(cErrorLog)).
    IMPORT UNFORMATTED cError.
    INPUT CLOSE.
  END.
END.
ELSE
  ASSIGN
    cError = "Could not locate script: " + cValue
    .
  
END PROCEDURE.


PROCEDURE pCopy:
/*------------------------------------------------------------------------------
  Purpose: copy from source dir to dest dir using progress commands
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM cSrc     AS CHAR NO-UNDO.
DEF INPUT PARAM cDest    AS CHAR NO-UNDO.
DEF INPUT PARAM cFilt    AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError  AS CHAR NO-UNDO.

DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR cFull AS CHAR NO-UNDO.
DEF VAR cAttr AS CHAR NO-UNDO.

FILE-INFO:FILE-NAME = cDest.
IF FILE-INFO:FILE-TYPE = ? THEN
DO:
  OS-CREATE-DIR VALUE(cDest).
  IF OS-ERROR <> 0 THEN
  DO:
    ASSIGN
      cError = fGetOsError(OS-ERROR)
      .
    RETURN.
  END.
END.

INPUT FROM OS-DIR(cSrc).
REPEAT :
  IMPORT cFile cFull cAttr.
  IF cFile <> "." AND cFile <> ".."  THEN
  DO ON ERROR UNDO, THROW:
    IF cFilt <> "" AND NOT(cFile MATCHES("*" + cFilt))  THEN
    DO:
      NEXT.
    END.
    IF cAttr MATCHES "*D*" THEN
    DO:
      OS-CREATE-DIR VALUE(cDest + "\" + cFile).
      IF OS-ERROR <> 0 AND OS-ERROR <> 10 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
      RUN pCopy (cSrc + "\" + cFile, cDest + "\" + cFile, cFilt, OUTPUT cError).
      IF OS-ERROR <> 0 AND OS-ERROR <> 10 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
    END.
    ELSE
    DO:
      OS-COPY VALUE(cSrc + "\" + cFile) VALUE(cDest + "\" + cFile).
      IF OS-ERROR <> 0 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
    END.
    PROCESS EVENTS.

    CATCH eError AS Progress.Lang.Error:
      ASSIGN cError = eError:getmessage(1).
      DELETE OBJECT eError.    
    END CATCH.
  END.
END.

INPUT CLOSE.
END PROCEDURE.


PROCEDURE pCopyWithoutSub:
/*------------------------------------------------------------------------------
  Purpose: copy from source dir to dest dir using progress commands
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM cSrc     AS CHAR NO-UNDO.
DEF INPUT PARAM cDest    AS CHAR NO-UNDO.
DEF INPUT PARAM cFilt    AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError  AS CHAR NO-UNDO.

DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR cFull AS CHAR NO-UNDO.
DEF VAR cAttr AS CHAR NO-UNDO.

FILE-INFO:FILE-NAME = cDest.
IF FILE-INFO:FILE-TYPE = ? THEN
DO:
  OS-CREATE-DIR VALUE(cDest).
  IF OS-ERROR <> 0 THEN
  DO:
    ASSIGN
      cError = fGetOsError(OS-ERROR)
      .
    RETURN.
  END.
END.

INPUT FROM OS-DIR(cSrc).
REPEAT :
  IMPORT cFile cFull cAttr.
  IF cFile <> "." AND cFile <> ".."  THEN
  DO ON ERROR UNDO, THROW:
    IF cFilt <> "" AND NOT(cFile MATCHES("*" + cFilt))  THEN
    DO:
      NEXT.
    END.

    IF cAttr MATCHES "*D*" THEN
    DO: /*
      OS-CREATE-DIR VALUE(cDest + "\" + cFile).
      IF OS-ERROR <> 0 AND OS-ERROR <> 10 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
      RUN pCopy (cSrc + "\" + cFile, cDest + "\" + cFile, cFilt, OUTPUT cError).
      IF OS-ERROR <> 0 AND OS-ERROR <> 10 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.*/
    END.
    ELSE
    DO:
      OS-COPY VALUE(cSrc + "\" + cFile) VALUE(cDest + "\" + cFile).
      IF OS-ERROR <> 0 THEN
      DO:
        ASSIGN cError = fGetOsError(OS-ERROR).
        MESSAGE "cError IN pCopyWithoutSub (dep-lib.p):" cError
          VIEW-AS ALERT-BOX.
        RETURN.
      END.
    END.

    CATCH eError AS Progress.Lang.Error:
      ASSIGN cError = eError:getmessage(1).
      DELETE OBJECT eError.    
    END CATCH.
  END.
END.

INPUT CLOSE.
END PROCEDURE.


PROCEDURE pWebclient:
/*------------------------------------------------------------------------------
  Purpose: Launch webclient
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM      cParam      AS CHAR      NO-UNDO.
  DEF INPUT PARAM      cAppl       AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cBefore     AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cAfter      AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.
  DEF VAR cTemp      AS CHAR NO-UNDO.

  DO ON ERROR UNDO, THROW:
    LOAD cParam.
    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN
        cError = "Load " + cparam + " failed: " + ERROR-STATUS:GET-MESSAGE(1)
        .

      LEAVE.
    END.
    USE cParam.
        
    GET-KEY-VALUE SECTION "Project" KEY "LatestProwcappVersion" VALUE cTemp.
    GET-KEY-VALUE SECTION "Versions" KEY cTemp VALUE cBefore.
    UNLOAD cParam NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN
        cError = "Unload " + cparam + " failed"
        .
      LEAVE.
    END.
  
    OS-COMMAND SILENT VALUE (cAppl + " " + cParam).
    PROCESS EVENTS.
    PROCESS EVENTS.
    LOAD cParam.
    USE  cParam.
    
    GET-KEY-VALUE SECTION "Project" KEY "LatestProwcappVersion" VALUE cTemp.
    GET-KEY-VALUE SECTION "Versions" KEY cTemp VALUE cAfter.
    UNLOAD cParam.
  
    IF cBefore = cAfter THEN
      ASSIGN cError = "Warning:No new package was created".
    ELSE if cAfter MATCHES("*fail*") THEN
      ASSIGN cError = "Error during webclient package generation".
  
    CATCH eError AS Progress.Lang.Error:
      ASSIGN cError = eError:getmessage(1).
      DELETE OBJECT eError.    
    END CATCH.
  END.
END PROCEDURE.


PROCEDURE pCopyToUnix:
/*------------------------------------------------------------------------------
  Purpose: Copy everything to unix (d2devel by default)
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM      cParam      AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

    
END PROCEDURE.


PROCEDURE pCallAppserver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM      cApps       AS CHAR      NO-UNDO.
DEF INPUT PARAM      cProc       AS CHAR      NO-UNDO.
DEF INPUT PARAM      cParam      AS CHAR      NO-UNDO.
DEF INPUT PARAM      cExt        AS CHAR      NO-UNDO.
DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.


DEFINE VAR spropshar# AS CHAR   NO-UNDO.
DEFINE VAR iversdepl# AS INT    NO-UNDO.
DEFINE VAR nerro#     AS CHAR   NO-UNDO.
DEFINE VAR iError     AS INT    NO-UNDO.
DEFINE VAR iCnt       AS INT    NO-UNDO.
DEFINE VAR lOk        AS LOG    NO-UNDO.
DEFINE VAR hCall      AS HANDLE NO-UNDO. 
DEFINE VAR hApps      AS HANDLE NO-UNDO. 

IF cProc = ? OR cProc = "" THEN
DO:
    ASSIGN cProc = "Missing procedure name".
    LEAVE.
END.

/* Specific deployment appserver asdeploy */
CREATE SERVER hApps.
IF NOT hApps:CONNECT(cApps) THEN
DO:
  ASSIGN
    cError = "Could not connect to deployment appserver"
    .
  LEAVE.
END.
ELSE
DO:
  CREATE CALL hCall. 
  
  hCall:CALL-NAME = cProc.
  hCall:SERVER = hApps.
  IF cExt <> ? AND cExt <> "" THEN
    DO:
      hCall:NUM-PARAMETERS = 3. 
      hCall:SET-PARAMETER( 1, "CHARACTER", "INPUT", cParam ).
      hCall:SET-PARAMETER( 2, "CHARACTER", "INPUT", cExt ).
      hCall:SET-PARAMETER( 3, "CHARACTER", "OUTPUT", cError).
    END.
  ELSE IF cParam <> ? AND cParam <> "" THEN
    DO:
      hCall:NUM-PARAMETERS = 2. 
      hCall:SET-PARAMETER( 1, "CHARACTER", "INPUT", cParam ).
      hCall:SET-PARAMETER( 2, "CHARACTER", "OUTPUT", cError).
    END.
  ELSE
    DO:
      hCall:NUM-PARAMETERS = 1. 
      hCall:SET-PARAMETER( 1, "CHARACTER", "OUTPUT", cError).
    END.
  DO ON ERROR UNDO, THROW:
    hCall:INVOKE.      
    
    CATCH eError AS Progress.Lang.Error:
       ASSIGN cError = eError:getmessage(1) .       
      DELETE OBJECT eError.    
    END CATCH.
  END.
END.

IF VALID-HANDLE(hApps) THEN
DO:
  hApps:DISCONNECT() NO-ERROR.
  DELETE OBJECT hApps NO-ERROR.
END.

IF VALID-HANDLE(hCall) THEN
  DELETE OBJECT hCall NO-ERROR.

END PROCEDURE.

PROCEDURE pCreateVersion:
/*------------------------------------------------------------------------------
  Purpose: Create DVversion file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM      cDest       AS CHAR      NO-UNDO.
  DEF INPUT PARAM      cVersion    AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

  FILE-INFO:FILENAME = cDest.
  IF FILE-INFO:PATHNAME <> ? THEN
  DO:
    OUTPUT TO VALUE(cDest + "/DVversion").
    PUT UNFORMATTED cVersion SKIP.
    OUTPUT CLOSE.
    IF SEARCH(cDest + "/DVversion") = ? THEN
      ASSIGN
        cError = "Unable to created version file (DVversion)"
        .
  END.
  ELSE
    ASSIGN
      cError = "Invalid destination(cDest)"
      .
END PROCEDURE.

PROCEDURE pCreatePostMouli:
/*------------------------------------------------------------------------------
  Purpose: Create postmouli.txt file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM      cDest       AS CHAR      NO-UNDO.
  DEF INPUT PARAM      cMouli      AS CHAR      NO-UNDO.
  DEF INPUT PARAM      cDir        AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

  DEF VAR cProg    AS CHAR NO-UNDO.

/*   FILE-INFO:FILENAME = cDest.     */
/*   IF FILE-INFO:PATHNAME <> ? THEN */
/*   DO:                             */
    FILE-INFO:FILENAME = cDest.
    IF FILE-INFO:PATHNAME <> ? THEN
    DO:
      OS-DELETE VALUE(FILE-INFO:PATHNAME).
      IF OS-ERROR <> 0 THEN
      DO:      
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
    END.
    OUTPUT TO VALUE(cDest).
    DO iCnt = 1 TO NUM-ENTRIES(cMouli, "|") BY 2:
      ASSIGN cProg = ENTRY(iCnt,cMouli, "|").
      IF INDEX(cProg, ".p") <> 0 THEN
        cProg = SUBSTRING(cProg, 1, INDEX(cProg, ".p") - 1).
      IF INDEX(cProg, ".r") = 0 THEN
        cProg = cProg + ".r".
      IF SEARCH(cProg) = ? THEN
      DO:
        ASSIGN
          cError = "Cannot find " + cProg
          .
        RETURN. 
      END.
      ELSE IF INDEX(SEARCH(cProg), cDir) = 0 THEN
        DO:
          ASSIGN
            cError = "Cannot find in " + cDir + cProg
            .
          RETURN. 
        END.
      PUT UNFORMATTED cProg SKIP.
    END.
    OUTPUT CLOSE.
    IF SEARCH(cDest) = ? THEN
      ASSIGN
        cError = "Unable to create moulinette file: " + cDest
        .
/*  END.                                        */
/*   ELSE                                       */
/*     ASSIGN                                   */
/*       cError = "Invalid destination(cDest)"  */
/*       .                                      */

END PROCEDURE.


PROCEDURE pCreateOrder:
/*------------------------------------------------------------------------------
  Purpose: Create ORDER file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM      cDest       AS CHAR      NO-UNDO.
  DEF INPUT PARAM      cOrder      AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

  FILE-INFO:FILENAME = cDest.
  IF FILE-INFO:PATHNAME <> ? THEN
  DO:
    FILE-INFO:FILENAME = cDest + "/ORDER".
    IF FILE-INFO:PATHNAME <> ? THEN
    DO:
      OS-DELETE VALUE(FILE-INFO:PATHNAME).
      IF OS-ERROR <> 0 THEN
      DO:      
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
    END.
    OUTPUT TO VALUE(cDest + "/ORDER").
    DO iCnt = 1 TO NUM-ENTRIES(cOrder, ";"):
/*      IF iCnt <  NUM-ENTRIES(cOrder, ";") THEN*/
        PUT UNFORMATTED ENTRY(iCnt,cOrder, ";") SKIP.
/*      ELSE
        PUT UNFORMATTED ENTRY(iCnt,cOrder, ";").*/
    END.
    OUTPUT CLOSE.
    IF SEARCH(cDest + "/ORDER") = ? THEN
      ASSIGN
        cError = "Unable to create order file (ORDER)"
        .
  END.
  ELSE
    ASSIGN
      cError = "Invalid destination(cDest)"
      .
END PROCEDURE.


PROCEDURE pCreateParam:
/*------------------------------------------------------------------------------
  Purpose: Create param file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM cDest       AS CHAR NO-UNDO.
  DEF INPUT  PARAM TABLE-HANDLE htt.
  DEF OUTPUT PARAM cError      AS CHAR NO-UNDO.

  DEF VAR hQry    AS HANDLE NO-UNDO.
  DEF VAR hBuff   AS HANDLE NO-UNDO.
  CREATE QUERY hQry.
  hBuff = htt:DEFAULT-BUFFER-HANDLE.
  hQry:ADD-BUFFER(hBuff).
  
  hQry:QUERY-PREPARE("FOR EACH ttDwSyPara NO-LOCK").
  hQry:QUERY-OPEN.

  FILE-INFO:FILENAME = cDest.
  IF FILE-INFO:PATHNAME <> ? THEN
  DO:
    FILE-INFO:FILENAME = cDest + "/param".
    IF FILE-INFO:PATHNAME <> ? THEN
    DO:
      OS-DELETE VALUE(FILE-INFO:PATHNAME).
      IF OS-ERROR <> 0 THEN
      DO:      
        ASSIGN cError = fGetOsError(OS-ERROR).
        RETURN.
      END.
    END.
    OUTPUT TO VALUE(cDest + "/param").
    REPEAT:
      hQry:GET-NEXT().  
      IF hQry:QUERY-OFF-END THEN LEAVE.
      PUT UNFORMATTED hBuff::rpara "," hBuff::nvalu "," STRING(hBuff::lCach) "," STRING(hBuff::ldown) SKIP.
    END.
    OUTPUT CLOSE.
    hQry:QUERY-CLOSE().
    DELETE OBJECT hQry.

    IF SEARCH(cDest + "/param") = ? THEN
      ASSIGN
        cError = "Unable to create system parameter file (param)"
        .
  END.
  ELSE
    ASSIGN
      cError = "Invalid destination(cDest)"
      .
END PROCEDURE.


PROCEDURE pGenerateCode:
/*------------------------------------------------------------------------------
  Purpose: Create postmouli.txt file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

  DEF VAR i                AS INT  NO-UNDO.
  DEF VAR cInfo            AS CHAR NO-UNDO.

  RUN dwp_generateApplicationHooks IN dwpbroker (INPUT "DV", OUTPUT cerror, OUTPUT cinfo).
END PROCEDURE.


PROCEDURE pCalculateCrc:
/*------------------------------------------------------------------------------
  Purpose: Calculate the driverdb DF code
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM     iCrc        AS INT       NO-UNDO.
DEF OUTPUT PARAM     cError      AS CHAR      NO-UNDO.

FOR EACH _File NO-LOCK
  WHERE driver._File._Tbl-Type = "T":
  ASSIGN iCRC = iCRC + _File._CRC NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
    ASSIGN cError = ERROR-STATUS:GET-MESSAGE(1).
    LEAVE.
  END.
END.

END PROCEDURE.

PROCEDURE pCreateCRC:
/*------------------------------------------------------------------------------
  Purpose: Create crc file
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM         cDest       AS CHAR      NO-UNDO.
  DEF INPUT-OUTPUT PARAM  cCrc        AS CHAR      NO-UNDO.
  DEF OUTPUT PARAM        cError      AS CHAR      NO-UNDO.

  IF cCrc = ? OR cCrc = "" THEN
    RUN pCalculateCrc(OUTPUT cCrc, OUTPUT cError).

  IF LENGTH(cError) = 0  AND LENGTH(cCrc) > 0 THEN
  DO:
    FILE-INFO:FILENAME = cDest.
    IF FILE-INFO:PATHNAME <> ? THEN
    DO:
      OUTPUT TO VALUE(cDest + "/" + cCrc + ".crc").
      PUT UNFORMATTED cCrc SKIP.
      OUTPUT CLOSE.
      IF SEARCH(cDest + "/" + cCrc + ".crc") = ? THEN
        ASSIGN
          cError = "Unable to create the crc file"
          .
    END.
    ELSE
      ASSIGN
        cError = "Invalid destination(cDest)"
        .
  END.
END PROCEDURE.


PROCEDURE pDelete:
/*------------------------------------------------------------------------------
  Purpose: Delete with the option of using wildcards
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAM cSrc     AS CHAR NO-UNDO.
DEF INPUT PARAM cFilt    AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError  AS CHAR NO-UNDO.

DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR cFull AS CHAR NO-UNDO.
DEF VAR cAttr AS CHAR NO-UNDO.

FILE-INFO:FILE-NAME = cSrc.
IF FILE-INFO:FILE-TYPE = ? THEN
DO:
  cError = "WARNING:Source path not found: " + cSrc.
  RETURN.
END.

INPUT FROM OS-DIR(cSrc).
REPEAT :
  IMPORT cFile cFull cAttr.
  IF cFile <> "." AND cFile <> ".."  THEN
  DO ON ERROR UNDO, THROW:
    IF cFilt <> "" AND NOT(cFile MATCHES("*" + cFilt)) THEN
    DO:
      NEXT.
    END.

    OS-DELETE VALUE(cFull) RECURSIVE.
    IF OS-ERROR <> 0 THEN
    DO:
      ASSIGN cError = fGetOsError(OS-ERROR).
      RETURN.
    END.

    CATCH eError AS Progress.Lang.Error:
      ASSIGN cError = eError:getmessage(1).
      DELETE OBJECT eError.    
    END CATCH.
  END.
END.
INPUT CLOSE.

END PROCEDURE.


PROCEDURE pGetDiffs:
/*------------------------------------------------------------------------------
  Purpose:    Find the differences in .r code between to folders
  Parameters:  
  Notes:      This routine can replace rsync in certain cases, because rsync 
              doesn't have access to the md5-value in a .r
------------------------------------------------------------------------------*/
DEF INPUT  PARAM cToDir  AS CHAR NO-UNDO.
DEF INPUT  PARAM cNewDir AS CHAR NO-UNDO.
DEF INPUT  PARAM cOldDir AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError  AS CHAR NO-UNDO.

DEF VAR cWcm         AS CHAR NO-UNDO.
DEF VAR cFile        AS CHAR NO-UNDO.
DEF VAR cPrevFile    AS CHAR NO-UNDO.
DEF VAR cFileRC      AS CHAR NO-UNDO.
DEF VAR cPrevFileRC  AS CHAR NO-UNDO.

OUTPUT TO VALUE(cToDir + "diffs.txt").
INPUT FROM OS-DIR(cNewDir).
REPEAT:
  IMPORT cFile.
  IF LENGTH(cFile) < 3 THEN
      NEXT.

  IF cFile MATCHES "*.r" THEN
  DO:
    FILE-INFO:FILE-NAME = cNewDir + cfile.
    /* New file */
    IF FILE-INFO:PATHNAME = ? THEN
      PUT UNFORMATTED cfile ",A" SKIP.
    ELSE
    DO:
      RCODE-INFO:FILE-NAME = cNewDir + cfile.
      cFileRC = RCODE-INFO:MD5-VALUE.
      RCODE-INFO:FILE-NAME = cOldDir + cfile.
      cPrevFileRC = RCODE-INFO:MD5-VALUE.

      /* File changed */
      IF cFileRC <> cPrevFileRC THEN
        PUT UNFORMATTED cfile ",R" SKIP.
    END.

  END.
END.
INPUT CLOSE.

/* Find the deleted files */
INPUT FROM OS-DIR(cOldDir).
REPEAT:
  IMPORT cFile.
  IF LENGTH(cFile) < 3 THEN
      NEXT.

  IF cFile MATCHES "*.r" THEN
  DO:
    FILE-INFO:FILE-NAME = cNewDir + cfile.

    /* Deleted file */
    IF FILE-INFO:PATHNAME = ? THEN
      PUT UNFORMATTED cfile ",D" SKIP.

  END.
END.
INPUT CLOSE.

OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE pMsgNumbers:
/*------------------------------------------------------------------------------
  Purpose:    Adjust the message numbers eg text[xxx]
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM cError  AS CHAR NO-UNDO.

  FOR EACH dwrpmesstran WHERE dwrpmesstran.rappl = "DV" EXCLUSIVE-LOCK:
    IF R-INDEX(dwrpmesstran.nmess,"]") = 0 THEN 
      DO:
        ASSIGN dwrpmesstran.nmess = dwrpmesstran.nmess + " [ " + STRING(dwrpmesstran.rmess)  + " ]" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          RETURN ERROR-STATUS:GET-MESSAGE(1).
      END.      
  END.
END PROCEDURE.


PROCEDURE pSetupXml:
/*------------------------------------------------------------------------------
  Purpose:    Export the setup xml
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
DEF INPUT  PARAM cToDir   AS CHAR NO-UNDO.
DEF INPUT  PARAM cTables  AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError   AS CHAR NO-UNDO.

DEF VAR wlError AS LOG  NO-UNDO.

EMPTY TEMP-TABLE dumptablett.

DO iCnt = 1 TO NUM-ENTRIES(cTables):
  CREATE dumptablett.
  ASSIGN dumptablett.parenttable = ENTRY(iCnt, cTables)
         .
END.

RUN dv-s-dumpdbcontent2.p
    (INPUT {&SetupXml},
     INPUT TABLE dumptablett BIND,
     INPUT ?, 
     INPUT cToDir,
     INPUT 0,
     OUTPUT wlError,
     OUTPUT cError).

IF wlError AND (cError = "" OR cError = ?) THEN
    ASSIGN
      cError = "Unknown error during setup xml export"
    .

END PROCEDURE.

PROCEDURE pCreatePlVersion:
/*------------------------------------------------------------------------------
  Purpose:    Generate a small program to check diclient.pl
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
DEF INPUT  PARAM cDest     AS CHAR NO-UNDO.
DEF INPUT  PARAM cVersion  AS CHAR NO-UNDO.
DEF OUTPUT PARAM cError    AS CHAR NO-UNDO.

DEF VAR wlError AS LOG  NO-UNDO.

  FILE-INFO:FILENAME = cDest.
  IF FILE-INFO:PATHNAME <> ? THEN
  DO:
    OUTPUT TO VALUE(cDest + "/" + SUBSTRING(cVersion,1,6) + ".txt").
    PUT UNFORMATTED "dummy version check" SKIP.
    OUTPUT CLOSE.
    IF SEARCH(cDest + "/" + SUBSTRING(cVersion,1,6) + ".txt") = ? THEN
      ASSIGN
        cError = "Unable to created pl version file (plversion.txt)"
        .
  END.
  ELSE
    ASSIGN
      cError = "Invalid destination(cDest)"
      .

END PROCEDURE.



