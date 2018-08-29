 /*
  For the geeks https://en.wikipedia.org/wiki/List_of_algorithms
                    https://en.wikipedia.org/wiki/Backtracking 
                    https://en.wikipedia.org/wiki/Branch_and_bound
                    */

define temp-table ttequipment no-undo
 field id      as integer 
 field surface as decimal
 field truck   as integer initial 0
  /* index truck truck surface */ .
empty temp-table ttequipment.

define temp-table tttruck no-undo 
    field id      as integer 
    field surface as decimal .
empty temp-table tttruck.

create ttequipment.
assign ttequipment.id      = 1
       ttequipment.surface = 9.09.
create ttequipment.
assign ttequipment.id      = 2
       ttequipment.surface = 9.
create ttequipment.
assign ttequipment.id      = 3
       ttequipment.surface = 8.
create ttequipment.
assign ttequipment.id      = 4
       ttequipment.surface = 15.
create ttequipment.
assign ttequipment.id      = 5
       ttequipment.surface = 15.
create ttequipment.
assign ttequipment.id     = 6
       ttequipment.surface= 8.33.
create ttequipment.
assign ttequipment.id      = 7
       ttequipment.surface = 25.
create ttequipment.
assign ttequipment.id      = 8
       ttequipment.surface = 25.
create ttequipment.
assign ttequipment.id      = 9
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 10
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 11
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 12
       ttequipment.surface = 8.33.
create ttequipment.
assign ttequipment.id      = 13
       ttequipment.surface = 16.67.
create ttequipment.
assign ttequipment.id      = 14
       ttequipment.surface = 16.67.
create ttequipment.
assign ttequipment.id      = 15
       ttequipment.surface = 25.
create ttequipment.
assign ttequipment.id      = 16
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 17
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 18
       ttequipment.surface = 33.33.
create ttequipment.
assign ttequipment.id      = 19
       ttequipment.surface = 50.
create ttequipment.
assign ttequipment.id     = 20
       ttequipment.surface= 66.67.

/* tt-transportcost  */ 
DEFINE VARIABLE iaantaltrucks AS INTEGER NO-UNDO initial 0.
define VARIABLE llogging      as logical no-undo initial no.
DEFINE VARIABLE aantal        AS INTEGER NO-UNDO.
llogging = yes.
 
FUNCTION vultruck returns integer (idtruck as integer , isurface as decimal ) :

  define buffer bttequipment for ttequipment.
  define variable rest       as integer no-undo.
  define variable truckfound as logical no-undo initial no .
  
  if llogging then disp idtruck isurface etime no-label .
  rest = 100 - isurface .
  FOR first bttequipment exclusive-lock where bttequipment.truck = 0 and bttequipment.surface <= rest :
    assign 
      bttequipment.truck = idtruck
      isurface           = isurface + bttequipment.surface
      truckfound         = true.
  END.

  IF truckfound and isurface < 100 THEN vultruck(idtruck, isurface).
  
  IF not truckfound or isurface = 100
  then do : 
    tttruck.surface = isurface .
    return (0)  .
  END.

END function .
 

FUNCTION gettrucks returns integer () :

  DEFINE VARIABLE lavailequimpent AS LOGICAL NO-UNDO initial yes.
  
  /* equipment met laadvermogen >= 50 % */
  DO while lavailequimpent :
    lavailequimpent = true.
    find first ttequipment exclusive-lock where ttequipment.surface >= 50 and ttequipment.truck = 0 no-error.
    IF not avail ttequipment THEN lavailequimpent = false . 
    else do : 
      create tttruck.
      assign 
        iaantaltrucks     = iaantaltrucks + 1
        tttruck.id        = iaantaltrucks
        tttruck.surface   = ttequipment.surface.
        ttequipment.truck = tttruck.id.
      release ttequipment.
    END.
  END.
  
  /* Aanvullen van de tricks */ 
  FOR each tttruck no-lock where tttruck.surface <= 100 :
    vultruck(tttruck.id, tttruck.surface). 
  END.
  
  /* overige equipment op trucks plaatsen */
  lavailequimpent = true.
  DO while lavailequimpent :
    lavailequimpent = true.
    find first ttequipment where ttequipment.truck = 0 no-error .
    IF not avail ttequipment
    THEN lavailequimpent = false . 
    else do : 
      create tttruck.
      assign 
        iaantaltrucks     = iaantaltrucks + 1
        tttruck.id        = iaantaltrucks
        tttruck.surface   = ttequipment.surface
        ttequipment.truck = tttruck.id .
      vultruck(tttruck.id, tttruck.surface). 
    END.
  END.
  
  return iaantaltrucks.
END function .

/* Bij meerder berekenig wijzen kan je hier beslissen welk resultaat te nemen */
FUNCTION validateBranches returns integer () :
  define variable iaantal as integer no-undo.
  iaantal = gettrucks().
  return iaantal.
END function .

IF llogging 
THEN do : 
  output to "c:\temp\truck.txt".
  etime(true) .
END.

aantal = gettrucks() .

IF llogging 
THEN do : 
  FOR each tttruck : 
    disp  iaantaltrucks tttruck.id tttruck.surface.
  END.
  FOR each ttequipment : 
    disp  ttequipment.surface ttequipment.truck.
  END.
  disp (etime).
  output close. 
END.
MESSAGE aantal VIEW-AS ALERT-BOX INFO BUTTONS OK.
