FUNCTION htmlEncode RETURNS CHARACTER
  (INPUT p_in AS CHARACTER):
  /****************************************************************************
  Description: Converts various ASCII characters to their HTML representation
    to prevent problems with invalid HTML.  This procedure can only be called
    once on a string or ampersands will incorrectly be replaced with "&amp; .
  Input Parameter: Character string to encode
  Returns: Encoded character string
  ****************************************************************************/
   
   
/* Ampersand must be replaced first or the output will be hosed if done
   after any of these other subsititutions. */
  ASSIGN
    p_in = REPLACE(p_in, "&":U, "&amp~;":U)       /* ampersand */
    p_in = REPLACE(p_in, "~"":U, "&quot~;":U)     /* quote */
    p_in = REPLACE(p_in, "<":U, "&lt~;":U)        /* < */
    p_in = REPLACE(p_in, ">":U, "&gt~;":U).       /* > */
	
	p_in = REPLACE(p_in,CHR(193),"%C1").   /* take care of Capital A-Acute  */
    p_in = REPLACE(p_in,CHR(225),"%E1").   /* take care of lowercase a-acute   */
    p_in = REPLACE(p_in,CHR(201),"%C9").   /* take care of Capital E-acute     */
    p_in = REPLACE(p_in,CHR(233),"%E9").   /* take care of Lowercase e-acute   */
    p_in = REPLACE(p_in,CHR(205),"%CD").   /* take care of Capital I-acute     */
    p_in = REPLACE(p_in,CHR(237),"%ED").   /* take care of Lowercase i-acute   */
    p_in = REPLACE(p_in,CHR(209),"%D1").   /* take care of Capital N-tilde     */
    p_in = REPLACE(p_in,CHR(241),"%F1").   /* take care of Lowercase n-tilde   */
    p_in = REPLACE(p_in,CHR(211),"%D3").   /* take care of Capital O-acute     */
    p_in = REPLACE(p_in,CHR(243),"%F3").   /* take care of Lowercase o-acute   */
    p_in = REPLACE(p_in,CHR(218),"%DA").   /* take care of Capital U-acute     */
    p_in = REPLACE(p_in,CHR(250),"%FA").   /* take care of Lowercase u-acute   */
    p_in = REPLACE(p_in,CHR(220),"%DC").   /* take care of Capital U-umlaut    */
    p_in = REPLACE(p_in,CHR(252),"%FC").   /* take care of Lowercase u-umlaut  */
    p_in = REPLACE(p_in,CHR(171),"%AB").   /* take care of Left angle quotes   */
    p_in = REPLACE(p_in,CHR(187),"%BB").   /* take care of Right angle quotes  */
    p_in = REPLACE(p_in,CHR(191),"%BF").   /* take care of Inverted question mark     */
    p_in = REPLACE(p_in,CHR(161),"%A1").   /* take care of Inverted exclamation point */
    p_in = REPLACE(p_in,CHR(128),"%80").   /* take care of Euro   */
	
  RETURN p_in.
END FUNCTION. /* html-encode */
