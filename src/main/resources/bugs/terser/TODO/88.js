"use strict";
( x => 0 in x ) ( ) ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RelationalExpression[6,0].Evaluation ((step 5, 6:43-73))<SYNTAX>:RelationalExpression[6,0].Evaluation) but got normal */
