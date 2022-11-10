"use strict";
( x => x in 0 ) ( ) ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RelationalExpression[6,0].Evaluation ((step 5, 6:43-73))<SYNTAX>:RelationalExpression[6,0].Evaluation) but got normal */
