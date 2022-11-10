"use strict";
( x => 0 ) ( ) . x -- ; 

/* TAG: NEW-SWC-ASYNC-POSTFIX-DECR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PutValue ((step 5.d, 15:72-102))<SYNTAX>:UpdateExpression[2,0].Evaluation) but got throw-error: SyntaxError(unnamed:4: SyntaxError: Function statements require a function name) */
