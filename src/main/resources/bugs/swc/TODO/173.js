"use strict";
( x => `` . x ) ( ) . x -- ; 

/* TAG: NEW-SWC-ASYNC-POSTFIX-DECR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:UpdateExpression[2,0].Evaluation) but got throw-error: SyntaxError(unnamed:4: SyntaxError: Function statements require a function name) */
