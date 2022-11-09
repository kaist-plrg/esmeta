"use strict";
+ { [ Symbol . toPrimitive ] : x => function ( ) { } ( ) . x ?. x ** 0 } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:OptionalExpression[1,0].Evaluation) but got normal */
