"use strict";
0 > { [ Symbol . toPrimitive ] : { } } / 0 ; 

/* TAG: NEW-SWC-OBJ-DIV-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:MultiplicativeExpression[1,0].Evaluation) but got transpile-failure */
