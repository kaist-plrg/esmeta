"use strict";
class x { static 0 = super [ { [ Symbol . toPrimitive ] : { } } > x ] ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:RelationalExpression[2,0].Evaluation) but got throw-error: ReferenceError(unnamed:19: ReferenceError: x is not defined - temporal dead zone) */
