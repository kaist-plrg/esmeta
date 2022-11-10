"use strict";
class x { static 0 = super [ { [ Symbol . toPrimitive ] : x => [ ] } > x ] ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:RelationalExpression[2,0].Evaluation) but got throw-error: ReferenceError(unnamed:23: ReferenceError: x is not defined - temporal dead zone) */
