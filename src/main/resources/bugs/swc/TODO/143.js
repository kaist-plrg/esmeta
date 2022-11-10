"use strict";
class x { [ { [ Symbol . toPrimitive ] : x => [ ] } ] ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:ComputedPropertyName[0,0].Evaluation) but got normal */
