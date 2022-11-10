"use strict";
class x { [ { [ Symbol . toPrimitive ] : { } } ] ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:ComputedPropertyName[0,0].Evaluation) but got normal */
