"use strict";
class x { static 0 = super [ { [ Symbol . toPrimitive ] : x } > x ] ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,1].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,1].ClassDefinitionEvaluation:clo0) but got throw-error: ReferenceError(unnamed:19: ReferenceError: x is not defined - temporal dead zone) */
