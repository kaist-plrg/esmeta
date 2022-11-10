"use strict";
class x { [ { [ Symbol . toPrimitive ] : class { } } ] ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,0].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,0].ClassDefinitionEvaluation:clo0) but got normal */
