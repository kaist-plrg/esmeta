"use strict";
1 < { [ Symbol . toPrimitive ] : class { } } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,0].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,0].ClassDefinitionEvaluation:clo0) but got normal */
