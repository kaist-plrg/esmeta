"use strict";
0 > { [ Symbol . toPrimitive ] : class { } } / 0 ; 

/* TAG: NEW-SWC-OBJ-DIV-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,0].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,0].ClassDefinitionEvaluation:clo0) but got transpile-failure */
