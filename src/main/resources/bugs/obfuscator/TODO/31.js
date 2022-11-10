"use strict";
( x => class { } ( ) ) ( ) ?. x ; 

/* TAG: NEW-OBF-OPT-CALL-CHAIN-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,0].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,0].ClassDefinitionEvaluation:clo0) but got transpile-failure */
