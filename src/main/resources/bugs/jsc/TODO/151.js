"use strict";
class x { static { var x = super [ 0 , super [ { [ Symbol . toPrimitive ] : class { } } ] ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,0].ClassDefinitionEvaluation:clo0 ((step 14.a.ii, 40:45-75))<BUILTIN-CLO>:ClassTail[0,0].ClassDefinitionEvaluation:clo0) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
