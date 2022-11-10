"use strict";
! async function * x ( x = class extends 0 { } ) { } ( ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,2].ClassDefinitionEvaluation ((step 8.g, 27:62-92))<SYNTAX>:ClassTail[0,2].ClassDefinitionEvaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with default parameter values.) */
