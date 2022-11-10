"use strict";
let [ ] = { [ [ 0 ** ~ 0 , ] [ 0 ] /= ! 0 ] : 0 } ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:BindingPattern[1,0].BindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '/=' must be a reference.) */
