"use strict";
let [ ] = ~ function * x ( [ ] = 0 , x , [ ] ) { } ( ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:BindingPattern[1,0].BindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with default parameter values.) */
