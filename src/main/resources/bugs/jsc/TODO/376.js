"use strict";
new class extends function x ( x = await , ... [ ] ) { } { } ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:SingleNameBinding[0,1].IteratorBindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with default parameter values.) */
