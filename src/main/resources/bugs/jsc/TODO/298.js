"use strict";
let [ ] = async function * x ( { x , } = x ) { } ( ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:BindingElement[1,1].IteratorBindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with default parameter values.) */
