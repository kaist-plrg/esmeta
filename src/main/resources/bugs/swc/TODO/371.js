"use strict";
for ( var { } of function * x ( ) { yield ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got normal */
