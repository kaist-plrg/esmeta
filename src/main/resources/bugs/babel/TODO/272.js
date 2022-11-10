"use strict";
function * x ( [ ] ) { } x ( ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:BindingPattern[1,0].BindingInitialization) but got normal */
