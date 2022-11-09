"use strict";
var [ ... [ x , ... { 0 : { ... x } , } ] ] = '' ; 

/* TAG: NEW-DSG-OBJ-REST-NULL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization)) but got normal */
