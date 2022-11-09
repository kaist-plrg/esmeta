"use strict";
var [ ... [ x , ... { 0 : { ... x } , } ] ] = '' ; 

/* TAG: NEW-SWC-NESTED-REST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization)) but got transpile-failure */
