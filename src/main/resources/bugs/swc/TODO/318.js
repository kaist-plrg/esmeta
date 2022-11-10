"use strict";
for ( let { } of { [ Symbol . iterator ] : async function * ( ) { } } ) ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got timeout */
