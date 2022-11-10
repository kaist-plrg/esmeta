"use strict";
for ( var { } of { [ Symbol . iterator ] : async function * ( ) { try { while ( await this ) ; } finally { } } } ) ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got timeout */
