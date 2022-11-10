"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { ; for await ( var { } of x ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */
