"use strict";
function * x ( ) { } var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = 0 , } of [ { x } ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */
