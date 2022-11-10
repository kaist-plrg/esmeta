"use strict";
let x ; var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = 0 } of [ 0 ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */
