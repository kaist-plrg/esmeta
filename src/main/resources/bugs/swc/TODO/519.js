"use strict";
[ , ] = { [ Symbol . iterator ] : async function * ( ) { ; for await ( let x of [ 0 ] [ 0 , 0 ] ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */
