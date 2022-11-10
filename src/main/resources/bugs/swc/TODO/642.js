"use strict";
async function x ( ) { ; for await ( let x of { [ Symbol . iterator ] : x => await } ) ; } x ( ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected normal but got transpile-failure */
