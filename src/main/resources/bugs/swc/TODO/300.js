"use strict";
async function x ( ) { ; for await ( var x of { [ Symbol . asyncIterator ] : x => await } ) ; } x ( ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected normal but got transpile-failure */
