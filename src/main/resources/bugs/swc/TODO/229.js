"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( x of { [ Symbol . asyncIterator ] : function * ( ) { 0 ? 0 : yield ; } } ) for ( var x of x ) ; } } ; 

/* TAG: NEW-TIMEOUT
[Exit Tag Mismatch]
 > Expected normal but got timeout */
