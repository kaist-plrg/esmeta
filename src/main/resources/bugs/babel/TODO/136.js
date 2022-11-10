"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * x ( ) ; } } ; async function * x ( ) { } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected the async generator function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected [object Object] but got the object prototype.
Expected [] but got ["constructor"] for Object. */
