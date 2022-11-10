"use strict";
function * x ( ) { } var [ , ] = { [ Symbol . iterator ] : async function * ( ) { for await ( { x = 0 , } of [ { x } ] ) ; } } ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected the generator function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected [object Object] but got [object Object]. */
