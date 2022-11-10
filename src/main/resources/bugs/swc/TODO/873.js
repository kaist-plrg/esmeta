"use strict";
async function x ( ) { await { get 0 ( ) { throw super [ super [ 0 ] %= 0 ] ??= 0 ; } , } [ 0 ] ; } x ( ) ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected the async function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function. */
