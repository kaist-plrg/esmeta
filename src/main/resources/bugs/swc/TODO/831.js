"use strict";
async function x ( ) { for await ( x . x of [ 0 ] ) ; } x ( ) ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Assertion Fail]
 > Expected the async function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name", "x"] but got ["length", "name", "prototype", "x"] for Function. */
