"use strict";
async function x ( ) { ; for await ( x . x of [ 0 ] ) ; } x ( ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "x"] but got ["x", "length", "name"] for AsyncFunction. */
