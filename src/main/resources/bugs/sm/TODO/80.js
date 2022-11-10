"use strict";
async function * x ( ) { for await ( var x of 0 ) ; } x ( ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for AsyncGeneratorFunction. */
