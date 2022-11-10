"use strict";
async function * x ( ) { yield 0 ; } [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * x ( ) ; } } ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for AsyncGeneratorFunction. */
