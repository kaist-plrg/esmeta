"use strict";
var [ , ] = { [ Symbol . iterator ] : async function * ( ) { yield * x ( ) ; } } ; async function * x ( ) { } 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for AsyncGeneratorFunction. */
