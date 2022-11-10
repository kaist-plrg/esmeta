"use strict";
async function * x ( ... x ) { } x ( 0 ) ; 

/* TAG: NEW-SM-PROP-ORDER
[Assertion Fail]
 > Expected ["length", "name", "prototype"] but got ["prototype", "length", "name"] for AsyncGeneratorFunction. */
