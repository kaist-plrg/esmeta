"use strict";
let x ; [ { 0 : x = function * ( ) { } } = 0 ] = '' ; 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected the generator function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected [object Object] but got [object Object].
descriptor value of "name" should be "x" but "_callee" */
