"use strict";
let x = x => x * 0 ; x ( ) ; 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected [object Function] does not have [[Construct]] but does.
descriptor value of "name" should be "x" but "bound x" */
