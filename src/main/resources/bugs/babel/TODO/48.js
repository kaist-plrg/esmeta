"use strict";
let x = async x => await 0 ?. x ( ) ; x ( ) ; 

/* TAG: NEW-NAME
[Assertion Fail]
 > Expected the async function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
descriptor value of "name" should be "x" but "bound x" */
