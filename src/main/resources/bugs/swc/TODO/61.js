"use strict";
async function x ( ) { ; for await ( var x of 0 . x ) ; } x ( ) ; 

/* TAG: NEW-SWC-CHAIN-ASSIGN-OBJ
[Assertion Fail]
 > Expected the async function prototype but got [object Function].
Expected [object Function] does not have [[Construct]] but does.
Expected ["length", "name"] but got ["length", "name", "prototype"] for Function. */
