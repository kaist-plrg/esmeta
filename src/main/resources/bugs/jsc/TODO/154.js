"use strict";
async function x ( ... [ [ ] ] ) { } x ( 0 ) ; 

/* TAG: NEW-SM-REJECTED-PROMISE
[Exit Tag Mismatch]
 > Expected normal but got throw-error: TypeError(Exception: TypeError: undefined is not a function (near '...[ ]...')) */
