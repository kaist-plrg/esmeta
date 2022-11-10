"use strict";
async function x ( x , ... [ [ ] ] ) { } x ( ) ; 

/* TAG: NEW-SM-REJECTED-PROMISE
[Exit Tag Mismatch]
 > Expected normal but got throw-error: TypeError(Exception: TypeError: undefined is not an object (evaluating '[ [ ] ]')) */
