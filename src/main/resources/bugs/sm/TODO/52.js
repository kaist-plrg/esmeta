"use strict";
async function x ( [ ] ) { } x ( ) ; 

/* TAG: NEW-SM-REJECTED-PROMISE
[Exit Tag Mismatch]
 > Expected normal but got throw-error: TypeError(-e:3:18 TypeError: can't access property Symbol.iterator, (destructured parameter) is undefined) */
