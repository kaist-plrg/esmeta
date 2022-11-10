"use strict";
Promise . allSettled ( { [ Symbol . iterator ] : x => await } ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected normal but got transpile-failure */
