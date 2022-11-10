"use strict";
async function x ( ) { } x instanceof x ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(OrdinaryHasInstance ((step 5, 8:40-70))<BUILTIN>:INTRINSICS.Function.prototype[@@hasInstance]) but got normal */
