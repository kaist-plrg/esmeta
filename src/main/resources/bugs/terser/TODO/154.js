"use strict";
[ { [ Symbol . toPrimitive ] : async function ( x ) { } } ] < [ 0 , 0 ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<BUILTIN>:INTRINSICS.Array.prototype.join) but got normal */
