"use strict";
~ [ , { [ Symbol . toPrimitive ] : x => [ ] } ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<BUILTIN>:INTRINSICS.Array.prototype.join) but got normal */
