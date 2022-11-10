"use strict";
new class { 0 = super [ delete super [ { [ Symbol . toPrimitive ] : async function * ( ) { } } ] ] ; } ; 

/* TAG: NEW-DELETE-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:SuperProperty[0,0].Evaluation) but got throw-error: ReferenceError(ReferenceError: Unsupported reference to 'super') */
