"use strict";
new class { 0 = super [ delete super [ { [ Symbol . toPrimitive ] : { } } ] ] ; } ; 

/* TAG: NEW-DELETE-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:SuperProperty[0,0].Evaluation) but got throw-error: ReferenceError(ReferenceError: Unsupported reference to 'super') */
