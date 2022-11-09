"use strict";
class x { static { super [ { [ Symbol . toPrimitive ] : 0 } ] ; } } 

/* TAG: NEW-SWC-STATIC-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(GetMethod ((step 3, 4:46-76))<SYNTAX>:SuperProperty[0,0].Evaluation)) but got throw-error: SyntaxError(unnamed:22: SyntaxError: 'super' keyword unexpected here) */
