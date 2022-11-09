"use strict";
0 == { [ Symbol . toPrimitive ] : 0 } in [ ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:RelationalExpression[6,0].Evaluation) but got normal */
