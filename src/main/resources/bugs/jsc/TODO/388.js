"use strict";
class x { static { var x = super [ 0 , super [ { [ Symbol . toPrimitive ] : [ 0 ] } ] ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:SuperProperty[0,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
