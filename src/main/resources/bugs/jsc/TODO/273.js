"use strict";
class x { static { var x = super [ 0 , super [ { [ Symbol . toPrimitive ] : function * ( x ) { } } ] ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:SuperProperty[0,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
