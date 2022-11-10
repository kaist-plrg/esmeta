"use strict";
class x { static { var x = super [ 0 , super [ { [ Symbol . toPrimitive ] : ( ) => { throw 0 ; } } ] ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-value: 0.0f but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
