"use strict";
class x { static { var x = super [ 0 , super [ new . target . x , 0 ] ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:Expression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
