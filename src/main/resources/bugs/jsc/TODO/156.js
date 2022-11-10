"use strict";
class x { static { var x = super [ new 0 , 0 ] ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
