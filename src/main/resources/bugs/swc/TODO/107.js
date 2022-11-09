"use strict";
class x { static { super [ new 0 ] ; } } 

/* TAG: NEW-SWC-STATIC-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation)) but got throw-error: SyntaxError(unnamed:9: SyntaxError: 'super' keyword unexpected here) */
