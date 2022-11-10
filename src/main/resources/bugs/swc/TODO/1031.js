"use strict";
class x { static #x = new super . x ( ) ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:MemberExpression[6,0].Evaluation) but got throw-error: SyntaxError(unnamed:14: SyntaxError: 'super' keyword unexpected here) */
