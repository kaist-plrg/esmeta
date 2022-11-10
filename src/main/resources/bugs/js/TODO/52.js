"use strict";
class x { static 0 = delete super [ new 0 ] ; } 

/* TAG: NEW-DELETE-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got throw-error: ReferenceError(ReferenceError: Unsupported reference to 'super') */
