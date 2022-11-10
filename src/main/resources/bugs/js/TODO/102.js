"use strict";
class x { static 0 = delete new 0 ; } 

/* TAG: NEW-GRL-DELETE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got normal */
