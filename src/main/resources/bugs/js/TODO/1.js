"use strict";
class x extends null { static 0 = delete new super [ 0 ] ( ) ; } 

/* TAG: NEW-GRL-DELETE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:MemberExpression[6,0].Evaluation) but got normal */
