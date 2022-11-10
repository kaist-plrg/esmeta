"use strict";
class x { static { if ( 0 ?. [ 0 >= ++ x . x >> 0 ] ** 0 ) var x ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:UpdateExpression[3,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
