"use strict";
class x { static { if ( 0 ?. [ 0 >= ++ x >> 0 < 0 ] . x ** 0 ) var x ; } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:ExponentiationExpression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a var variable that shadows a let/const/class variable: 'x'.) */
