"use strict";
class x { static { function * x ( ) { } function x ( ) { } } } 

/* TAG: NEW-JSC-STATIC-SAME-NAME
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(Exception: SyntaxError: Cannot declare a function that shadows a let/const/class/function variable 'x' in strict mode.) */
