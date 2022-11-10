"use strict";
class x { static { var { x } = super [ 0 ] ; } } 

/* TAG: NEW-STATIC-SAME-NAME-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got transpile-failure */
