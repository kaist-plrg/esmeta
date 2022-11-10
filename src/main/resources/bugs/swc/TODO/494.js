"use strict";
class x { static { var { x } = super [ 0 ] ; } } 

/* TAG: NEW-SWC-STATIC-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got throw-error: SyntaxError(unnamed:9: SyntaxError: 'super' keyword unexpected here) */
