"use strict";
for ( var { } of { [ Symbol . iterator ] : async function * x ( ) { return yield * yield * [ ! 0 , ] [ 0 ] /= 0 ; } } ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RequireObjectCoercible<SYNTAX>:BindingPattern[0,0].BindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Unexpected token '*'. Expected a ';' following a return statement.) */
