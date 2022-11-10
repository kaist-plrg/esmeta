"use strict";
class x extends new function ( ) { x ( ) ; } { } 

/* TAG: NEW-GRL-EXTENDS-ITSELF
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: TypeError(TypeError: x is not a function) */
