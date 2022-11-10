"use strict";
class x { [ - { [ Symbol . toPrimitive ] : ( ) => { throw this . x [ x ] ; } } ] ; } 

/* TAG: NEW-ALL-TO-PRIMITIVE
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:MemberExpression[1,0].Evaluation) but got throw-error: TypeError(TypeError: Cannot convert undefined or null to object: undefined) */
