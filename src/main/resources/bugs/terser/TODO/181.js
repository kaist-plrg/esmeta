"use strict";
for ( var x of function * x ( ) { yield * x ; let x ; } ( ) ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:YieldExpression[2,0].Evaluation) but got throw-error: TypeError(unnamed:3: TypeError: undefined is not iterable (cannot read property Symbol(Symbol.iterator))) */
