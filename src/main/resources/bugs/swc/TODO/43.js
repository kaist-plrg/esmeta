"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * `` [ x , x ] ; } } ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:Expression[1,0].Evaluation) but got throw-error: TypeError(unnamed:157: TypeError: undefined is not iterable (cannot read property Symbol(Symbol.iterator))) */
