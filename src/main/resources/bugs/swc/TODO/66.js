"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { return yield * [ 0 , x , ] ; } } ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ElementList[2,0].ArrayAccumulation) but got normal */
