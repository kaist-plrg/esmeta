"use strict";
class x { [ x ] ( ) { } } 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ComputedPropertyName[0,0].Evaluation) but got normal */
