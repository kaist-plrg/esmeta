"use strict";
class x { async * [ x ] ( ) { } } 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(Some(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ComputedPropertyName[0,0].Evaluation)) but got normal */
