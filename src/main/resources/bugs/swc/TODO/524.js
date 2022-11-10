"use strict";
class x extends null { static [ x ] = 0 ; } 

/* TAG: NEW-DSG-CLASS-IN-COMPUTED
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ComputedPropertyName[0,0].Evaluation) but got throw-error: Error(unnamed:16: Error: Class "x" cannot be referenced in computed property keys.) */
