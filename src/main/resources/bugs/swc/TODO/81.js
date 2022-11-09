"use strict";
class x extends null { [ x ??= 0 ] ; } 

/* TAG: NEW-DSG-CLASS-IN-COMPUTED
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(Some(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[8,0].Evaluation)) but got throw-error: Error(unnamed:16: Error: Class "x" cannot be referenced in computed property keys.) */
