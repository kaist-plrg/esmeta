"use strict";
class x { [ x > 0 ] ; } 

/* TAG: NEW-DSG-CLASS-IN-COMPUTED
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:RelationalExpression[2,0].Evaluation) but got throw-error: Error(unnamed:10: Error: Class "x" cannot be referenced in computed property keys.) */
