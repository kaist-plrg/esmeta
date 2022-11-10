"use strict";
class x extends await { ; } class await { } 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ClassTail[0,3].ClassDefinitionEvaluation) but got transpile-failure */
