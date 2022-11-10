"use strict";
class x { [ { [ 0 ] : x = class extends 0 { } } ] ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,2].ClassDefinitionEvaluation ((step 8.g, 27:62-92))<SYNTAX>:ClassTail[0,2].ClassDefinitionEvaluation) but got throw-error: ReferenceError(Exception: ReferenceError: Cannot access uninitialized variable.) */
