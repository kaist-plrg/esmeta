"use strict";
let x ; ! class extends x { ; } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ClassTail[0,3].ClassDefinitionEvaluation ((step 8.g, 27:62-92))<SYNTAX>:ClassTail[0,3].ClassDefinitionEvaluation) but got normal */
