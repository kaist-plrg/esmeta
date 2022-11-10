"use strict";
class x { static #x = super [ { x : 0 . #x } ] ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:PropertyDefinition[2,0].PropertyDefinitionEvaluation) but got throw-error: SyntaxError(unnamed:35: SyntaxError: 'super' keyword unexpected here) */
