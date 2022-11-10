"use strict";
class x { static #x = super . x . #x ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:Initializer[0,0].EvaluateBody) but got throw-error: SyntaxError(unnamed:35: SyntaxError: 'super' keyword unexpected here) */
