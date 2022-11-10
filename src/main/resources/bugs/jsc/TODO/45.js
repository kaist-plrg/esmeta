"use strict";
var x = { ... this ?. x ?. ( ) . x } ; function x ( ) { } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:PropertyDefinition[4,0].PropertyDefinitionEvaluation) but got normal */
