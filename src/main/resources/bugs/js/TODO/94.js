"use strict";
class x { static 0 = delete super [ this [ 0 ] [ 0 ] ] ; } 

/* TAG: NEW-DELETE-SUPER
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:SuperProperty[0,0].Evaluation) but got throw-error: ReferenceError(ReferenceError: Unsupported reference to 'super') */
