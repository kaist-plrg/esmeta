"use strict";
let x ; ( delete { set : async * function ( x ) { } } ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:MultiplicativeExpression[1,0].Evaluation) but got normal */
