"use strict";
class x extends null { static { throw x &&= 0 ; } } 

/* TAG: NEW-GRL-LOGICAL-ASSIGN-CONST
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[6,0].Evaluation)) but got throw-value: "0
	at <js> :initializer(<eval_script>:3:178-192)
	at <js> :program(<eval_script>:3:146-196)" */
