"use strict";
const { x } = 0 ; for ( x of [ 0 ] ) ; 

/* TAG: NEW-SWC-ASSIGN-CONST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:ForInOfStatement[3,0].ForInOfLoopEvaluation) but got transpile-failure */
