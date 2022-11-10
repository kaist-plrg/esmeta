"use strict";
class x { set [ #x in 0 ?. [ x ] ?? 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:OptionalChain[1,0].ChainEvaluation) but got transpile-failure */
