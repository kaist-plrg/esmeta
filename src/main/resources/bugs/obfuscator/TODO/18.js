"use strict";
var [ x = await ] = '' ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:SingleNameBinding[0,1].IteratorBindingInitialization) but got transpile-failure */
