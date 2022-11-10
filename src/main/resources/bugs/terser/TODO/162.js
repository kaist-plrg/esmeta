"use strict";
[ ... { [ Symbol . iterator ] : function * ( ... [ ] ) { let [ ] = 0 ; } } ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:BindingPattern[1,0].BindingInitialization) but got normal */
