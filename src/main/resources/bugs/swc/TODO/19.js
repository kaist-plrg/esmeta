"use strict";
[ , ] = { [ Symbol . iterator ] : function * ( ) { throw yield ; } } ; 

/* TAG: NEW-DSG-SPARSE-ARR-SYMBOL-ITERATOR
[Exit Tag Mismatch]
 > Expected normal but got throw-value: "unnamed:43: undefined
            if (_d) throw _e;
                    ^
" */
