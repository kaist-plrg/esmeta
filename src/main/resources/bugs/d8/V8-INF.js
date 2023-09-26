for ( let [ , ] of [ { [ Symbol . iterator ] : async function * ( ) { x : for ( yield * '' ; ; 0 ) ; } } ] ) throw 0 ; 
for ( let [ , ] of { [ Symbol . iterator ] : async function * ( ) { for ( ; ; yield * `` ) ; } } ) ; 
for ( let [ , ] of { [ Symbol . iterator ] : async function * ( ) { for ( ; ; yield * { [ Symbol . asyncIterator ] : function * ( ) { } } ) ; } } ) ; 
for ( var { } of { [ Symbol . iterator ] : async function * ( ) { try { while ( await this ) ; } finally { } } } ) ; 
