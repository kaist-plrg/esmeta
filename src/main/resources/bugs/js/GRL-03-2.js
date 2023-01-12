 //  const ID1 = ANY; ID1 (&&= / ||=) ANY;
 ///
const x = 0 ; x ||= 0 ; 
const x = 1 ; x : switch ( x &&= 0 ) { } 
const x = 1n ; x : switch ( x &&= 0 ) { } 
const { ... x } = 0 ; throw x &&= 0 ;
