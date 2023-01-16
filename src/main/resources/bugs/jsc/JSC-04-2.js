 //  var ID = { * [ ANY ] ( PARAM? ) { BODY? } } ; 
 //  class ID { * [ ANY ] ( PARAM? ) { BODY? } } ; 
 ///
class x { * [ 0 ] ( ) { } } 
var x = { * [ 1 ** 0 ** 0 ] ( ) { } } ; 
var x = { * [ 1 ** 0 ** ~ 0 ] ( ) { } } ; 
var x = { * [ false ] ( ) { } } ; 
var x = { * [ x ** 0 ** 0 ] ( ) { } } ; 
var x = { * [ x ** 0 ** ~ 0 ] ( ) { } } ; 
var x = { * [ x ] ( ) { } } ; 
var x = { * [ { } ** 0 ** 0 ] ( ) { } } ; 
