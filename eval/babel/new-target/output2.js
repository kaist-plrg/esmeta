var f = function f() {
  this instanceof f ? this.constructor : void 0;
}

f();
new f();
