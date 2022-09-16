var p = {
  cnt: 0,
  toString() {this.cnt++;}
}
var { [p]: _, ..._ } = {};
