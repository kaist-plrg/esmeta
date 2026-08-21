// spectec/test/harness/testharness.js assumes a browser/worker global
// (`self`). Load this before testharness.js so it runs standalone in a bare
// JS shell (like WJI).
var self = globalThis;
