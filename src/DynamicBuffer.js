const fs = require('fs');

exports.setImpl = function(array, offset, x) {
  array[offset] = x;
};

exports.setAllImpl = function(array, offset, xs) {
  array.set(xs, offset);
};

exports.allocate = function(size) {
  return new Uint8Array(size);
};

exports.subarray = function(array, offset, length) {
  return array.subarray(offset, length);
};

exports.whenE = function(pred, action) {
  if (pred) {
    action();
  }
};

exports.writeToFileImpl = function(path, buf, cb) {
  fs.writeFile(path, buf, cb);
};

exports.printImpl = function(array) {
  return function() {
    let numbers = [];
    array.forEach(b => numbers.push("0x" + b.toString(16).toUpperCase()));
    console.log("[" + numbers.join(", ") + "]");
  };
};
