const fs = require('fs');

exports.alloc = size => () => Buffer.alloc(size);
exports.writeUInt8 = buf => offset => byt => () => buf.writeUInt8(byt, offset);
exports.toString = buf => buf.toString('hex');
exports.writeToFileImpl = path => buf => cb => () => fs.writeFile(path, buf, cb);
