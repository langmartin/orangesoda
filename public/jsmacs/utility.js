function whitespacep (ch) {
  return (ch.search(/^[ \r\n\t\v]$/) >= 0);
}

function numericp (ch) {
  return (ch.search(/^\d$/) >= 0);
}

function isArray (obj) {
  return toString.call(obj) === "[object Array]";
}

function isArrayLike (obj) {
  return obj && obj.length;
}

// function isDate (obj) {
//   return obj && obj.getMonth();
// }

// function foreach (proc, obj) {
//   if (isArray(obj)) {
//     for (var ii = 0; ii < obj.length; ii++) {
//       if (proc(ii, obj[ii]) === false) break;
//     }
//   }
//   else {
//     for (var key in obj) {
//       if (proc(key, obj[key]) === false) break;
//     }
//   }
// }

// function foldl (proc, nil, obj) {
//   foreach(
//     function (key, val) {
//       return nil = proc(key, val, nil);
//     },
//     obj
//   );
//   return nil;
// }

// function map (proc, obj) {
//   return foldl(
//     function (ii, val, lst) {
//       lst.push(proc(ii, val));
//     },
//     [],
//     obj
//   );
// }
