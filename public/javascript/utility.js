function isObject (obj) {
  return (typeof obj == "object");
}

function eachObj (obj, proc) {
  for (var key in obj) {
    if (proc(key, obj[key]) === false) break;
  }
}

function eachArray (obj, proc, start, stop, step) {
  start = start || 0;
  stop = stop || obj.length;
  step = step || 1;
  for (var ii = start; ii < stop; ii = ii + step) {
    if (proc(ii, obj[ii]) === false) break;
  }
}

function eachString (obj, proc, start, stop, step) {
  start = start || 0;
  stop = stop || obj.length;
  step = step || 1;
  for (var ii = start; ii < stop; ii = ii + step) {
    if (proc(ii, obj.charAt(ii)) === false) break;
  }
}

function each (obj, proc) {
  if (isArray(obj)) eachArray(obj, proc);
  else if (isObject(obj)) eachObj(obj, proc);
  else if (typeof obj == "string") eachObj(obj, proc);
  else throw new TypeError("Not an object");
}
