// -------------------------------------------------
// Main
// -------------------------------------------------

var undef,
	_toString = {}.toString,
	_slice    = [].slice;

function curry(fn){
  function ret(){
    var args = slice(arguments);
    return args.length >= (fn._length === undefined ? fn.length : fn._length) ? fn.apply(null, args) : 
      function(){
        return ret.apply( null, args.concat(slice(arguments)) );
      };
  }
  return ret;
}

var id = function(x){ return x };

//var const_ = curry(function(x, _){ return x });
function const_(x){ return function(_){ return x } };

var call = curry(function(a, b){ return a(b) });

function isType(str, a){ return _toString.call(a) == "[object " + str + "]" }

function isArray(a){ return _toString.call(a) == "[object Array]" }

function isDefined(x){ return x !== undef }


function slice(arr, i1){
	return _slice.call(arr, i1 || 0);
}

function foldl(f, initial, arr) {
    for(var i = 0, l = arr.length; i < l; ++i) 
		initial = f(arr[i], initial);
    return initial;
}

function foldr(f, initial, arr) {
    for(var l = arr.length - 1; l > -1 ; --l) 
		initial = f(arr[l], initial);
    return initial;
}

function map(f, arr){
	var res = [], i = 0, l = arr.length;
    for (; i < l; ++i)
        res[i] = f(arr[i], i);
    return res;
}


function filter(arr, f) {
	var res = [], i = 0, l = arr.length;
	for(; i < l; ++i)
		if(f(arr[i]))
			res.push(arr[i]);
	return res;
}

function indexOf(arr, value) {
    var length = arr.length;   
  	if (!length)
		return -1;
	
    for (var from = 0; from < length; from++)  
      if (arr[from] === value)  
        return from;  
   
    return -1;  
}

function lastIndexOf(arr, value) {
	var length = arr.length;
	if (!length)
		return -1;

    for (var from = length - 1; from > -1; --from)
      if (arr[from] === value)
        return from;

    return -1;
}



function extend(a, b){
	for(var key in b)
		a[key] = b[key];
	return a;
}

function compose(fst, snd){
	return function(){
		return fst(snd.apply(null, arguments));
	};
}

//this is the same as (.) in Haskell:
//the inner function receives only the first argument
function compose1(fst, snd){
	return function(a, b, c){ 
		var args = slice(arguments, 1);
		args.unshift(snd(a));
		return fst.apply(null, args);
	};
}

function flip(fn) {
    return function(a, b){ return fn(b, a) };
}

function cons(x, xs){
	if(typeof x == "string" && typeof xs == "string")
		return x+xs;

	xs.unshift(x);

	return xs;
}

function replicate(n, x){
	for (var ret = [], i = 0; i < n; ++i)
		ret[i] = x;
	return ret;
}

function elem(x, xs){
	return (xs.indexOf ? xs.indexOf(x) : indexOf(xs, x)) != -1
}

function isSpace(c){
	return /\s/.test(c);
}
function isUpper(c){
	return c.toUpperCase() == c;
}
function isLower(c){
	return c.toLowerCase() == c;
}
function isAlphaNum(c){
	return /\w/.test(c);
}
function isAlpha(c){
	return /\w/.test(c) && /\D/.test(c);
}
function isDigit(c){
	return /\d/.test(c);
}
function isHexDigit(c){
	return /[0-9A-Fa-f]/.test(c);
}
function isOctDigit(c){
	return /[0-7]/.test(c);
}

