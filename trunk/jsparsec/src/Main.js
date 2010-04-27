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

//-- | 'zip' takes two lists and returns a list of corresponding pairs.
//-- If one input list is short, excess elements of the longer list are
//-- discarded.
//zip :: [a] -> [b] -> [(a,b)]
//zip (a:as) (b:bs) = (a,b) : zip as bs
//zip _      _      = []
function zip(arr1, arr2){
	var res = [], i = 0, l = Math.min(arr1.length, arr2.length);
    for (; i < l; ++i)
        res[i] = [arr1[i], arr2[i]];
    return res;
}

function sort(arr) {
	var type = typeof arr;

	if(type == "object")
		return arr.sort();

	if(type == "string")
		return slice(arr).sort().join("");
}

//-- | The 'nub' function removes duplicate elements from a list.
//-- In particular, it keeps only the first occurrence of each element.
//-- (The name 'nub' means \`essence\'.)
//-- It is a special case of 'nubBy', which allows the programmer to supply
//-- their own equality test.
//nub                     :: (Eq a) => [a] -> [a]
//#ifdef USE_REPORT_PRELUDE
//nub                     =  nubBy (==)
//#else
//-- stolen from HBC
//nub l                   = nub' l []             -- '
//  where
//    nub' [] _           = []                    -- '
//    nub' (x:xs) ls                              -- '
//        | x `elem` ls   = nub' xs ls            -- '
//        | otherwise     = x : nub' xs (x:ls)    -- '
//#endif

function nub(arr, ls){
	ls = ls === undef ? [] : ls;
	
	var x  = arr[0],
		xs = slice(arr, 1);
	
	return !arr.length ? [] :
			elem(x, ls) ? nub(xs, ls) : 
			cons(x, nub(xs, cons(x,ls)) );
}


//-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
//-- value.  If the 'Maybe' value is 'Nothing', the function returns the
//-- default value.  Otherwise, it applies the function to the value inside
//-- the 'Just' and returns the result.
//maybe :: b -> (a -> b) -> Maybe a -> b
//maybe n _ Nothing  = n
//maybe _ f (Just x) = f x

function maybe(n, f, m){
	if(m.Nothing)
		return n;
	if(m.Just)
		return f(m[0]);
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


function negate(a){
	return -a;
};

//returns True if a list is empty, otherwise False
function null_(a){
	return !a.length;
};


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


function digitToInt(c){
	if(c.length != 1)
		throw "digitToInt accepts only a single character";

	if(!isHexDigit(c))
		throw "Char.digitToInt: not a digit " + c;

	return parseInt(c, 16);
}