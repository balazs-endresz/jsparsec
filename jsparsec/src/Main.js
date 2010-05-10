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
function const_(x){ return function(_){ return x } }

var call = curry(function(a, b){ return a(b) });

function isType(str, a){ return _toString.call(a) == "[object " + str + "]" }

function isArray(a){ return _toString.call(a) == "[object Array]" }

function isDefined(x){ return x !== undef }


function slice(arr, i1){
    return _slice.call(arr, i1 || 0);
}

function foldl(f, initial, arr) {
    for(var i = 0, l = arr.length; i < l; ++i) 
        initial = f(initial, arr[i]);
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

//  compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT

function compare(x, y){
    return x === y ? Ordering.EQ : 
           x <=  y ? Ordering.LT :
                     Ordering.GT;
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
    
    return [x].concat(xs);
}


function consJoin(x, xs){
    if(typeof x == "string" && typeof xs == "string")
        return x+xs;
    
    return x + xs.join("");
}


function replicate(n, x){
    for (var ret = [], i = 0; i < n; ++i)
        ret[i] = x;
    return ret;
}


function negate(a){
    return -a;
}

//returns True if a list is empty, otherwise False
function null_(a){
    return !a.length;
}


function elem(x, xs){
    return (xs.indexOf ? xs.indexOf(x) : indexOf(xs, x)) != -1;
}

function isSpace(c){
    return /^\s$/.test(c);
}
function isUpper(c){
    return c.toUpperCase() == c;
}
function isLower(c){
    return c.toLowerCase() == c;
}
function isAlphaNum(c){
    return /^\w$/.test(c);
}
function isAlpha(c){
    return /^\w$/.test(c) && /^\D$/.test(c);
}
function isDigit(c){
    return /^\d$/.test(c);
}
function isHexDigit(c){
    return /^[0-9A-Fa-f]$/.test(c);
}
function isOctDigit(c){
    return /^[0-7]$/.test(c);
}


function digitToInt(c){
    if(!isHexDigit(c))
        throw "Data.Char.digitToInt: not a digit " + c;

    return parseInt(c, 16);
}


var toInteger = parseInt; //TODO

var fromInteger = id; //TODO

var fromIntegral = id; //TODO

function range(lower, upper){
    return {
        indexOf: function(ch){ return (ch >= lower && ch <= upper) ? true : -1 },
        toString: function(){ return "range(" + lower + ", " + upper + ")" }
    };
}

function fst(tuple){
    return tuple[0];
}

function snd(tuple){
    return tuple[1];
}


//-- | 'uncurry' converts a curried function to a function on pairs.
//uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
//uncurry f p             =  f (fst p) (snd p)
function uncurry(f){
    return function(p){
        return f(p[0], p[1]);
    }
}

function namespace(){
    var o, d;
    map(function(v) {
        d = v.split(".");
        o = window[d[0]] = window[d[0]] || {};
        map(function(v2){
            o = o[v2] = o[v2] || {};
        }, d.slice(1));
    }, arguments);
    return o;
}

var JSParsec = window.JSParsec = {};

extend(JSParsec, {
    curry       : curry,
    const_      : const_,
    //"const"     : const_,
    isArray     : isArray,
    isDefined   : isDefined,
    slice       : slice,
    foldl       : foldl,
    foldr       : foldr,
    map         : map,
    filter      : filter,
    indexOf     : indexOf,
    lastIndexOf : lastIndexOf,
    zip         : zip,
    sort        : sort,
    nub         : nub,
    maybe       : maybe,
    compare     : compare,
    compose     : compose,
    compose1    : compose1,
    call        : call,
    id          : id,
    flip        : flip,
    cons        : cons,
    consJoin    : consJoin,
    replicate   : replicate,
    negate      : negate,
    null_       : null_,
    //"null"      : null_,
    elem        : elem,
    isSpace     : isSpace,
    isUpper     : isUpper,
    isLower     : isLower,
    isAlpha     : isAlpha,
    isAlphaNum  : isAlphaNum,
    isDigit     : isDigit,
    isHexDigit  : isHexDigit,
    isOctDigit  : isOctDigit,
    digitToInt  : digitToInt,
    range       : range,
    extend      : extend,
    namespace   : namespace,
    toInteger   : toInteger,
    fromInteger : fromInteger,
    fromIntegral: fromIntegral,
    fst         : fst,
    snd         : snd,
    uncurry     : uncurry
});