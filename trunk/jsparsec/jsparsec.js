;(function(){
/** @license
 * JSParsec - A parser combinator library for JavaScript
 * 
 * Version: 1.0.1
 * 
 * http://code.google.com/p/jsparsec/
 * 
 * Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 * Dual licensed under the MIT and GPL licenses.
 * 
 *
 * The initial implementation of some combinators and the memoization is derived from:
 * http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html
 *
 * Most functions should behave like their counterparts in Parsec:
 * http://www.haskell.org/haskellwiki/Parsec
 * 
 */

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

//lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
function lookup(key, arr){
    var length = arr.length;   
    if (!length)
        return Maybe.Nothing;
    
    for (var i = 0; i < length; ++i)  
      if (arr[i][0] === key)
        return Maybe.Just(arr[i][1]);  
   
    return Maybe.Nothing;
}

function readHex(str){
    return parseInt(str.join ? str.join("") : str, 16);
}

function readOct(str){
    return parseInt(str.join ? str.join("") : str, 8);
}

var chr = String.fromCharCode;

var round = Math.round;


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
    uncurry     : uncurry,
    lookup      : lookup,
    readHex     : readHex,
    readOct     : readOct,
    chr         : chr,
    round       : round
});

// -------------------------------------------------
// Algebraic Data Types
// -------------------------------------------------


function Record(){}
var record = new Record();


//this is used for simulating the record syntax
//but it's not guaranteed to work in every javascript environment
//beacause it assumes that the keys of objects are iterated in the order they were defined
//but that is not part of the ECMAScript standard
function getNthKey(obj, nth) {
    if(!/^[0-9]$/.test(nth))
        return nth;
    if(typeof obj != "object")
        return nth;
    var i = 0;
    for (var key in obj){
        if (obj.hasOwnProperty(key) && i == nth)  
            return key;  
        i++;
    }
    return -1;  
}


function ADT(){}

function adtToString(type){
    return function(){
        var acc=[], rec = this._recordset;
        if(rec && !isArray(rec)){
            for(var name in rec){
                var item = (type ? (rec[name].name || rec[name]) : this[name]);
                if(!type && (item instanceof Function))
                    item = (item.constructor != Function) ?
                                        item.constructor.name :
                                        "Function(" + item.name + ")";
                acc.push(name + " :: " + item );
            }
            var indent = replicate(this._dataConstructor.length + 2, " ").join("");
            acc = "{" + acc.join("\n" + indent + ",") + "\n" + indent +"}";
        }else{
            for(var i = 0; i in this; i++)
            acc.push(type ? (rec[i].name || rec[i]) : this[i]);
            acc = acc.join(" ");
        }
        return "(" + this._dataConstructor + (acc ? " " : "") + acc + ")";
    };
}

ADT.prototype.toString = adtToString();

ADT.prototype.dataConstructorToString = adtToString(true);


function data(type, constr){
    if(type.constructors)
        throw "Type constructor has been already defined: '" + type.name + "'";
    type.constructors = constr;

    type.prototype = new ADT();

    for(var i = 0, l = constr.length; i < l; ++i){
        var single = typeof constr[i] != "object",
            name =  single  ? constr[i] : constr[i][0];
        if(name in type)
            throw "The name of the data constructor (" + name + ") is invalid!";

        type[name] = single ? value(name)() : value(name, slice(constr[i], 1));
        if(!single)
            type[name]._length = slice(constr[i], 1).length;
    }

    function value(constr, fields){
        var recordDef = fields && typeof fields[0] == "object";
        function create(_isrecord, rec){
            var isrecord = (_isrecord instanceof Record),
                args = isrecord ? rec : slice(arguments),
                that = new type(),
                i = 0;
            
            that.constructor = type;
            that._recordset = (recordDef && fields[0]) || fields;
            that._dataConstructor = constr;

            that.update = function(newRecords){
                var obj = {};
                for(var n in fields[0]){
                    obj[n] = this[n];
                    if(n in newRecords)
                        obj[n] = newRecords[n];
                }
                return create(record, obj);
            };

            that[constr] = true;

            if(args !== undef)
                for(var name in args)
                    if(args.hasOwnProperty(name) && name != constr){

                        if(isrecord && fields && recordDef)
                            if( !(name in fields[0]))
                                throw "The accessor '" + name + "' is not defined for the data constructor '" + constr + "'";

                        var recName = getNthKey(fields[0], name);
                        var arg = (args[i] !== undefined) ? args[i] : args[recName];

                        var check = recordDef ? fields[0][recName] : fields[i];
                        if(check.name && !((check == arg.constructor) || (arg instanceof check) ))
                            throw "Type error: expecting '" + check.name + "' instead of '" + arg.constructor.name +
                                    "' in the argument '" + (recName || i) + "' of the data constructor '" +
                                    constr + "' of type '" + type.name + "'";

                        that[recName] = that[i] = that[name] = args[name];
                        i++;

                    }else if(name == constr)
                        throw "Accessor has the same name as the data constructor: '" + constr + "'";

            return that;
        }
        return create;
    }
}

//currently type variables on the lhs cannot be declared, and they are not checked at all:

//  function Maybe(){}
//  data(Maybe, [["Just", "a"], "Nothing"]);

//  var Just    = Maybe.Just;
//  var Nothing = Maybe.Nothing;

//Just("a") instanceof Maybe
//Just("a").constructor == Maybe
//
//Just("a").Just == true //can be used in place of pattern matching: if(maybeval.Just) ... if(maybeval.Nothing) ...
//Just("a")[0] == "a"    //access arguments by index
//
//Nothing.Nothing == true
//Nothing == Nothing


// using record syntax:
 
/*
function Type(){}

data(Type, [["Constr1", Number, "a"]
            ,"Constr2"
            ,["Constr3", {acc: Number}]
            ,["Constr4", Number]
            ]);

//in Haskell:
data Number = ... -- javascript number type
data Type a = Constr1 Number a
            | Constr2
            | Constr3 {acc :: Number}
            | Constr4 Number
*/

//Type.Constr3(record, {acc:1}).Constr3 == true
//Type.Constr3(record, {acc:1}).acc == 1
//Type.Constr3(record, {acc:1})[0] == 1
//Type.Constr3(1)[0] == 1
//Type.Constr3(1).a == 1
//Type.Constr2.Constr2 == true
//Type.Constr2 == Type.Constr2


//record update (creates a new object):

//function T(){}
//data(T, [["C", {a: String,b: String}]]);
//T.C("2","3").update({a:"4"}).a == "4"

function Maybe(){}
data(Maybe, [["Just", "a"], "Nothing"]);

function Ordering(){}
data(Ordering, ["LT", "EQ", "GT"]);

function Either(){}
data(Either, [["Left", "a"], ["Right", "b"]]);

// -------------------------------------------------
// Operators
// -------------------------------------------------

function infixl(strength){ return ["l", strength] }
function infixr(strength){ return ["r", strength] }
function infix (strength){ return ["x", strength] }

function getFixity(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return ["l", 9];

    return op && op.fixity;
}

function getFixityDir(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return "l";

    return op && (op.fixity[0] || "l" );
}

function getFixityStrn(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return 9;

    return op && (isDefined(op.fixity[1]) ? op.fixity[1] : 9);
}


var operators = {
    "$" : {
        func:   call,
        fixity: infixr(0)
        //,type:    [Function, "*", "*"]
    },
    "." : {
        func:   compose1,
        fixity: infixr(9)
        //,type:    [Function, Function, Function]
    },
    ":" : {
        func:   cons,
        fixity: infixr(5)
    }
    
};




// -------------------------------------------------
// Array expressions
// -------------------------------------------------

// -- see usage in Char

function splice_args(args, i, rec){
    var op;
    if(args[i]._Op){
        delete args[i]._Op;
        op = args[i];
    }else
        op = operators[args[i]].func;
    
    var item = op(args[i-1], args[i+1]);
    args.splice(i-1, 3 , item);
    return resolve(args, rec);
}

//in array-expressions if the square brackets are 
//not intended for groupping subexpressions
//but an actual array is needed the it should be wrapped in `arr`
//
//if a string might be the same as an operator then use `str`
//
//and functions can be used as operators by wrapping them in `op`

//var p = [string, str("<|>"), op(parserPlus), return_, arr([])].resolve();

//but usually this can be done by simply using the javascript call operator:
//
//var p = [string("<|>"), "<|>", return_([])].resolve();


function arr(a){ a._Array = true; return a;}

function str(s){ 
    var string = new String(s);
    string._String = true;
    return string;
}

function op(fn){ fn._Op = true; return fn;}


//TODO: reject multiple infix operators in the same expression
function resolve(args, rec){
    //recurse on nested array-expressions or callstreams
    args = map(function(e){ 
        if(e && e._Array){
            delete e._Array;
            return e;
        }
        return isArray(e) ? resolve(e, rec) :
                (e && e.CallStream) ? e.resolve(rec) : e;
    }, args);
    
    //inject recursive calls
    if(rec)
        args = map(function(e){return e instanceof Recurse ? rec : e}, args);
    
    //execute functions between operators
    var fna = [], fn, newfna = [], i = 0, l = args.length;
    for(; i < l; ++i){
        var e = args[i], isOp = false;
        
        if(operators[e])
            isOp = true;
        if(e && e._String){
            isOp = false;
            e = e.toString();
        }
        if(e && e._Op)
            isOp = true;

        if(!isOp && i != (l-1))
            fna.push(e);
        else{
            if(i == (l-1))
                fna.push(e);
            if(fna.length> 1){
                //if(!fna[0] || !fna[0].apply)
                //    throw ["Expecting function in array-expression instead of " + fna[0], args, fna];
                var functionInArrayExpr = fna[0];
                fn = functionInArrayExpr.apply(null, fna.slice(1));
            }
            else
                fn = fna[0];
            newfna.push(fn);
            if(i != l-1)
                newfna.push(e);
            fna = [];
        }
    }
    args = newfna;

    //execute operators
    var dir    = map(getFixityDir , args),
        strn   = map(getFixityStrn, args),
        max    = filter(strn, isDefined).sort().pop(),
        maxfst = indexOf(strn, max),
        maxlst = lastIndexOf(strn, max);
    
    return  dir[maxfst] == "l" ? splice_args(args, maxfst, rec) :
            dir[maxlst] == "r" ? splice_args(args, maxlst, rec) :
            dir[maxfst] == "x" ? splice_args(args, maxfst, rec) :
            args[0];
}

Array.prototype.resolve = function(){ return resolve(this) };

//an interface for array-expressions that handles self recursion, and evalutes lazily
function ex(){

    function rec(){ return p.apply(null, arguments) }

    var line = arguments, p, resolved;

    function expr(){
        return (resolved ? p : expr.resolve()).apply(null, arguments);
    }

    expr.resolve = function(_rec){
        if(resolved)
            return p;
        p = resolve(line, _rec || rec);
        line = null;
        resolved = true;
        return p;
    };

    expr.CallStream = true;
    expr.constructor = Parser;
    return expr;
}

// -------------------------------------------------
// Callstream interface for the do notation
// -------------------------------------------------

function Recurse(){}

var recurse = new Recurse();

function cs(){

    function rec(state, scope, k){ return p(state, scope, k) }

    var lines = [], p, resolved;

    lines.push(resolve(arguments, rec));

    function line(state, scope, k){
        if(resolved || (state instanceof ParseState))
            return (resolved ? p : line.resolve())(state, scope, k);
        
        lines.push(resolve(arguments, rec));
        return line;
    }

    line.resolve = function(){
        if(resolved)
            return p;
        p = do_.apply(null, lines);
        resolved = true;
        lines = null;
        return p;
    };

    line.CallStream = true;
    line.constructor = Parser;
    return line;
}

extend(JSParsec, {
    data      : data,
    ADT       : ADT,
    record    : record,
    Maybe     : Maybe,
    Ordering  : Ordering,
    Either    : Either,
    operators : operators,
    infix     : infix,
    infixl    : infixl,
    infixr    : infixr,
    arr       : arr,
    op        : op,
    str       : str,
    ex       : ex,
    resolve   : resolve,
    recurse   : recurse,
    Recurse   : Recurse,
    cs        : cs
});

// -------------------------------------------------
// ParseState
// -------------------------------------------------

function ParseState(input, index) {
    this.input  = input;
    this.index  = index || 0;
    this.length = input.length - this.index;
    this.cache  = {};
    return this;
}

ParseState.prototype = {

    memoize: false,

    scrollTo: function(index) {
        this.index  = index;
        this.length = this.input.length - index;
        return this;
    },

    scroll: function(index) {
        this.index  += index;
        this.length -= index;
        return this;
    },


    at: function(index){
        return this.input.charAt(this.index + index);
    },

    substring: function(start, end){
        return this.input.substring(
            start + this.index,
            (end || this.length) + this.index);
    },

    substr: function(start, length){
        return this.input.substring(
            start + this.index,
            length || this.length);
    },

    toString: function(){
        var substr = this.substring(0);
        return 'PS at ' + this.index + ' ' + 
            (substr.length ? '"' + substr + '"' : "Empty"); 
    },

    getCached: function(pid) {
        if(!this.memoize)
            return;

        var p = this.cache[pid];
        if(!p)
            return;

        var result = p[this.index];

        if(!result)
            return;

        //result.remaining === this
        this.index  = result.index;
        this.length = result.length;

        return result;
    },

    putCached: function(pid, index, cached) {
        if(!this.memoize)
            return false;
        
        //cached.remaining === this
        cached.index  = this.index;
        cached.length = this.length;


        var p = this.cache[pid];
        if(!p)
            p = this.cache[pid] = {};

        p[index] = cached;
    }
    
    ,sourceLine: function(pos){
        var m = this.input.substring(0, pos).match(/(\r\n)|\r|\n/g);
        return m ? m.length : 0;
    }

    /*

    //returns a new state object
    ,from: function(index) {
        var r = new ParseState(this.input, this.index + index);
        r.cache  = this.cache;
        r.length = this.length - index;
        return r;
    }

    ,skipWhitespace: function(){
        var m = this.substring(0).match(/^\s+/);
        return m ? this.scroll(m[0].length) : this;
    }

    */
};

function ps(str) {
    return new ParseState(str);
}




// -------------------------------------------------
// Result
// -------------------------------------------------


// remaining: is the remaining string(ParseState) to be parsed
// matched:   is the portion of the string that was successfully matched by the parser
// ast:       is the AST returned by the parse, which doesn't need to be successful
//                this is the value that Functor, Applicative, and Monad functions operate on
// success:   might be true or false
// expecting: contains the value that the parser expected but haven't matched completely or at all
//                It's either a single string, or an object with a property 'string' and 'at'.
//                If it's just a string, then the index can be determined from ParseState.index,
//                else the latter form should be used (this might be changed later!).
//                It might be an array of these values, which represents a choice.


function make_result(ast, success, expecting){
    return  {ast: ast
            ,success: success === undef ? true : success
            ,expecting: expecting
            };
}

var _EmptyOk = make_result(undef);


function _fail(expecting){
    return make_result(undef, false, expecting);
}


function unexpected(name){
    return function(state, scope, k){
        return k(make_result(null, false, {unexpected: name}));
    };
}

//accepts an identifier string, see usage with notFollowedBy
function unexpectedIdent(name){
    return function(state, scope, k){
        return k(make_result(null, false, {unexpected: scope[name]}));
    };
}


function parserFail(msg){
    return function(state, scope, k){
        return k(make_result(undef, false, msg));
    };
};

var fail = parserFail;


function parserZero(state, scope, k){
    return k(make_result(undef, false));
}

var mzero = parserZero;
var empty = mzero;



// -------------------------------------------------
// Parser
// -------------------------------------------------


// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p){
    return (typeof p == "string") ? string(p) : 
        isArray(p) ? resolve(p) : p;
}


function trampoline(x){
    while(x && x.func)
        x = x.func.apply(null, x.args || []);
}


function trampolineAsync(x, count){ //TODO: use while
    count = count || 0 ;
    count++;
    
    if(!(x && x.func)){
        count = 0;
        return;
    }

    x = x.func.apply(null, x.args || []);
    
    if(count % 500 == 0 )
        setTimeout(function(){ trampolineAsync(x, count) }, 1);
    else
        trampolineAsync(x, count);
}

function run(p, strOrState, complete, error, async){
    var input = strOrState instanceof ParseState ? strOrState : ps(strOrState);
    (async ? trampolineAsync : trampoline) ({func:p, args:[input, {}, function(result){
        result.state = input;
        delete result.index;
        delete result.length;
        if(!result.success){
            result.error = processError(result.expecting, result.state);
            error && error(result.error);
        }else{
            delete result.error;
            delete result.expecting;
        }
        complete(result);
    }]});
}

function processError(e, s, i, unexp){
    var index = i === undefined ? s.index : i;

    if(typeof e == "string"){
        var lines = s.input.split("\n"),
            linecount = lines.length,
            restlc = s.input.substr(index).split("\n").length,
            line = linecount - restlc + 1,
            lindex = index - lines.splice(0,line-1).join("\n").length,
            unexpMsg = unexp || s.input.substr(index, e.length).substr(0, 6);
        return 'Unexpected "' + (unexpMsg.length ? unexpMsg : "end of file") +  
                (unexp ? "" : ('", expecting "' + e)) + 
                '" at line ' + line + ' char ' + lindex;
    }

    if(isArray(e)){
        var err = map(function(er){ return typeof er == "object" ? er.expecting : er }, e);
        return processError(err.join('" or "'), s);
    }else if(typeof e == "object")
        return processError(e.expecting, s, e.at, e.unexpected);
}

var parser_id = 0;

function Parser(){}

function parserBind(p, f){ 
    return function(state, scope, k){
        return {func:p, args:[state, scope, function(result){
            if(result.success){
                return {func:f(result.ast), args:[state, scope, k]}
            }else{
                return k(result);
            }
        }]};
    };
}


var do2 = function(p1, p2){
    function fn(state, scope, k){
        return { func: p1, args: [state, scope, function(result){
            return result.success ? p2(state, scope, k) : k(result);
        }]};
    }
    fn.constructor = Parser;
    return fn;
};

var do_ = function(p1, p2, p3 /* ... */){
    var parsers = map(toParser, arguments);
    function fn(state, _scope, k){
        var scope = {},
            i = 1,
            l = parsers.length,
            result = parsers[0];
        
        scope.scope = _scope;

        for(; i < l; ++i)
            result = do2(result, parsers[i]);

        return result(state, scope, k);
    }
    fn.constructor = Parser;
    return fn;
};


function bind(name, p){ 
    if(name == "scope")
        throw "Can't use 'scope' as an identifier!";
    return function(state, scope, k){
        return { func: p, args: [state, scope, function(result){
            if(result.success)
                scope[name] = result.ast;
            result = extend({}, result);
            
            return k(result);
        }]};
    };
}


function ret(name, more){
    var args;
    if(more) 
        args = slice(arguments);

    return function(state, scope, k){

        return { func: function(){
            var ast, type = typeof name;
            //if(args){
            //  ast =  resolve(resolveBindings(args, scope));
            //}else 
            if(type == "string"){
                if(!(name in scope))
                    throw 'Not in scope: "' + name + '"';
                ast = scope[name];      
            }else
                ast = name(scope);

            return k(make_result(ast));

        }};
    };
}

function resolveBindings(arr, scope){
    return isArray(arr) ?
        map(function(e){ return (e in scope) ? scope[e] : resolveBindings(e) }, arr)
        : arr;
}

function withBound(fn){
    var args = slice(arguments, 1);
    return function(scope){
        return fn.apply(null, map(function(e){ return scope[e] }, args));
    };
}

var returnCall = compose(ret, withBound);

function getPosition(state, scope, k){
    return k(make_result(state.index));
}

var getParserState = getPosition; //TODO?

function setPosition(id){
    var type = typeof id;
    return function(state, scope, k){
        state.scrollTo(type == "string" ? scope[id] : id);
        return k(_EmptyOk);
    };
}

var setParserState = setPosition; //TODO?

//in contrast with Haskell here's no closure in the do_ notation,
//it's simulated with `bind` and `ret`,
//this function does what `pure` and `return` do in Haskell
function parserReturn(value){
    return function(state, scope, k){
        return k(make_result(value));
    };
}

var return_ = parserReturn;
var pure = return_;


function ap(a, b){
    return do_(bind("a", a), bind("b", b), ret(function(scope){ return scope.a(scope.b) }));
}

//liftM f m1 = do { x1 <- m1; return (f x1) }
function liftM(f, m1){
    return do_(bind("x1", m1), returnCall(f, "x1"));
}
var parsecMap = liftM;
var fmap   = parsecMap;
var liftA  = fmap;

//liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
function liftM2(f, m1, m2){
    return do_(bind("x1", m1), bind("x2", m2), returnCall(f, "x1", "x2"));
}
var liftA2 = liftM2;

//liftM3 f m1 m2 m3 = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
function liftM3(f, m1, m2, m3){
    return do_(bind("x1", m1), bind("x2", m2), bind("x3", m3),
               returnCall(f, "x1", "x2", "x3"));
}
var liftA3 = liftM3;



//var skip_fst = function(p1, p2){ return liftA2(const_(id), p1, p2) };
//function skip_fst(p1, p2){ return do_(p1, p2) }
var skip_fst = do2;

//var skip_snd = function(p1, p2){ return liftA2(const_, p1, p2) };
function skip_snd(p1, p2){ return do_(bind("a", p1), p2, ret("a")) }



function parserPlus(p1, p2){
    function fn(state, scope, k){
        return {func: p1, args:[state, scope, function(result){
            var errors =  [];

            function handleError(result){
                var err = result.expecting;
                if(err){
                    if(isArray(err))
                        errors = errors.concat(err);
                    else
                        errors.push(err);
                }
                if(!result.success)
                    result.expecting = errors;
                else
                    delete result.expecting;
            }
            
            handleError(result);
            if(result.ast !== undef)
                return {func:k, args: [result]};
            else
                return {func: p2, args: [state, scope, function(result){
                    handleError(result);
                    return k(result);
                }]};
            
        }]};
    }
    fn.constructor = Parser;
    return fn;
}

// 'parserPlus' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that matches some string 
// results in a successfull parse. It fails if all parsers fail.
function parserPlusN(p1, p2, p3 /* ... */){
    var parsers = map(toParser, arguments);
    return function(state, scope, k){
        var i = 1,
            l = parsers.length,
            result = parsers[0];
        
        for(; i < l; ++i)
            result = parserPlus(result, parsers[i]);

        return result(state, scope, k);
    };
}

var mplus = parserPlus;



//accepts multiple parsers and returns a new parser that
//evaluates them in order and
//succeeds if all the parsers succeeded
//fails when a parser fails but returns the array of previous ASTs in the result
function tokens(parsers){
    return function(state, scope, k){
        var i = 0,
            ast = [],
            length = parsers.length;
        
        function next(parser){
            return function(state, scope, k){
                return {func:parser, args:[state, scope, function(result){
                    i++;
                    if(!result.success)
                        return k(result);
                    if(result.ast !== undef)
                        ast.push(result.ast);
                    return i < length ? next(parsers[i])(state, scope, k) : k(result);
                }]};
            };
        }

        return {func:next(parsers[i]), args:[state, scope, function(_result){
            var result = extend({}, _result);
            result.ast = ast;
            if(result.success)
                delete result.expecting;
            else
                delete result.ast;
            return k(result);
        }]};
    };
}

function _many(onePlusMatch){
    return function(parser){
        return function(state, scope, k){
            var matchedOne = false,
                ast = [];
            
            function next(parser){
                return function(state, scope, k){
                    return {func:parser, args:[state, scope, function(result){
                        if(!result.success)
                            return k(result);
                            
                        matchedOne = true;
                        if(result.ast !== undef)
                            ast.push(result.ast);
                                
                        return next(parser)(state, scope, k);
                    }]};
                };
            }
    
            return {func:next(parser), args:[state, scope, function(_result){
                var result = extend({}, _result);
                result.success = !onePlusMatch || (matchedOne && onePlusMatch);
                result.ast = ast;
                if(result.success)
                    delete result.expecting;                    
                return k(result);
            }]};
        };
    };
}

var many = _many(false);

var many1 = _many(true);

//tokenPrim :: (c -> ParseState -> startIndex -> Result) -> (c -> Parser)
function tokenPrim(fn){
    return function(c){
        var pid = parser_id++;
        var combinator = function(state, scope, k){
            var startIndex = state.index;
            var result = state.getCached(pid);
            if(result !== undef)
                return k(result);
                
            result = fn(c, state, startIndex);
                        
            state.putCached(pid, startIndex, result);
            return k(result);
        };
        combinator.constructor = Parser;
        return combinator;
    };
}

//tokenPrimP1 :: (arg2 -> parser1Result -> ParseState -> startIndex -> newResult)
//              -> (Parser -> arg2 -> Parser)
function tokenPrimP1(fn){
    return function(p1, arg2){
        var pid = parser_id++;
        var combinator = function(state, scope, k){
            var startIndex = state.index;
            var result = state.getCached(pid);
            if(result !== undef)
                return k(result);
                
            return {func:p1, args:[state, scope, function(result){
                
                    result = fn(arg2, result, state, startIndex);
                    
                    state.putCached(pid, startIndex, result);
                    return k(result);
                }]};
            
        };
        combinator.constructor = Parser;
        return combinator;
    };
}


var try_ = tokenPrimP1(function(_, result, state, startIndex){
    result = extend({}, result);
    if(result.success)
        return result;
    state.scrollTo(startIndex);
    delete result.ast;
    return result;
});


var skipMany = function(p){
    return tokenPrimP1(function(_, result, state, startIndex){
        result = extend({}, result);
        result.ast = undef;
        return result;
    })(many(p), null);
};

//string :: Char -> Parser
var char_ = tokenPrim(function(c, state, startIndex){
    if(state.length > 0 && state.at(0) == c){
        state.scroll(1);
        return make_result(c);
    }
    return _fail(c);
});


//string :: (Char -> Bool) -> Parser
var satisfy = tokenPrim(function(cond, state){
    var fstchar = state.at(0);
    if(state.length > 0 && cond(fstchar)){
        state.scroll(1);
        return make_result(fstchar);
    }
    return _fail(fstchar);
});



//string :: String -> Parser
var string = function(s){ //TODO
    return tokenPrimP1(function(_, result, state, startIndex){
        result.ast = result.ast.join("");
        result = extend({}, result);
        if(!result.success)
            result.expecting = {at:startIndex, expecting: s};
        else delete result.expecting;
        if(!result.ast.length) //TODO
            result.ast = undef;
        return result;
    })(tokens(map(char_, s)), null);
};


//tokenPrimP1 :: (a -> parser1Result -> ParseState -> startIndex -> newResult)
//              -> (Parser -> a -> Parser)
//label :: Parser -> String -> Parser
var label = tokenPrimP1(function(str, result, state, startIndex){
    if(!result.success){
        result = extend({}, result);
        result.expecting = {at: startIndex, expecting: str};
    }
    return result;  
});


//accepts a regexp or a string
//in case of a string it either matches the whole string or nothing

//match :: StringOrRegex -> Parser
var match = tokenPrim(function(sr, state){
        var result;
        if(typeof sr == "string"){
            if(state.substring(0, sr.length) == sr){
                state.scroll(sr.length);
                result = make_result(sr);
            }else
                result = _fail(sr);
                        
        }else if(sr.exec){
            var rx = new RegExp("^" + sr.source);
            var substr = state.substring(0);
            var match = rx.exec(substr);
            match = match && match[0];
            var length = match && match.length;
            var matched = substr.substr(0, length);
            if(length){
                state.scroll(length);
                result = make_result(matched);
            }else
                result = _fail(sr.source);
        }
        return result;
});


//from Control.Monad
//
//-- | Evaluate each action in the sequence from left to right,
//-- and collect the results.
//sequence       :: Monad m => [m a] -> m [a] 
//{-# INLINE sequence #-}
//sequence ms = foldr k (return []) ms
//            where
//              k m m' = do { x <- m; xs <- m'; return (x:xs) }

function sequence(ms){

    function k(m1, m2){
        return do_(
            bind("x", m1),
            bind("xs", m2),
            ret(withBound(cons, "x", "xs"))
        );
    }

    return foldr(k, return_([]), ms);
}



extend(operators, {
    "<-" : {
        func:   bind,
        fixity: infixr(-1) //this is a special operator, don't use negative fixity anywhere else!
        //,type:    [String, Parser, Parser]
    },
    ">>=": {
        func:   parserBind,
        fixity: infixl(1)
        //,type:    [Parser, Function, Parser]
    },
    "=<<": {
        func:   flip(parserBind),
        fixity: infixr(1)
        //,type:    [Parser, Parser, Parser]
    },
    ">>" : {
        func:   skip_fst,
        fixity: infixl(1)
        //,type:    [Parser, Parser, Parser]
    },
    "*>" : { //liftA2 (const id)
        func:   skip_fst,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<*" : { //liftA2 const
        func:   skip_snd,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<$>": {
        func:   fmap,
        fixity: infixl(4)
        //,type:    [Function, Parser, Parser]
    },
    "<*>": {
        func:   ap,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<**>": { //liftA2 (flip ($))
        func:   curry(liftA2)(flip(call)),
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
        //the (<$) combinator uses the value on the left 
        //if the parser on the right succeeds. x <$ p = pure x <* p
        //from Control.Applicative: (<$>) . const :: Functor f => a -> f b -> f a
    "<$" : {
        func:   function(val, parser){ return skip_snd(pure(value), parser) },
        fixity: infixl(4)
        //,type:    ["*", Parser, Parser]
    },
    "<|>": {
        func:   parserPlus,
        fixity: infixr(1)
        //,type:    [Parser, Parser, Parser]
    },
    "<?>": {
        func:   label,
        fixity: infix(0)
        //,type:    [Parser, String, Parser]
    }   
});

function lazy(f){
    return function(state, scope, k){
        return f()(state, scope, k);
    }
}

extend(JSParsec, {
    sequence        : sequence,
    run             : run,
    Parser          : Parser,
    ParseState      : ParseState,
    ps              : ps, 
    toParser        : toParser,
    unexpected      : unexpected,
    parsecMap       : parsecMap,
    fmap            : fmap,
    liftM           : liftM,
    liftM2          : liftM2,
    liftM3          : liftM3,
    liftA           : liftA,
    liftA2          : liftA2,
    liftA3          : liftA3,
    ap              : ap,
    parserBind      : parserBind,
    parserReturn    : parserReturn,
    return_         : return_,
    pure            : pure,
    parserFail      : parserFail,
    fail            : fail,
    parserZero      : parserZero,
    mzero           : mzero,
    empty           : empty,
    parserPlus      : parserPlus,
    parserPlusN     : parserPlusN,
    mplus           : mplus,
    do_             : do_,
    do2             : do2,
    bind            : bind,
    ret             : ret,
    withBound       : withBound,
    returnCall      : returnCall,
    lazy            : lazy,
    getPosition     : getPosition,
    setPosition     : setPosition,
    getParserState  : getParserState,
    setParserState  : setParserState,
    tokens          : tokens,
    many            : many,
    many1           : many1,
    string          : string,
    char_           : char_,
    satisfy         : satisfy,
    label           : label,
    try_            : try_,
    skipMany        : skipMany,
    match           : match
});


// -------------------------------------------------
// Char
// -------------------------------------------------


//-- Commonly used character parsers.

//module Text.Parsec.Char where
//
//import Data.Char
//import Text.Parsec.Pos
//import Text.Parsec.Prim

// | @oneOf cs@ succeeds if the current character is in the supplied
// list of characters @cs@. Returns the parsed character. See also
// 'satisfy'.
// 
// >   vowel  = oneOf "aeiou"

//oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//oneOf cs            = satisfy (\c -> elem c cs)

var oneOf = function(cs){
	return label(satisfy(function(c){ return elem(c, cs) }), "oneOf(" + cs + ")");
};

// | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
// character /not/ in the supplied list of characters @cs@. Returns the
// parsed character.
//
// >  consonant = noneOf "aeiou"

//noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//noneOf cs           = satisfy (\c -> not (elem c cs))

var noneOf = function(cs){
	return label(satisfy(function(c){ return !elem(c, cs) }), "noneOf(" + cs + ")");
};


// | Parses a white space character (any character which satisfies 'isSpace')
// Returns the parsed character. 

//space :: (Stream s m Char) => ParsecT s u m Char
//space               = satisfy isSpace       <?> "space"

var space = [satisfy, isSpace ,"<?>", "space"].resolve();


// | Skips /zero/ or more white space characters. See also 'skipMany'.

//spaces :: (Stream s m Char) => ParsecT s u m ()
//spaces              = skipMany space        <?> "white space"

var spaces = [skipMany, space ,"<?>", "white space"].resolve();


// | Parses a newline character (\'\\n\'). Returns a newline character. 

//newline :: (Stream s m Char) => ParsecT s u m Char
//newline             = char '\n'             <?> "new-line"

var newline = [char_, '\n' ,"<?>", "new-line"].resolve();

// | Parses a tab character (\'\\t\'). Returns a tab character. 

//tab :: (Stream s m Char) => ParsecT s u m Char
//tab                 = char '\t'             <?> "tab"

var tab = [char_, '\t' ,"<?>", "tab"].resolve();

// | Parses an upper case letter (a character between \'A\' and \'Z\').
// Returns the parsed character. 

//upper :: (Stream s m Char) => ParsecT s u m Char
//upper               = satisfy isUpper       <?> "uppercase letter"

var upper = [satisfy, isUpper ,"<?>", "uppercase letter"].resolve();


// | Parses a lower case character (a character between \'a\' and \'z\').
// Returns the parsed character. 

//lower :: (Stream s m Char) => ParsecT s u m Char
//lower               = satisfy isLower       <?> "lowercase letter"

var lower = [satisfy, isLower ,"<?>", "lowercase letter"].resolve();


// | Parses a letter or digit (a character between \'0\' and \'9\').
// Returns the parsed character. 

//alphaNum :: (Stream s m Char => ParsecT s u m Char)
//alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

var alphaNum = [satisfy, isAlphaNum ,"<?>", "letter or digit"].resolve();


// | Parses a letter (an upper case or lower case character). Returns the
// parsed character. 

//letter :: (Stream s m Char) => ParsecT s u m Char
//letter              = satisfy isAlpha       <?> "letter"

var letter = [satisfy, isAlpha ,"<?>", "letter"].resolve();

// | Parses a digit. Returns the parsed character. 

//digit :: (Stream s m Char) => ParsecT s u m Char
//digit               = satisfy isDigit       <?> "digit"

var digit = [satisfy, isDigit ,"<?>", "digit"].resolve();


// | Parses a hexadecimal digit (a digit or a letter between \'a\' and
// \'f\' or \'A\' and \'F\'). Returns the parsed character. 

//hexDigit :: (Stream s m Char) => ParsecT s u m Char
//hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

var hexDigit = [satisfy, isHexDigit ,"<?>", "hexadecimal digit"].resolve();


// | Parses an octal digit (a character between \'0\' and \'7\'). Returns
// the parsed character. 

//octDigit :: (Stream s m Char) => ParsecT s u m Char
//octDigit            = satisfy isOctDigit    <?> "octal digit"

var octDigit = [satisfy, isOctDigit ,"<?>", "octal digit"].resolve();


// | This parser succeeds for any character. Returns the parsed character. 

//anyChar :: (Stream s m Char) => ParsecT s u m Char
//anyChar             = satisfy (const True)

var anyChar = [satisfy, const_(true)].resolve();


// | @char c@ parses a single character @c@. Returns the parsed
// character (i.e. @c@).
//
// >  semiColon  = char ';'

//char c              = satisfy (==c)  <?> show [c]

//var char_ = function(c){
//	return [satisfy, function(ch){ return ch == c } ,"<?>", c].resolve();
//}

// -- a specialized version is defined in Prim



// | @string s@ parses a sequence of characters given by @s@. Returns
// the parsed string (i.e. @s@).
//
// >  divOrMod    =   string "div" 
// >              <|> string "mod"

//string :: (Stream s m Char) => String -> ParsecT s u m String
//string s            = tokens show updatePosString s

// -- defined in Prim

extend(JSParsec, {
    oneOf    : oneOf,
    noneOf   : noneOf,
    space    : space,
    spaces   : spaces,
    newline  : newline,
    tab      : tab,
    upper    : upper,
    lower    : lower,
    alphaNum : alphaNum,
    letter   : letter,
    digit    : digit,
    hexDigit : hexDigit,
    octDigit : octDigit,
    anyChar  : anyChar
});

// -------------------------------------------------
// Combinator
// -------------------------------------------------


//-- Commonly used generic combinators

//module Text.Parsec.Combinator
//    ( choice
//    , count
//    , between
//    , option, optionMaybe, optional
//    , skipMany1
//    , many1
//    , sepBy, sepBy1
//    , endBy, endBy1
//    , sepEndBy, sepEndBy1
//    , chainl, chainl1
//    , chainr, chainr1
//    , eof, notFollowedBy
//    -- tricky combinators
//    , manyTill, lookAhead, anyToken
//    ) where
//
//import Control.Monad
//import Text.Parsec.Prim
//


//-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
//-- until one of them succeeds. Returns the value of the succeeding
//-- parser.
//
//choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
//choice ps           = foldr (<|>) mzero ps
//

function choice(ps){
    return foldr(parserPlus, mzero, ps);
}


//-- | @option x p@ tries to apply parser @p@. If @p@ fails without
//-- consuming input, it returns the value @x@, otherwise the value
//-- returned by @p@.
//--
//-- >  priority  = option 0 (do{ d <- digit
//-- >                          ; return (digitToInt d) 
//-- >                          })
//
//option :: (Stream s m t) => a -> ParsecT s u m a -> ParsecT s u m a
//option x p          = p <|> return x
//

function option(x, p){
    return parserPlus(p, return_(x));
}


//-- | @optionMaybe p@ tries to apply parser @p@.  If @p@ fails without
//-- consuming input, it return 'Nothing', otherwise it returns
//-- 'Just' the value returned by @p@.
//
//optionMaybe :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (Maybe a)
//optionMaybe p       = option Nothing (liftM Just p)
//

function optionMaybe(p){
    return option(Maybe.Nothing, liftM(Maybe.Just, p));
}


//-- | @optional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
//-- It only fails if @p@ fails after consuming input. It discards the result
//-- of @p@.
//
//optional :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
//optional p          = do{ p; return ()} <|> return ()
//

function optional(p){
    return parserPlus(do_(p, return_(null)), return_(null));
}


//-- | @between open close p@ parses @open@, followed by @p@ and @close@.
//-- Returns the value returned by @p@.
//--
//-- >  braces  = between (symbol "{") (symbol "}")
//
//between :: (Stream s m t) => ParsecT s u m open -> ParsecT s u m close
//            -> ParsecT s u m a -> ParsecT s u m a
//between open close p
//                    = do{ open; x <- p; close; return x }
//

var between = curry(function(open, close, p){
    return do_(open, bind("x", p), close, ret("x"));
});


//-- | @skipMany1 p@ applies the parser @p@ /one/ or more times, skipping
//-- its result. 
//
//skipMany1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
//skipMany1 p         = do{ p; skipMany p }
//{-
//skipMany p          = scan
//                    where
//                      scan  = do{ p; scan } <|> return ()
//-}
//

function skipMany1(p){
    return do_(p, skipMany(p));
}

//-- | @many p@ applies the parser @p@ /one/ or more times. Returns a
//-- list of the returned values of @p@.
//--
//-- >  word  = many1 letter
//
//many1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
//many1 p             = do{ x <- p; xs <- many p; return (x:xs) }
//{-
//many p              = scan id
//                    where
//                      scan f    = do{ x <- p
//                                    ; scan (\tail -> f (x:tail))
//                                    }
//                                <|> return (f [])
//-}
//
//

// -- defined in Prim


//-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
//-- by @sep@. Returns a list of values returned by @p@.
//--
//-- >  commaSep p  = p `sepBy` (symbol ",")
//
//sepBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepBy p sep         = sepBy1 p sep <|> return []
//

function sepBy(p, sep){
    return parserPlus(sepBy1(p, sep), return_([]));
}


//-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
//-- by @sep@. Returns a list of values returned by @p@. 
//
//sepBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepBy1 p sep        = do{ x <- p
//                        ; xs <- many (sep >> p)
//                        ; return (x:xs)
//                        }
//
//

function sepBy1(p, sep){
    return do_(
        bind("x", p),
        bind("xs", many( do_(sep, p) ) ),
        returnCall(cons, "x", "xs")
    );
}


//-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
//-- separated and optionally ended by @sep@. Returns a list of values
//-- returned by @p@. 
//
//sepEndBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepEndBy1 p sep     = do{ x <- p
//                        ; do{ sep
//                            ; xs <- sepEndBy p sep
//                            ; return (x:xs)
//                            }
//                          <|> return [x]
//                        }
//

function sepEndBy1(p, sep){
    return do_(
        bind("x", p),
        parserPlus(
            do_(
                sep,
                //bind("xs", sepEndBy(p, sep)),
                //thanks to eager evaluation this doesn't terminate without eta-expansion
                bind("xs", function(state, scope, k){ return sepEndBy(p, sep)(state, scope, k) }),
                ret(function(scope){ return cons(scope.scope.x, scope.xs) })
            ),
            ret(function(scope){ return [scope.x] })
        )
    );
}

//-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
//-- separated and optionally ended by @sep@, ie. haskell style
//-- statements. Returns a list of values returned by @p@.
//--
//-- >  haskellStatements  = haskellStatement `sepEndBy` semi
//
//sepEndBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepEndBy p sep      = sepEndBy1 p sep <|> return []
//
//

function sepEndBy(p, sep){
    return parserPlus(sepEndBy1(p, sep), return_([]));
}

//-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
//-- and ended by @sep@. Returns a list of values returned by @p@. 
//
//endBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//endBy1 p sep        = many1 (do{ x <- p; sep; return x })
//

function endBy1(p, sep){
    return many1(do_( bind("x", p),  sep, ret("x") ));
}


//-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
//-- and ended by @sep@. Returns a list of values returned by @p@.
//--
//-- >   cStatements  = cStatement `endBy` semi
//
//endBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//endBy p sep         = many (do{ x <- p; sep; return x })
//

function endBy(p, sep){
    return many(do_( bind("x", p),  sep, ret("x") ));
}


//-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
//-- equal to zero, the parser equals to @return []@. Returns a list of
//-- @n@ values returned by @p@. 
//
//count :: (Stream s m t) => Int -> ParsecT s u m a -> ParsecT s u m [a]
//count n p           | n <= 0    = return []
//                    | otherwise = sequence (replicate n p)
//

function count(n, p){
    return (n <= 0) ? return_([]) : sequence(replicate(n, p));
}

//-- | @chainr p op x@ parser /zero/ or more occurrences of @p@,
//-- separated by @op@ Returns a value obtained by a /right/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. If there are no occurrences of @p@, the value @x@ is
//-- returned.
//
//chainr :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
//chainr p op x       = chainr1 p op <|> return x
//

function chainr(p, op, x){
    return parserPlus(chainr1(p, op), return_(x));
}


//-- | @chainl p op x@ parser /zero/ or more occurrences of @p@,
//-- separated by @op@. Returns a value obtained by a /left/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. If there are zero occurrences of @p@, the value @x@ is
//-- returned.
//
//chainl :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
//chainl p op x       = chainl1 p op <|> return x
//

function chainl(p, op, x){
    return parserPlus(chainl1(p, op), return_(x));
}


//-- | @chainl1 p op x@ parser /one/ or more occurrences of @p@,
//-- separated by @op@ Returns a value obtained by a /left/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. . This parser can for example be used to eliminate left
//-- recursion which typically occurs in expression grammars.
//--
//-- >  expr    = term   `chainl1` addop
//-- >  term    = factor `chainl1` mulop
//-- >  factor  = parens expr <|> integer
//-- >
//-- >  mulop   =   do{ symbol "*"; return (*)   }
//-- >          <|> do{ symbol "/"; return (div) }
//-- >
//-- >  addop   =   do{ symbol "+"; return (+) }
//-- >          <|> do{ symbol "-"; return (-) }
//
//chainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
//chainl1 p op        = do{ x <- p; rest x }
//                    where
//                      rest x    = do{ f <- op
//                                    ; y <- p
//                                    ; rest (f x y)
//                                    }
//                                <|> return x
//

function chainl1(p, op){
    var scan =  do_( 
                    bind("x", p), 
                    function(state, scope, k){ return rest(scope.x)(state, scope, k) }
                );

    function rest(x){ 
        var a = do_(
                    bind("f", op),
                    bind("y", p),
                    function(state, scope, k){
                        return rest(scope.f(x, scope.y))(state, scope, k);
                    }
                );
        return parserPlus(a, return_(x));
    }

    return scan;
}


//-- | @chainr1 p op x@ parser /one/ or more occurrences of |p|,
//-- separated by @op@ Returns a value obtained by a /right/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@.
//
//chainr1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
//chainr1 p op        = scan
//                    where
//                      scan      = do{ x <- p; rest x }
//
//                      rest x    = do{ f <- op
//                                    ; y <- scan
//                                    ; return (f x y)
//                                    }
//                                <|> return x
//

function chainr1(p, op){
    var scan =  do_( 
                    bind("x", p), 
                    function(state, scope, k){ return rest(scope.x)(state, scope, k) }
                );

    function rest(x){ 
        var a = do_(
                    bind("f", op),
                    bind("y", scan),
                    function(state, scope, k){
                        return k(make_result(scope.f(x, scope.y)));
                    }
                );
        return parserPlus(a, return_(x));
    }

    return scan;
}




//-----------------------------------------------------------
//-- Tricky combinators
//-----------------------------------------------------------


//-- | The parser @anyToken@ accepts any kind of token. It is for example
//-- used to implement 'eof'. Returns the accepted token. 
//
//anyToken :: (Stream s m t, Show t) => ParsecT s u m t
//anyToken            = tokenPrim show (\pos _tok _toks -> pos) Just
//

function anyToken(state, scope, k){
    var at = state.at(0);
    if(at.length){
        state.scroll(1);
        return k(make_result(at));
    }
    
    return k(_fail("anyToken"));
}


//-- | This parser only succeeds at the end of the input. This is not a
//-- primitive parser but it is defined using 'notFollowedBy'.
//--
//-- >  eof  = notFollowedBy anyToken <?> "end of input"
//
//eof :: (Stream s m t, Show t) => ParsecT s u m ()
//eof                 = notFollowedBy anyToken <?> "end of input"
//

// this works too:
// var eof = [notFollowedBy, anyToken ,"<?>", "end of input"].resolve();
function eof(state, scope, k){
    return k(make_result(undef, !state.length, state.length ? "end of input" : undef));
}

//-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
//-- does not consume any input. This parser can be used to implement the
//-- \'longest match\' rule. For example, when recognizing keywords (for
//-- example @let@), we want to make sure that a keyword is not followed
//-- by a legal identifier character, in which case the keyword is
//-- actually an identifier (for example @lets@). We can program this
//-- behaviour as follows:
//--
//-- >  keywordLet  = try (do{ string "let"
//-- >                       ; notFollowedBy alphaNum
//-- >                       })
//
//notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
//notFollowedBy p     = try (do{ c <- try p; unexpected (show c) }
//                           <|> return ()
//                          )
//

function notFollowedBy(p){
    return try_(
        parserPlus(
            do_(
                bind("c", try_(p)),
                unexpectedIdent("c")
            ),
            return_(null)
        )
    );
}


//-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
//-- parser @end@ succeeds. Returns the list of values returned by @p@.
//-- This parser can be used to scan comments:
//--
//-- >  simpleComment   = do{ string "<!--"
//-- >                      ; manyTill anyChar (try (string "-->"))
//-- >                      }
//--
//--    Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
//--    therefore the use of the 'try' combinator.
//
//manyTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
//manyTill p end      = scan
//                    where
//                      scan  = do{ end; return [] }
//                            <|>
//                              do{ x <- p; xs <- scan; return (x:xs) }
//

function manyTill(p, end){

    function _scan(state, scope, k){ return scan(state, scope, k) }

    var scan = parserPlus(
        do_( end, return_([]) ),
        do_( bind("x", p), bind("xs", _scan), returnCall(cons, "x", "xs") )
    );

    return scan;
}


//-- | @lookAhead p@ parses @p@ without consuming any input.
//
//lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
//lookAhead p         = do{ state <- getParserState
//                        ; x <- p
//                        ; setParserState state
//                        ; return x
//                        }

function lookAhead(p){
    return do_(
        bind("state", getParserState),
        bind("x", p),
        setParserState("state"),
        ret("x")
    );
}


extend(JSParsec, {
    choice        : choice
  , count         : count
  , between       : between
  , option        : option
  , optionMaybe   : optionMaybe
  , optional      : optional
  , skipMany1     : skipMany1
//, many1         : many1
  , sepBy         : sepBy
  , sepBy1        : sepBy1
  , endBy         : endBy
  , endBy1        : endBy1
  , sepEndBy      : sepEndBy
  , sepEndBy1     : sepEndBy1
  , chainl        : chainl
  , chainl1       : chainl1
  , chainr        : chainr
  , chainr1       : chainr1
  , eof           : eof
  , notFollowedBy : notFollowedBy
  , manyTill      : manyTill
  , lookAhead     : lookAhead
  , anyToken      : anyToken
});

// -------------------------------------------------
// Token
// -------------------------------------------------

 
//-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
//-- for a description of how to use it.

//module Text.Parsec.Token
//  ( LanguageDef
//  , GenLanguageDef (..)
//  , TokenParser
//  , GenTokenParser (..)
//  , makeTokenParser
//  ) where
//
//import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
//import Data.List ( nub, sort )
//import Control.Monad.Identity
//import Text.Parsec.Prim
//import Text.Parsec.Char
//import Text.Parsec.Combinator
//
//-----------------------------------------------------------
//-- Language Definition
//-----------------------------------------------------------
//
//type LanguageDef st = GenLanguageDef String st Identity
//
//-- | The @GenLanguageDef@ type is a record that contains all parameterizable
//-- features of the 'Text.Parsec.Token' module. The module 'Text.Parsec.Language'
//-- contains some default definitions.
//

//TODO: extend Parser and Array types (not implemented yet in Haskell.js)
function GenLanguageDef(){}

data(GenLanguageDef, [["LanguageDef", {

//data GenLanguageDef s u m
//  = LanguageDef { 
//  
//  -- | Describes the start of a block comment. Use the empty string if the
//  -- language doesn't support block comments. For example \"\/*\". 
//
//  commentStart   :: String,
commentStart: String,
//
//  -- | Describes the end of a block comment. Use the empty string if the
//  -- language doesn't support block comments. For example \"*\/\". 
//
//  commentEnd     :: String,
commentEnd: String,
//
//  -- | Describes the start of a line comment. Use the empty string if the
//  -- language doesn't support line comments. For example \"\/\/\". 
//
//  commentLine    :: String,
commentLine: String,
//
//  -- | Set to 'True' if the language supports nested block comments. 
//
//  nestedComments :: Bool,
nestedComments: Boolean,
//
//  -- | This parser should accept any start characters of identifiers. For
//  -- example @letter \<|> char \"_\"@. 
//
//  identStart     :: ParsecT s u m Char,
identStart: Parser,
//
//  -- | This parser should accept any legal tail characters of identifiers.
//  -- For example @alphaNum \<|> char \"_\"@. 
//
//  identLetter    :: ParsecT s u m Char,
identLetter: Parser,
//
//  -- | This parser should accept any start characters of operators. For
//  -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@ 
//
//  opStart        :: ParsecT s u m Char,
opStart: Parser,
//
//  -- | This parser should accept any legal tail characters of operators.
//  -- Note that this parser should even be defined if the language doesn't
//  -- support user-defined operators, or otherwise the 'reservedOp'
//  -- parser won't work correctly. 
//
//  opLetter       :: ParsecT s u m Char,
opLetter: Parser,
//
//  -- | The list of reserved identifiers. 
//
//  reservedNames  :: [String],
reservedNames: Array,
//
//  -- | The list of reserved operators. 
//
//  reservedOpNames:: [String],
reservedOpNames: Array,
//
//  -- | Set to 'True' if the language is case sensitive. 
//
//  caseSensitive  :: Bool
caseSensitive: Boolean
//
//  }

}]]);



//-----------------------------------------------------------
//-- A first class module: TokenParser
//-----------------------------------------------------------
//
//type TokenParser st = GenTokenParser String st Identity
//
//-- | The type of the record that holds lexical parsers that work on
//-- @s@ streams with state @u@ over a monad @m@.
//

function GenTokenParser(){}

data(GenTokenParser, [["TokenParser", {

//data GenTokenParser s u m
//  = TokenParser {
//
//      -- | This lexeme parser parses a legal identifier. Returns the identifier
//      -- string. This parser will fail on identifiers that are reserved
//      -- words. Legal identifier (start) characters and reserved words are
//      -- defined in the 'LanguageDef' that is passed to
//      -- 'makeTokenParser'. An @identifier@ is treated as
//      -- a single token using 'try'.
//
//      identifier       :: ParsecT s u m String,
identifier: Parser,
//      
//      -- | The lexeme parser @reserved name@ parses @symbol 
//      -- name@, but it also checks that the @name@ is not a prefix of a
//      -- valid identifier. A @reserved@ word is treated as a single token
//      -- using 'try'. 
//
//      reserved         :: String -> ParsecT s u m (),
reserved: Function,
//
//      -- | This lexeme parser parses a legal operator. Returns the name of the
//      -- operator. This parser will fail on any operators that are reserved
//      -- operators. Legal operator (start) characters and reserved operators
//      -- are defined in the 'LanguageDef' that is passed to
//      -- 'makeTokenParser'. An @operator@ is treated as a
//      -- single token using 'try'. 
//
//      operator         :: ParsecT s u m String,
operator: Parser,
//
//      -- |The lexeme parser @reservedOp name@ parses @symbol
//      -- name@, but it also checks that the @name@ is not a prefix of a
//      -- valid operator. A @reservedOp@ is treated as a single token using
//      -- 'try'. 
//
//      reservedOp       :: String -> ParsecT s u m (),
reservedOp: Function,
//
//
//      -- | This lexeme parser parses a single literal character. Returns the
//      -- literal character value. This parsers deals correctly with escape
//      -- sequences. The literal character is parsed according to the grammar
//      -- rules defined in the Haskell report (which matches most programming
//      -- languages quite closely). 
//
//      charLiteral      :: ParsecT s u m Char,
charLiteral: Parser,
//
//      -- | This lexeme parser parses a literal string. Returns the literal
//      -- string value. This parsers deals correctly with escape sequences and
//      -- gaps. The literal string is parsed according to the grammar rules
//      -- defined in the Haskell report (which matches most programming
//      -- languages quite closely). 
//
//      stringLiteral    :: ParsecT s u m String,
stringLiteral: Parser,
//
//      -- | This lexeme parser parses a natural number (a positive whole
//      -- number). Returns the value of the number. The number can be
//      -- specified in 'decimal', 'hexadecimal' or
//      -- 'octal'. The number is parsed according to the grammar
//      -- rules in the Haskell report. 
//
//      natural          :: ParsecT s u m Integer,
natural: Parser,
//
//      -- | This lexeme parser parses an integer (a whole number). This parser
//      -- is like 'natural' except that it can be prefixed with
//      -- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
//      -- number can be specified in 'decimal', 'hexadecimal'
//      -- or 'octal'. The number is parsed according
//      -- to the grammar rules in the Haskell report. 
//      
//      integer          :: ParsecT s u m Integer,
integer: Parser,
//
//      -- | This lexeme parser parses a floating point value. Returns the value
//      -- of the number. The number is parsed according to the grammar rules
//      -- defined in the Haskell report. 
//
//      float            :: ParsecT s u m Double,
float_: Parser,
//
//      -- | This lexeme parser parses either 'natural' or a 'float'.
//      -- Returns the value of the number. This parsers deals with
//      -- any overlap in the grammar rules for naturals and floats. The number
//      -- is parsed according to the grammar rules defined in the Haskell report. 
//
//      naturalOrFloat   :: ParsecT s u m (Either Integer Double),
naturalOrFloat: Parser,
//
//      -- | Parses a positive whole number in the decimal system. Returns the
//      -- value of the number. 
//
//      decimal          :: ParsecT s u m Integer,
decimal: Parser,
//
//      -- | Parses a positive whole number in the hexadecimal system. The number
//      -- should be prefixed with \"0x\" or \"0X\". Returns the value of the
//      -- number. 
//
//      hexadecimal      :: ParsecT s u m Integer,
hexadecimal: Parser,
//
//      -- | Parses a positive whole number in the octal system. The number
//      -- should be prefixed with \"0o\" or \"0O\". Returns the value of the
//      -- number. 
//
//      octal            :: ParsecT s u m Integer,
octal: Parser,
//
//      -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
//      -- trailing white space. 
//
//      symbol           :: String -> ParsecT s u m String,
symbol: Function,
//
//      -- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
//      -- parser, returning the value of @p@. Every lexical
//      -- token (lexeme) is defined using @lexeme@, this way every parse
//      -- starts at a point without white space. Parsers that use @lexeme@ are
//      -- called /lexeme/ parsers in this document.
//      -- 
//      -- The only point where the 'whiteSpace' parser should be
//      -- called explicitly is the start of the main parser in order to skip
//      -- any leading white space.
//      --
//      -- >    mainParser  = do{ whiteSpace
//      -- >                     ; ds <- many (lexeme digit)
//      -- >                     ; eof
//      -- >                     ; return (sum ds)
//      -- >                     }
//
//      lexeme           :: forall a. ParsecT s u m a -> ParsecT s u m a,
lexeme: Function,
//
//      -- | Parses any white space. White space consists of /zero/ or more
//      -- occurrences of a 'space', a line comment or a block (multi
//      -- line) comment. Block comments may be nested. How comments are
//      -- started and ended is defined in the 'LanguageDef'
//      -- that is passed to 'makeTokenParser'. 
//
//      whiteSpace       :: ParsecT s u m (),
whiteSpace: Parser,
//
//      -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
//      -- returning the value of @p@.
//
//      parens           :: forall a. ParsecT s u m a -> ParsecT s u m a,
parens: Function,
//
//      -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
//      -- \'}\'), returning the value of @p@. 
//
//      braces           :: forall a. ParsecT s u m a -> ParsecT s u m a,
braces: Function,
//
//      -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
//      -- and \'>\'), returning the value of @p@. 
//
//      angles           :: forall a. ParsecT s u m a -> ParsecT s u m a,
angles: Function,
//
//      -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
//      -- and \']\'), returning the value of @p@. 
//
//      brackets         :: forall a. ParsecT s u m a -> ParsecT s u m a,
brackets: Function,
//
//      -- | DEPRECATED: Use 'brackets'.
//
//      squares          :: forall a. ParsecT s u m a -> ParsecT s u m a,
squares: Function,
//
//      -- | Lexeme parser |semi| parses the character \';\' and skips any
//      -- trailing white space. Returns the string \";\". 
//
//      semi             :: ParsecT s u m String,
semi: Parser,
//
//      -- | Lexeme parser @comma@ parses the character \',\' and skips any
//      -- trailing white space. Returns the string \",\". 
//
//      comma            :: ParsecT s u m String,
comma: Parser,
//
//      -- | Lexeme parser @colon@ parses the character \':\' and skips any
//      -- trailing white space. Returns the string \":\". 
//
//      colon            :: ParsecT s u m String,
colon: Parser,
//
//      -- | Lexeme parser @dot@ parses the character \'.\' and skips any
//      -- trailing white space. Returns the string \".\". 
//
//      dot              :: ParsecT s u m String,
dot: Parser,
//
//      -- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
//      -- separated by 'semi'. Returns a list of values returned by
//      -- @p@.
//
//      semiSep          :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep: Function,
//
//      -- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
//      -- separated by 'semi'. Returns a list of values returned by @p@. 
//
//      semiSep1         :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep1: Function,
//
//      -- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
//      -- @p@ separated by 'comma'. Returns a list of values returned
//      -- by @p@. 
//
//      commaSep        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep: Function,
//
//      -- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
//      -- @p@ separated by 'comma'. Returns a list of values returned
//      -- by @p@. 
//
//      commaSep1        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep1: Function
//  }
}]]);

//
//-----------------------------------------------------------
//-- Given a LanguageDef, create a token parser.
//-----------------------------------------------------------
//
//-- | The expression @makeTokenParser language@ creates a 'GenTokenParser'
//-- record that contains lexical parsers that are
//-- defined using the definitions in the @language@ record.
//--
//-- The use of this function is quite stylized - one imports the
//-- appropiate language definition and selects the lexical parsers that
//-- are needed from the resulting 'GenTokenParser'.
//--
//-- >  module Main where
//-- >
//-- >  import Text.Parsec
//-- >  import qualified Text.Parsec.Token as P
//-- >  import Text.Parsec.Language (haskellDef)
//-- >
//-- >  -- The parser
//-- >  ...
//-- >
//-- >  expr  =   parens expr
//-- >        <|> identifier
//-- >        <|> ...
//-- >       
//-- >
//-- >  -- The lexer
//-- >  lexer       = P.makeTokenParser haskellDef    
//-- >      
//-- >  parens      = P.parens lexer
//-- >  braces      = P.braces lexer
//-- >  identifier  = P.identifier lexer
//-- >  reserved    = P.reserved lexer
//-- >  ...
//
//makeTokenParser :: (Stream s m Char)
//              => GenLanguageDef s u m -> GenTokenParser s u m
//makeTokenParser languageDef
//  = TokenParser{ identifier = identifier
//               , reserved = reserved
//               , operator = operator
//               , reservedOp = reservedOp
//
//               , charLiteral = charLiteral
//               , stringLiteral = stringLiteral
//               , natural = natural
//               , integer = integer
//               , float = float
//               , naturalOrFloat = naturalOrFloat
//               , decimal = decimal
//               , hexadecimal = hexadecimal
//               , octal = octal
//
//               , symbol = symbol
//               , lexeme = lexeme
//               , whiteSpace = whiteSpace
//
//               , parens = parens
//               , braces = braces
//               , angles = angles
//               , brackets = brackets
//               , squares = brackets
//               , semi = semi
//               , comma = comma
//               , colon = colon
//               , dot = dot
//               , semiSep = semiSep
//               , semiSep1 = semiSep1
//               , commaSep = commaSep
//               , commaSep1 = commaSep1
//               }


function makeTokenParser(languageDef){
    if(!languageDef.LanguageDef)
        throw "Type error: unexpected '" + languageDef.constructor.name + "', expecting 'GenLanguageDef.LanguageDef'";


//  -----------------------------------------------------------
//  -- White space & symbols
//  -----------------------------------------------------------

//  symbol name
//      = lexeme (string name)

function symbol(name){
    return lexeme(string(name));
}


//
//  lexeme p
//      = do{ x <- p; whiteSpace; return x  }

function lexeme(p){
    return do_(bind("x", p), whiteSpace, ret("x") );
}


//
//
//  simpleSpace =
//      skipMany1 (satisfy isSpace)

var simpleSpace =
        skipMany1(satisfy(isSpace));


//
//  oneLineComment =
//      do{ try (string (commentLine languageDef))
//        ; skipMany (satisfy (/= '\n'))
//        ; return ()
//        }

var oneLineComment =
        cs( try_(string(languageDef.commentLine)) )
          ( skipMany, satisfy(function(c){ return c != '\n' }) )
          ( return_, null)


//
//  inCommentSingle
//      =   do{ try (string (commentEnd languageDef)); return () }
//      <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
//      <|> do{ oneOf startEnd                      ; inCommentSingle }
//      <?> "end of comment"
//      where
//        startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

var startEnd = nub( slice( languageDef.commentEnd + languageDef.commentStart ) );

function _inCommentSingle(state, scope, k){ return inCommentSingle(state, scope, k) }

var inCommentSingle
            = [ do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentSingle )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentSingle )
        ,"<?>", "end of comment"]



//  inCommentMulti
//      =   do{ try (string (commentEnd languageDef)) ; return () }
//      <|> do{ multiLineComment                     ; inCommentMulti }
//      <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
//      <|> do{ oneOf startEnd                       ; inCommentMulti }
//      <?> "end of comment"
//      where
//        startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

function _inCommentMulti(state, scope, k){ return inCommentMulti(state, scope, k) }

var inCommentMulti
            = ex( do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( _multiLineComment                     , _inCommentMulti )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentMulti )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentMulti )
        ,"<?>", "end of comment")



//  inComment
//      | nestedComments languageDef  = inCommentMulti
//      | otherwise                = inCommentSingle

var inComment = languageDef.nestedComments ? inCommentMulti : inCommentSingle;


//  multiLineComment =
//      do { try (string (commentStart languageDef))
//         ; inComment
//         }

function _multiLineComment(state, scope, k){ return multiLineComment(state, scope, k) }

var multiLineComment =
        do_( try_ (string (languageDef.commentStart))
           , inComment);


//  whiteSpace
//      | noLine && noMulti  = skipMany (simpleSpace <?> "")
//      | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
//      | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
//      | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
//      where
//        noLine  = null (commentLine languageDef)
//        noMulti = null (commentStart languageDef)

var noLine   = null_(languageDef.commentLine);
var noMulti  = null_(languageDef.commentStart);

var whiteSpace = (
    (noLine && noMulti) ? [skipMany, [simpleSpace ,"<?>", ""]] :
    noLine              ? [skipMany, [simpleSpace ,"<|>", multiLineComment ,"<?>", ""]] :
    noMulti             ? [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<?>", ""]] :
                          [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<|>", multiLineComment ,"<?>", ""]]
    ).resolve();




//  -----------------------------------------------------------
//  -- Bracketing
//  -----------------------------------------------------------
//  parens p        = between (symbol "(") (symbol ")") p
//  braces p        = between (symbol "{") (symbol "}") p
//  angles p        = between (symbol "<") (symbol ">") p
//  brackets p      = between (symbol "[") (symbol "]") p
//
//  semi            = symbol ";"
//  comma           = symbol ","
//  dot             = symbol "."
//  colon           = symbol ":"
//
//  commaSep p      = sepBy p comma
//  semiSep p       = sepBy p semi
//
//  commaSep1 p     = sepBy1 p comma
//  semiSep1 p      = sepBy1 p semi



function parens(p){
    return between(symbol("("), symbol(")"), p);
}
function braces(p){
    return between(symbol("{"), symbol("}"), p);
}
function angles(p){
    return between(symbol("<"), symbol(">"), p);
}
function brackets(p){
    return between(symbol("["), symbol("]"), p);
}

var semi  = symbol(";");
var comma = symbol(",");
var dot   = symbol(".");
var colon = symbol(":");

function commaSep(p){
    return sepBy(p, comma);
}
function semiSep(p){
    return sepBy(p, semi);
}

function commaSep1(p){
    return sepBy1(p, comma);
}
function semiSep1(p){
    return sepBy1(p, semi);
}



//  -----------------------------------------------------------
//  -- Chars & Strings
//  -----------------------------------------------------------


var ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"];

var ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"];


var ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP'];

var ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL'];


//  -- escape code tables
//  escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
//  asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

var escMap          = zip("abfnrtv\\\"\'", "\a\b\f\n\r\t\v\\\"\'");
var asciiMap        = zip((ascii3codes + ascii2codes), (ascii3 + ascii2));

//
//  charEsc         = choice (map parseEsc escMap)
//                  where
//                    parseEsc (c,code)     = do{ char c; return code }

var charEsc         = choice(map(parseEsc, escMap));
                    
function parseEsc(tuple){
    return do_( char_(tuple[0]), return_(tuple[1]) );
}


//  charAscii       = choice (map parseAscii asciiMap)
//                  where
//                    parseAscii (asc,code) = try (do{ string asc; return code })

var charAscii       = choice(map(parseAscii, asciiMap));

function parseAscii(tuple){
    return try_(do_( string(tuple[0]), return_(tuple[1]) ));
}


//  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

var stringLetter    = satisfy(function(c){ return (c != '"') && (c != '\\') && (c > '\026') }); //TODO: last expr.


//  escapeEmpty     = char '&'

var escapeEmpty     = char_('&');


//  escapeGap       = do{ many1 space
//                      ; char '\\' <?> "end of string gap"
//                      }

var escapeGap       = cs( many1, space )
                        ( char_('\\') ,"<?>", "end of string gap")
                        

//  charNum         = do{ code <- decimal
//                                <|> do{ char 'o'; number 8 octDigit }
//                                <|> do{ char 'x'; number 16 hexDigit }
//                      ; return (toEnum (fromInteger code))
//                      }

var charNum         = cs( "code" ,"<-", _decimal
                                      ,"<|>", do_( char_('o'), number(8, octDigit) )
                                      ,"<|>", do_( char_('x'), number(16, hexDigit) )
                        )
                        ( ret(function(scope){ return toEnum(fromInteger(scope.code)) }) )


//  charControl     = do{ char '^'
//                      ; code <- upper
//                      ; return (toEnum (fromEnum code - fromEnum 'A'))
//                      }

var charControl     = cs( char_('^') )
                        ( "code" ,"<-", upper )
                        ( ret(function(scope){ return toEnum(fromEnum(scope.code) - fromEnum('A'))  }) )
 

//  -- escape codes
//  escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
//                  <?> "escape code"

var escapeCode      = ex(charEsc ,"<|>", charNum ,"<|>", charAscii ,"<|>", charControl
                    ,"<?>", "escape code")


//  charEscape      = do{ char '\\'; escapeCode }

var charEscape        = do_(char_('\\'), escapeCode);



//  charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

var charLetter      = satisfy(function(c){ return (c != '\'') && (c != '\\') && (c > '\026') }); //TODO: last expr.


//
//  characterChar   = charLetter <|> charEscape
//                  <?> "literal character"

var characterChar   = ex(charLetter ,"<|>", charEscape
                    ,"<?>", "literal character")


//  charLiteral     = lexeme (between (char '\'')
//                                    (char '\'' <?> "end of character")
//                                    characterChar )
//                  <?> "character"

var charLiteral     = ex(lexeme, [between, char_('\''), 
                                    [char_('\'') ,"<?>", "end of character"],
                                    characterChar]
                    ,"<?>", "character");


//
//  stringEscape    = do{ char '\\'
//                      ;     do{ escapeGap  ; return Nothing }
//                        <|> do{ escapeEmpty; return Nothing }
//                        <|> do{ esc <- escapeCode; return (Just esc) }
//                      }

var stringEscape    = cs( char_('\\') )
                        (         cs( escapeGap   ) ( return_, Maybe.Nothing )
                          ,"<|>", cs( escapeEmpty ) ( return_, Maybe.Nothing )
                          ,"<|>", cs( "esc" ,"<-", escapeCode) ( returnCall, Maybe.Just, "esc" )
                        )


//  stringChar      =   do{ c <- stringLetter; return (Just c) }
//                  <|> stringEscape
//                  <?> "string character"

var stringChar      = ex(cs( "c" ,"<-", stringLetter )
                         ( returnCall, Maybe.Just, "c" )
                      ,"<|>", stringEscape
                      ,"<?>", "string character");


//  stringLiteral   = lexeme (
//                    do{ str <- between (char '"')
//                                       (char '"' <?> "end of string")
//                                       (many stringChar)
//                      ; return (foldr (maybe id (:)) "" str)
//                      }
//                    <?> "literal string")

var stringLiteral   = ex(lexeme,
                          [ cs( "str", "<-", between, char_('"'),
                                                      [char_('"') ,"<?>", "end of string"],
                                                      [many, stringChar]
                               )
                               (ret, function(scope){ return foldr(curry(maybe)(id, curry(cons)), "", scope.str) })
                          ,"<?>", "literal string"]
                      );



//  -----------------------------------------------------------
//  -- Numbers
//  -----------------------------------------------------------


//  number base baseDigit
//      = do{ digits <- many1 baseDigit
//          ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
//          ; seq n (return n)
//          }

function number(base, baseDigit){ 
    return cs( "digits" ,"<-", many1, baseDigit )
             ( ret, function(scope){
                        return foldl(function(x, d){
                                  return base * x + toInteger(digitToInt(d));
                              }, 0, scope.digits);
             })
}


//  decimal         = number 10 digit
//  hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
//  octal           = do{ oneOf "oO"; number 8 octDigit  }

function _decimal(state, scope, k){ return decimal(state, scope, k) }

var decimal         = number(10, digit);
var hexadecimal     = cs( oneOf, "xX" ) ( number, 16, hexDigit )
var octal           = cs( oneOf, "oO" ) ( number, 8, octDigit  )



//
//  fraction        = do{ char '.'
//                      ; digits <- many1 digit <?> "fraction"
//                      ; return (foldr op 0.0 digits)
//                      }
//                    <?> "fraction"
//                  where
//                    op d f    = (f + fromIntegral (digitToInt d))/10.0

function _op(d, f){
    return (f + fromIntegral(digitToInt(d))) / 10.0;
}

var fraction        = ex( cs( char_('.'))
                            ( "digits" ,"<-", many1, digit ,"<?>", "fraction")
                            ( ret, function(scope){ return foldr(_op, 0.0, scope.digits) })
                        ,"<?>", "fraction")



//
//  sign            =   (char '-' >> return negate)
//                  <|> (char '+' >> return id)
//                  <|> return id

var sign            = ex([char_('-') ,">>", return_, negate]
                            ,"<|>", [char_('+') ,">>", return_, id]
                            ,"<|>", return_, id);


//
//  exponent'       = do{ oneOf "eE"
//                      ; f <- sign
//                      ; e <- decimal <?> "exponent"
//                      ; return (power (f e))
//                      }
//                    <?> "exponent"
//                  where
//                     power e  | e < 0      = 1.0/power(-e)
//                              | otherwise  = fromInteger (10^e)

function power(e){
    return (e < 0) ?  1.0 / power(-e) :  fromInteger(Math.pow(10,e));
}

var exponent_       = ex( cs( oneOf, "eE" )
                            ( "f" ,"<-", sign )
                            ( "e" ,"<-", decimal ,"<?>", "exponent" )
                            ( returnCall, power, "f", "e")
                        ,"<?>", "exponent");



//  fractExponent n = do{ fract <- fraction
//                      ; expo  <- option 1.0 exponent'
//                      ; return ((fromInteger n + fract)*expo)
//                      }
//                  <|>
//                    do{ expo <- exponent'
//                      ; return ((fromInteger n)*expo)
//                      }

function fractExponent(n){
    return ex(
          cs( "fract" ,"<-", fraction )
            ( "expo"  ,"<-", option, 1.0, exponent_ )
            ( ret, function(scope){ return fromInteger(n + scope.fract) * scope.expo })
        ,"<|>",
          cs( "expo", "<-", exponent_ )
            ( ret, function(scope){ return fromInteger(n) * scope.expo })
    );
}

//  -- floats
//  floating        = do{ n <- decimal
//                      ; fractExponent n
//                      }

var floating        = cs( "n" ,"<-", decimal)
                        ( function(state, scope, k){ return fractExponent(scope.n)(state, scope, k) })


//  fractFloat n    = do{ f <- fractExponent n
//                      ; return (Right f)
//                      }

function fractFloat(n){
    return cs( "f" ,"<-", fractExponent, n)
             ( returnCall, Either.Right, "f")
}


//
//  decimalFloat    = do{ n <- decimal
//                      ; option (Left n)
//                               (fractFloat n)
//                      }

var decimalFloat    = cs( "n" ,"<-", decimal )
                        ( function(state, scope, k){ 
                               return option(Either.Left(scope.n), fractFloat(scope.n))(state, scope, k);
                        })


//  zeroNumFloat    =  do{ n <- hexadecimal <|> octal
//                       ; return (Left n)
//                       }
//                  <|> decimalFloat
//                  <|> fractFloat 0
//                  <|> return (Left 0)


var zeroNumFloat    = ex(cs( "n" ,"<-", hexadecimal ,"<|>", octal )
                           ( returnCall, Either.Left, "n" )
                       ,"<|>", decimalFloat
                       ,"<|>", fractFloat(0)
                       ,"<|>", return_, Either.Left(0)
                       );




//  natFloat        = do{ char '0'
//                      ; zeroNumFloat
//                      }
//                    <|> decimalFloat

var natFloat        = ex(do_( char_('0'),
                            zeroNumFloat
                            )
                      ,"<|>", decimalFloat);



//  zeroNumber      = do{ char '0'
//                      ; hexadecimal <|> octal <|> decimal <|> return 0
//                      }
//                    <?> ""

var zeroNumber      = ex( cs( char_, '0')
                            ( hexadecimal ,"<|>", octal ,"<|>", decimal ,"<|>", return_, 0 )
                      ,"<?>", "");


//  nat             = zeroNumber <|> decimal

var nat             = parserPlus(zeroNumber, decimal);

//  -- integers and naturals
//  int             = do{ f <- lexeme sign
//                      ; n <- nat
//                      ; return (f n)
//                      }

var int_            = cs( "f" ,"<-", lexeme, sign )
                        ( "n" ,"<-", nat )
                        ( ret, function(scope){ return scope.f(scope.n) })



//  naturalOrFloat  = lexeme (natFloat) <?> "number"
//
//  float           = lexeme floating   <?> "float"
//  integer         = lexeme int        <?> "integer"
//  natural         = lexeme nat        <?> "natural"

var naturalOrFloat  = [lexeme, natFloat   ,"<?>", "number" ].resolve();

var float_          = [lexeme, floating   ,"<?>", "float"  ].resolve();
var integer         = [lexeme, int_       ,"<?>", "integer"].resolve();
var natural         = [lexeme, nat        ,"<?>", "natural"].resolve();




//  -----------------------------------------------------------
//  -- Operators & reserved ops
//  -----------------------------------------------------------


//  reservedOp name =
//      lexeme $ try $
//      do{ string name
//        ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
//        }

function reservedOp(name){
    return ex(lexeme ,"$", try_ ,"$",
                cs( string(name) ) 
                  ( notFollowedBy, languageDef.opLetter ,"<?>", "end of " + name )
            );
}


//  oper =
//      do{ c <- (opStart languageDef)
//        ; cs <- many (opLetter languageDef)
//        ; return (c:cs)
//        }
//      <?> "operator"

var oper =
        ex(cs( "c"  ,"<-", languageDef.opStart )
             ( "cs" ,"<-", many, languageDef.opLetter )
             ( returnCall, consJoin, "c", "cs" )
         ,"<?>", "operator");


//  isReservedOp name =
//      isReserved (sort (reservedOpNames languageDef)) name

function isReservedOp(name){
        return isReserved( sort( languageDef.reservedOpNames ), name);
}


//  operator =
//      lexeme $ try $
//      do{ name <- oper
//        ; if (isReservedOp name)
//           then unexpected ("reserved operator " ++ show name)
//           else return name
//        }

var operator =
        ex(lexeme ,"$", try_ ,"$",
            cs( "name" ,"<-", oper )
            ( function(state, scope, k){
                    return (isReservedOp(scope.name) ? 
                        unexpected("reserved operator " + scope.name) : return_(scope.name) )(state, scope, k);
          }));





//  -----------------------------------------------------------
//  -- Identifiers & Reserved words
//  -----------------------------------------------------------


//  caseString name
//      | caseSensitive languageDef  = string name
//      | otherwise               = do{ walk name; return name }
//      where
//        walk []     = return ()
//        walk (c:cs) = do{ caseChar c <?> msg; walk cs }
//
//        caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
//                    | otherwise  = char c
//
//        msg         = show name

function caseString(name){

    function walk(cs){
        return (!cs.length) ? return_(null) :
                       do_( label(caseChar(cs[0]), "" + name),
                            walk(slice(cs, 1)) );
    }

    function caseChar(c){
        return isAlpha(c) ? parserPlus( char_(c.toLowerCase()),
                                        char_(c.toUpperCase())) : 
                            char_(c);
    }

    return languageDef.caseSensitive ? string(name) : do_( walk(name), return_(name) );

}

//  reserved name =
//      lexeme $ try $
//      do{ caseString name
//        ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
//        }

function reserved(name){
    return ex(lexeme ,"$", try_ ,"$",
              cs( caseString(name) )
                ( notFollowedBy, languageDef.identLetter ,"<?>", "end of " + name )
            );
}


//  ident
//      = do{ c <- identStart languageDef
//          ; cs <- many (identLetter languageDef)
//          ; return (c:cs)
//          }
//      <?> "identifier"

var ident
        = ex( cs( "c"  ,"<-", languageDef.identStart )
                ( "cs" ,"<-", many, languageDef.identLetter )
                ( returnCall, consJoin, "c", "cs" )
           ,"<?>", "identifier");


//  isReserved names name
//      = scan names
//      where
//        scan []       = False
//        scan (r:rs)   = case (compare r name) of
//                          LT  -> scan rs
//                          EQ  -> True
//                          GT  -> False

function isReserved(names, name){
    function scan(rs){
        if(!rs.length) 
            return false;

        var ord = compare(rs[0], name);

        return  ord.LT ? scan(slice(rs, 1)) :
                ord.EQ ? true : 
                ord.GT ? false : null;
    }

    return scan(names);
}


//  isReservedName name
//      = isReserved theReservedNames caseName
//      where
//        caseName      | caseSensitive languageDef  = name
//                      | otherwise               = map toLower name

function isReservedName(name){
    var caseName = languageDef.caseSensitive ? name : name.toLowerCase();

    return isReserved(theReservedNames, caseName);
}


//  identifier =
//      lexeme $ try $
//      do{ name <- ident
//        ; if (isReservedName name)
//           then unexpected ("reserved word " ++ show name)
//           else return name
//        }

var identifier =
        ex(lexeme ,"$", try_ ,"$",
            cs( "name" ,"<-", ident )
              ( function(state, scope, k){
                    return ( isReservedName(scope.name) ? 
                                unexpected("reserved word " + scope.name) : 
                                return_(scope.name)
                            )(state, scope, k);
              })
        );


//  theReservedNames
//      | caseSensitive languageDef  = sortedNames
//      | otherwise               = map (map toLower) sortedNames
//      where
//        sortedNames   = sort (reservedNames languageDef)

var sortedNames = sort(languageDef.reservedNames);
var theReservedNames = languageDef.caseSensitive ? 
                            sortedNames : 
                            map( function(str){ return str.toLowerCase() }, sortedNames );



    return GenTokenParser.TokenParser(record, {
        identifier : identifier,
        reserved   : reserved,
        operator   : operator,
        reservedOp : reservedOp,
        
        charLiteral    : charLiteral,
        stringLiteral  : stringLiteral,
        natural        : natural,
        integer        : integer,
        float_         : float_,
        naturalOrFloat : naturalOrFloat,
        decimal        : decimal,
        hexadecimal    : hexadecimal,
        octal          : octal,
        
        symbol     : symbol,
        lexeme     : lexeme,
        whiteSpace : whiteSpace,
        
        parens     : parens,
        braces     : braces,
        angles     : angles,
        brackets   : brackets,
        squares    : brackets,
        semi       : semi,
        comma      : comma,
        colon      : colon,
        dot        : dot,
        semiSep    : semiSep,
        semiSep1   : semiSep1,
        commaSep   : commaSep,
        commaSep1  : commaSep1
    });
}


extend(JSParsec, {
    GenLanguageDef  : GenLanguageDef,
    GenTokenParser  : GenTokenParser,
    makeTokenParser : makeTokenParser
});

// -------------------------------------------------
// Language
// -------------------------------------------------


//-- A helper module that defines some language definitions that can be used
//-- to instantiate a token parser (see "Text.Parsec.Token").

//module Text.Parsec.Language
//    ( haskellDef, haskell
//    , mondrianDef, mondrian
//    , emptyDef
//    , haskellStyle
//    , javaStyle
//    , LanguageDef
//    , GenLanguageDef
//    ) where
//
//import Text.Parsec
//import Text.Parsec.Token


//-----------------------------------------------------------
//-- minimal language definition
//--------------------------------------------------------
//
//-- TODO: This seems wrong
//-- < This is the most minimal token definition. It is recommended to use
//-- this definition as the basis for other definitions. @emptyDef@ has
//-- no reserved names or operators, is case sensitive and doesn't accept
//-- comments, identifiers or operators.
//
//emptyDef   :: LanguageDef st
//emptyDef    = LanguageDef
//               { commentStart   = ""
//               , commentEnd     = ""
//               , commentLine    = ""
//               , nestedComments = True
//               , identStart     = letter <|> char '_'
//               , identLetter    = alphaNum <|> oneOf "_'"
//               , opStart        = opLetter emptyDef
//               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
//               , reservedOpNames= []
//               , reservedNames  = []
//               , caseSensitive  = True
//               }

var emptyDefOpLetter = oneOf(":!#$%&*+./<=>?@\\^|-~");

var emptyDef = GenLanguageDef.LanguageDef(record,
               { commentStart   : ""
               , commentEnd     : ""
               , commentLine    : ""
               , nestedComments : true
               , identStart     : [letter   ,"<|>", char_('_')].resolve()
               , identLetter    : [alphaNum ,"<|>", oneOf("_'")].resolve()
               , opStart        : emptyDefOpLetter
               , opLetter       : emptyDefOpLetter
               , reservedOpNames: []
               , reservedNames  : []
               , caseSensitive  : true
               });


//-----------------------------------------------------------
//-- Styles: haskellStyle, javaStyle
//-----------------------------------------------------------
//
//-- | This is a minimal token definition for Haskell style languages. It
//-- defines the style of comments, valid identifiers and case
//-- sensitivity. It does not define any reserved words or operators.
//
//haskellStyle :: LanguageDef st
//haskellStyle = emptyDef
//                { commentStart   = "{-"
//                , commentEnd     = "-}"
//                , commentLine    = "--"
//                , nestedComments = True
//                , identStart     = letter
//                , identLetter    = alphaNum <|> oneOf "_'"
//                , opStart        = opLetter haskellStyle
//                , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
//                , reservedOpNames= []
//                , reservedNames  = []
//                , caseSensitive  = True
//                }

var haskellStyle = GenLanguageDef.LanguageDef(record,
               { commentStart   : "{-"
               , commentEnd     : "-}"
               , commentLine    : "--"
               , nestedComments : true
               , identStart     : letter
               , identLetter    : [alphaNum   ,"<|>", oneOf, "_'"].resolve()
               , opStart        : emptyDefOpLetter
               , opLetter       : emptyDefOpLetter
               , reservedOpNames: []
               , reservedNames  : []
               , caseSensitive  : true
               });


//-- | This is a minimal token definition for Java style languages. It
//-- defines the style of comments, valid identifiers and case
//-- sensitivity. It does not define any reserved words or operators.
//
//javaStyle  :: LanguageDef st
//javaStyle   = emptyDef
//      { commentStart   = "/*"
//      , commentEnd     = "*/"
//      , commentLine    = "//"
//      , nestedComments = True
//      , identStart     = letter
//      , identLetter    = alphaNum <|> oneOf "_'"
//      , reservedNames  = []
//      , reservedOpNames= []
//      , caseSensitive  = False
//      }

var javaStyle = GenLanguageDef.LanguageDef(record,
               { commentStart    : "/*"
               , commentEnd      : "*/"
               , commentLine     : "//"
               , nestedComments  : true
               , identStart      : letter
               , identLetter     : [alphaNum   ,"<|>", oneOf, "_'"].resolve()
               , opStart         : emptyDefOpLetter
               , opLetter        : emptyDefOpLetter
               , reservedOpNames : []
               , reservedNames   : []
               , caseSensitive   : false
               });

//-----------------------------------------------------------
//-- Haskell
//-----------------------------------------------------------


//-- | The language definition for the language Haskell98.
//
//haskell98Def :: LanguageDef st
//haskell98Def = haskellStyle
//                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
//                , reservedNames  = ["let","in","case","of","if","then","else",
//                                    "data","type",
//                                    "class","default","deriving","do","import",
//                                    "infix","infixl","infixr","instance","module",
//                                    "newtype","where",
//                                    "primitive"
//                                    -- "as","qualified","hiding"
//                                   ]
//                }

var haskell98Def = haskellStyle.update(
                { reservedOpNames : ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames   : ["let","in","case","of","if","then","else",
                                     "data","type",
                                     "class","default","deriving","do","import",
                                     "infix","infixl","infixr","instance","module",
                                     "newtype","where",
                                     "primitive"
                                   // ,"as","qualified","hiding"
                                    ]
                });



//-- | The language definition for the Haskell language.
//
//haskellDef  :: LanguageDef st
//haskellDef   = haskell98Def
//          { identLetter    = identLetter haskell98Def <|> char '#'
//          , reservedNames  = reservedNames haskell98Def ++
//                     ["foreign","import","export","primitive"
//                     ,"_ccall_","_casm_"
//                     ,"forall"
//                     ]
//                }

var haskellDef = haskell98Def.update(
            { identLetter   : [haskell98Def.identLetter ,"<|>", char_('#')].resolve()
            , reservedNames : haskell98Def.reservedNames.concat(
                                  ["foreign","import","export","primitive"
                                  ,"_ccall_","_casm_"
                                  ,"forall"
                                  ])
            });

//-- | A lexer for the haskell language.
//
//haskell :: TokenParser st
//haskell      = makeTokenParser haskellDef

//var haskell = makeTokenParser(haskellDef);


//-----------------------------------------------------------
//-- Mondrian
//-----------------------------------------------------------

//-- | The language definition for the language Mondrian.
//
//mondrianDef :: LanguageDef st
//mondrianDef = javaStyle
//      { reservedNames = [ "case", "class", "default", "extends"
//                , "import", "in", "let", "new", "of", "package"
//                ]
//                , caseSensitive  = True
//      }


var mondrianDef = javaStyle.update(
        { reservedNames : [ "case", "class", "default", "extends"
                          , "import", "in", "let", "new", "of", "package"
                          ]
        , caseSensitive : true
        });

//-- | A lexer for the mondrian language.
//
//mondrian :: TokenParser st
//mondrian    = makeTokenParser mondrianDef

//var mondrian = makeTokenParser(mondrianDef);



extend(JSParsec, {
    emptyDef    : emptyDef,
    haskellStyle: haskellStyle,
    javaStyle   : javaStyle,
    haskellDef  : haskellDef,
    mondrianDef : mondrianDef,
    getHaskell  : function(){ return makeTokenParser(haskellDef)  },
    getMondrian : function(){ return makeTokenParser(mondrianDef) }
});

//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Text.Parsec.Expr
//-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//-- License     :  BSD-style (see the LICENSE file)
//-- 
//-- Maintainer  :  derek.a.elkins@gmail.com
//-- Stability   :  provisional
//-- Portability :  non-portable
//-- 
//-- A helper module to parse \"expressions\".
//-- Builds a parser given a table of operators and associativities.
//-- 
//-----------------------------------------------------------------------------
//
//module Text.Parsec.Expr
//    ( Assoc(..), Operator(..), OperatorTable
//    , buildExpressionParser
//    ) where
//
//import Text.Parsec.Prim
//import Text.Parsec.Combinator
//
//-----------------------------------------------------------
//-- Assoc and OperatorTable
//-----------------------------------------------------------
//
//-- |  This data type specifies the associativity of operators: left, right
//-- or none.
//
//data Assoc                = AssocNone
//                          | AssocLeft
//                          | AssocRight

function Assoc(){}
data(Assoc, ["AssocNone", "AssocLeft", "AssocRight"]);

//-- | This data type specifies operators that work on values of type @a@.
//-- An operator is either binary infix or unary prefix or postfix. A
//-- binary operator has also an associated associativity.
//
//data Operator s u m a   = Infix (ParsecT s u m (a -> a -> a)) Assoc
//                        | Prefix (ParsecT s u m (a -> a))
//                        | Postfix (ParsecT s u m (a -> a))
function Operator(){}
data(Operator, [
    ["Infix", Parser, Assoc],
    ["Prefix", Parser],
    ["Postfix", Parser]
]);


//-- | An @OperatorTable s u m a@ is a list of @Operator s u m a@
//-- lists. The list is ordered in descending
//-- precedence. All operators in one list have the same precedence (but
//-- may have a different associativity).
//
//type OperatorTable s u m a = [[Operator s u m a]]


//-----------------------------------------------------------
//-- Convert an OperatorTable and basic term parser into
//-- a full fledged expression parser
//-----------------------------------------------------------
//
//-- | @buildExpressionParser table term@ builds an expression parser for
//-- terms @term@ with operators from @table@, taking the associativity
//-- and precedence specified in @table@ into account. Prefix and postfix
//-- operators of the same precedence can only occur once (i.e. @--2@ is
//-- not allowed if @-@ is prefix negate). Prefix and postfix operators
//-- of the same precedence associate to the left (i.e. if @++@ is
//-- postfix increment, than @-2++@ equals @-1@, not @-3@).
//--
//-- The @buildExpressionParser@ takes care of all the complexity
//-- involved in building expression parser. Here is an example of an
//-- expression parser that handles prefix signs, postfix increment and
//-- basic arithmetic.
//--
//-- >  expr    = buildExpressionParser table term
//-- >          <?> "expression"
//-- >
//-- >  term    =  parens expr 
//-- >          <|> natural
//-- >          <?> "simple expression"
//-- >
//-- >  table   = [ [prefix "-" negate, prefix "+" id ]
//-- >            , [postfix "++" (+1)]
//-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
//-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
//-- >            ]
//-- >          
//-- >  binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
//-- >  prefix  name fun       = Prefix (do{ reservedOp name; return fun })
//-- >  postfix name fun       = Postfix (do{ reservedOp name; return fun })
//
//buildExpressionParser :: (Stream s m t)
//                      => OperatorTable s u m a
//                      -> ParsecT s u m a
//                      -> ParsecT s u m a
//buildExpressionParser operators simpleExpr = ...
function buildExpressionParser(operators, simpleExpr){
    
    function hook(fn, ident){
        return function(state, scope, k){
            return fn(scope[ident])(state, scope, k);
        };
    }
    
    function splitOp(oper, tuple){
        
        var op = oper[0];
        var rassoc  = tuple[0],
            lassoc  = tuple[1],
            nassoc  = tuple[2],
            prefix  = tuple[3],
            postfix = tuple[4];
        
//      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
//        = case assoc of
//            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
//            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
//            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)
        if(oper.Infix){
            var assoc = oper[1];
            if(assoc.AssocNone)
                return [rassoc, lassoc, cons(op, nassoc), prefix, postfix];
            if(assoc.AssocLeft)
                return [rassoc, cons(op, lassoc), nassoc, prefix, postfix];
            if(assoc.AssocRight)
                return [cons(op, rassoc), lassoc, nassoc, prefix, postfix];
        }

//      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
//        = (rassoc,lassoc,nassoc,op:prefix,postfix)
        if(oper.Prefix)
            return [rassoc, lassoc, nassoc, cons(op, prefix), postfix];
        
//      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
//        = (rassoc,lassoc,nassoc,prefix,op:postfix)
        if(oper.Postfix)
            return [rassoc, lassoc, nassoc, prefix, cons(op, postfix)];
    }
    
    
//              ambigious assoc op = try $
//                                  do{ op; fail ("ambiguous use of a " ++ assoc
//                                                 ++ " associative operator")
//                                    }
    function ambigious(assoc, op){
        return try_(do_(op, fail("ambiguous use of a " + assoc + " associative operator")));
    }


    function makeParser(term, ops){
        
        var tuple = foldr(splitOp, [[],[],[],[],[]], ops);
        
        var rassoc  = tuple[0],
            lassoc  = tuple[1],
            nassoc  = tuple[2],
            prefix  = tuple[3],
            postfix = tuple[4];
            
        var rassocOp   = choice(rassoc),
            lassocOp   = choice(lassoc),
            nassocOp   = choice(nassoc),
            prefixOp   = label(choice(prefix) , ""),
            postfixOp  = label(choice(postfix), "");
            
        var ambigiousRight = ambigious("right", rassocOp),
            ambigiousLeft  = ambigious("left" , lassocOp),
            ambigiousNon   = ambigious("non"  , nassocOp);
            
        var postfixP  = parserPlus(postfixOp, return_(id)),
            prefixP   = parserPlus(prefixOp , return_(id));
            
//              termP      = do{ pre  <- prefixP
//                             ; x    <- term
//                             ; post <- postfixP
//                             ; return (post (pre x))
//                             }
        var termP = cs
            ("pre"  ,"<-", prefixP)
            ("x"    ,"<-", term)
            ("post" ,"<-", postfixP)
            (ret, function(scope){ return scope.post(scope.pre(scope.x)) })
        
        
//              rassocP x  = do{ f <- rassocOp
//                             ; y  <- do{ z <- termP; rassocP1 z }
//                             ; return (f x y)
//                             }
//                           <|> ambigiousLeft
//                           <|> ambigiousNon
//                           -- <|> return x
        function rassocP(x){
            return ex(cs("f"  ,"<-", rassocOp)
                        ("y"  ,"<-", cs("z"  ,"<-", termP)
                                       (hook, rassocP1, "z")
                        )
                        (ret, function(scope){ return scope.f(x, scope.y) })
                    ,"<|>", ambigiousLeft
                    ,"<|>", ambigiousNon
                    //,"<|>", return_, x
                    );
        }
        
//              rassocP1 x = rassocP x  <|> return x
        function rassocP1(x){
            return parserPlus(rassocP(x), return_(x));
        }
        
//              lassocP x  = do{ f <- lassocOp
//                             ; y <- termP
//                             ; lassocP1 (f x y)
//                             }
//                           <|> ambigiousRight
//                           <|> ambigiousNon
//                           -- <|> return x
        function lassocP(x){
            return ex(cs("f"  ,"<-", lassocOp)
                        ("y"  ,"<-", termP)
                        (function(state, scope, k){
                            return lassocP1(scope.f(x, scope.y))(state, scope, k);
                        })
                        ,"<|>", ambigiousRight
                        ,"<|>", ambigiousNon
                        //,"<|>", return_, x
                    );
        }
        
//              lassocP1 x = lassocP x <|> return x
        function lassocP1(x){
            return parserPlus(lassocP(x), return_(x));
        }

//              nassocP x  = do{ f <- nassocOp
//                             ; y <- termP
//                             ;    ambigiousRight
//                              <|> ambigiousLeft
//                              <|> ambigiousNon
//                              <|> return (f x y)
//                             }
//                           -- <|> return x
        function nassocP(x){
            return cs
                ("f"  ,"<-", nassocOp)
                ("y"  ,"<-", termP)
                (       ambigiousRight
                ,"<|>", ambigiousLeft
                ,"<|>", ambigiousNon
                ,"<|>", ret, function(scope){ return scope.f(x, scope.y) }
                )
        }
        
//           in  do{ x <- termP
//                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
//                   <?> "operator"
//                 }
        return cs("x" ,"<-", termP)
                 (hook, rassocP, "x"
                  ,"<|>", hook, lassocP, "x"
                  ,"<|>", hook, nassocP, "x"
                  ,"<|>", ret, "x"
                  ,"<?>", "operator").resolve();
    }
        
//  buildExpressionParser operators simpleExpr
//       = foldl (makeParser) simpleExpr operators
    return foldl(makeParser, simpleExpr, operators);
}


extend(JSParsec, {
    Assoc: Assoc,
    Operator: Operator,
    buildExpressionParser: buildExpressionParser
});


})();