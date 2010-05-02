/*! 
 * JSParsec - A parser combinator library for JavaScript
 * 
 * Version: 0.0.4
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
		             Ordering.GT
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
};

//returns True if a list is empty, otherwise False
function null_(a){
	return !a.length;
};


function elem(x, xs){
	return (xs.indexOf ? xs.indexOf(x) : indexOf(xs, x)) != -1
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

// -------------------------------------------------
// Algebraic Data Types
// -------------------------------------------------

function Record(){}
var record = new Record;


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
		i++
	}
    return -1;  
}


function ADT(){}

function adtToString(type){
	return function(){
		var acc=[], rec = this._recordset;
		if(!isArray(rec)){
			for(var name in rec){
				var item = (type ? (rec[name].name || rec[name]) : this[name]);
				if(!type && (item instanceof Function))
					item = item.constructor != Function ? item.constructor.name : "Function(" + item.name + ")";
				acc.push(name + " :: " + item );
			}
			var indent = replicate(this._dataConstructor.length + 2," ").join("");
			acc = "{" + acc.join("\n" + indent + ",") + "\n" + indent +"}";
		}else{
			for(var i = 0; i in this; i++)
			acc.push(type ? (rec[i].name || rec[i]) : this[i]);
			acc = acc.join(" ");
		}
		return  this._dataConstructor + " " + acc;
	}
}

ADT.prototype.toString = adtToString();

ADT.prototype.dataConstructorToString = adtToString(true);


function data(type, constr){
	if(type.constructors)
		throw "Type constructor has been already defined: '" + type.name + "'";
	type.constructors = constr;

	type.prototype = new ADT;

	for(var i = 0, l = constr.length; i < l; ++i){
		var single = typeof constr[i] != "object",
			name =  single  ? constr[i] : constr[i][0];
		if(name in {})
			throw "The name of the data constructor can't be a property of Object.prototype as well!";

		type[name] = single ? value(name)() : value(name, slice(constr[i], 1));
		if(!single)
			type[name]._length = slice(constr[i], 1).length
	}

	function value(constr, fields){
		var recordDef = fields && typeof fields[0] == "object";
		function create(_isrecord, rec){
			var isrecord = (_isrecord instanceof Record),
				args = isrecord ? rec : slice(arguments),
				that = new type,
				i = 0;
			
			that.constructor = type;
			that._recordset = (recordDef && fields[0]) || fields;
			that._dataConstructor = constr;

			that.update = function(newRecords){
				var obj = {};
				for(var n in fields[0]){
					obj[n] = this[n]
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
							throw "Type mismatch: expecting '" + check.name + "' instead of '" + arg.constructor.name +"' in the argument '" + (recName || i) + "' of the data constructor '" + constr + "' of type '" + type.name +"'"

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
data(Ordering, ["LT", "EQ", "GT"])

function Either(){}
data(Either, [["Left", "a"], ["Right", "b"]])

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
		func:	call,
		fixity: infixr(0)
		//,type:	[Function, "*", "*"]
	},
	"." : {
		func:	compose1,
		fixity: infixr(9)
		//,type:	[Function, Function, Function]
	},
	":" : {
		func:	cons,
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
		op = operators[args[i]].func
	
	var	item = op(args[i-1], args[i+1]);
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
	var str = new String(s);
	str._String = true;
	return str;
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
				(e && e.CallStream) ? e.resolve() : e;
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
			if(fna.length> 1)
				fn = fna[0].apply(null, fna.slice(1));
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
	var	dir    = map(getFixityDir , args),
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


// -------------------------------------------------
// Callstream interface for the do notation
// -------------------------------------------------

function Recurse(){}

var recurse = new Recurse();

function cs(){

	function rec(s){return p(s)}

	var lines = [], p, resolved;

	lines.push(resolve(arguments, rec));

	function line(s){
		if(s instanceof ParseState)
			return (resolved ? p : line.resolve())(s);
			
		lines.push(resolve(arguments, rec));
		return line;
	}

	line.resolve = function(){
		if(resolved)
			return p;
		p = do_.apply(null, lines);
		lines = null;
		resolved = true;
		return p;
	}

	line.CallStream = true;

	return line;
}



// -------------------------------------------------
// ParseState
// -------------------------------------------------

function ParseState(input, index) {
    this.input  = input;
    this.index  = index || 0;
    this.length = input.length - this.index;
    this.cache  = { };
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

	putCached: function(pid, cached) {
		if(!this.memoize)
			return false;
		
		//cached.remaining === this
		cached.index  = this.index;
		cached.length = this.length;


		var p = this.cache[pid];
		if(!p)
			p = this.cache[pid] = { };

		p[this.index - cached.matched.length] = cached;
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


function make_result(remaining, matched, ast, success, expecting){
	success = success === undef ? true : success;
	return { remaining: remaining, matched: matched, ast: ast, 
				success: success, expecting: expecting };
}

var EmptyOk = function(state){
	return make_result(state, "", undef);
}

function _fail(state, expecting){
	return make_result(state, "", undef, false, expecting);
}


//accepts an identifier string, see usage with notFollowedBy
function unexpected(name){ return function(state, scope){
	return make_result(state, "", null, false, {unexpected: scope[name]});
}}

function parserFail(msg){ return function(state){
	return make_result(state, "", undef, false, msg);
}};

var fail = parserFail;


function parserZero(state){
	return make_result(state, "", undef, false);
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

var trampolineCount = 0;

function trampolineAsync(x) {
	trampolineCount++;
	
	if(!(x && x.func)){
		trampolineCount = 0;
		return;
	}

	x = x.func.apply(null, x.args || []);
	
	if(trampolineCount % 500 == 0 )
		setTimeout(function(){ trampoline2(x) }, 1);
	else
		trampoline2(x);
}

function run(p, strOrState, complete, error, async){
	(async ? trampolineAsync : trampoline) ({func:p, args:[strOrState instanceof ParseState ? strOrState : ps(strOrState), {}, function(result){
		if(!result.success){
			result.error = processError(result.expecting, result.remaining);
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
			lindex = index - lines.splice(0,line-1).join("\n").length;
		return 'Unexpected "' + (unexp || s.input.substr(index, e.length).substr(0, 6)) +  
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

function _make(fn, show, p1, p2, pN, action){
	return function(p, opt1){
		var pid = parser_id++;
		p = pN ? map(toParser, arguments) : p;
		p = p1 ? toParser(p) : p;
		opt1 = p2 ? toParser(opt1) : opt1;
		p = action ? curry(p) : p;

		var ret = function(state, scope) {
			var result = state.getCached(pid);
			if(result !== undef)
				return result;
			result = fn(state, scope,
						p1 ? (p.length ? p : p()) : p,
						p2 ? (opt1.length ? opt1 : opt1()) : opt1);

			state.putCached(pid, result);

			return result;
		}
		ret.constructor = Parser;
		//ret.show = show;
		return ret;
	}
}


//true values apply toParser to the nth argument of the function
var make       = function(fn, show){return _make(fn, show)};
var make1P     = function(fn, show){return _make(fn, show, true)};
var make2P     = function(fn, show){return _make(fn, show, true, true)};
//apply toParser to all:
var makeNP     = function(fn, show){return _make(fn, show, false, false, true)}; 
//curries the first arg:
var makeAction = function(fn, show){return _make(fn, show, false, true, false, true)};


function parserBind(p,f){ 
	return function(state, scope){ return f(p(state, scope)) }
}


var do2 = function(p1, p2){
	return function(state, scope, k){
		return { func: p1, args: [state, scope, function(result){
			return result.success ? p2(state, scope, k) : k(result);
		}]};
}};

var do_ = makeNP(function(state, _scope, k, parsers){
		var scope = {},
			i = 1,
			l = parsers.length,
			result = parsers[0];
		
		scope.scope = _scope;

		for(; i < l; ++i)
			result = tdo2(result, parsers[i]);

		return result(state, scope, k);
	});


function bind(name, p){ 
	if(name == "scope")
		throw "Can't use 'scope' as an identifier!";
	return function(state, scope, k){
		return { func: p, args: [state, scope, function(result){
			if(result.success)
				scope[name] = result.ast;
			return k(result);
		}]};
	}
};


function ret(name, more){
	var args;
	if(more) 
		args = slice(arguments);

	return function(state, scope, k){

		return { func: function(){
			var ast, type = typeof name;
			//if(args){
			//	ast =  resolve(resolveBindings(args, scope));
			//}else 
			if(type == "string"){
				if(!(name in scope))
					throw 'Not in scope: "' + name + '"';
				ast = scope[name];		
			}else
				ast = name(scope);

			return k(make_result(state, "", ast));

		}};
	}
}

function resolveBindings(arr, scope){
	return isArray(arr) ?
		map(function(e){ return (e in scope) ? scope[e] : resolveBindings(e) }, arr)
		: arr;
}

function withBound(fn){
	var args = slice(arguments, 1)
	return function(scope){
		return fn.apply(null, map(function(e){ return scope[e] }, args));
	}
}

var returnCall = compose(ret, withBound);

function getParserState(state){
	return make_result(state, "", state.index);
}

function setParserState(id){ return function(state, scope){
	state.scrollTo(scope[id]);
	return EmptyOk(state);
}}

//in contrast with Haskell here's no closure in the do_ notation,
//it's simulated with `bind` and `ret`,
//this function does what `pure` and `return` do in Haskell
function parserReturn(value){ return function(state, scope, k){
	return {func: function(){ return k(make_result(state, "", value)); }}
}}

var return_ = parserReturn;
var pure = return_;


//executes two parsers in a sequence 
//and applies the ast of the first to the ast of the second
//the ast of the first must be a function
function ap(a, b){
	return fmap(function(ast){ return ast[0](ast[1]) }, tokens(a, b));
}

// Parser combinator that passes the AST generated from the parser 'p' 
// to the function 'f'. The result of 'f' is used as the AST in the result.
// the function 'f' will be curried automatically
var parsecMap = makeAction(function(state, scope, k, f, p){
		return {func:p, args:[state, scope, function(result){
				if(!result.success)
					return k(result);
				result = extend({}, result);
				result.ast = f(result.ast);
				return k(result);
		}]};
	});


var fmap = parsecMap;
var liftM = fmap;
var liftA = liftM;
var liftA2 = function(f, a, b){ return ap(fmap(f, a), b) };
var liftA3 = function(f, a, b, c){ return ap(ap(fmap(f, a), b), c) };


// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    return fmap(function(ast) { return ast.join(sep); }, p);
}

//var skip_fst = function(p1, p2){ return liftA2(const_(id), p1, p2) };
function skip_fst(p1, p2){ return do_(p1, p2) }

//var skip_snd = function(p1, p2){ return liftA2(const_, p1, p2) };
function skip_snd(p1, p2){ return do_(bind("a", p1), p2, ret("a")) }



var parserPlus2 = function(p1, p2){
	return function(state, scope, k){
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
			
			return (result.ast !== undefined) ? {func:k, args: [result]} :
				{func: p2, args: [state, scope, function(result){
					handleError(result);
					return k(result);
				}]}
		}]};
	}
}

// 'parserPlus' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that matches some string 
// results in a successfull parse. It fails if all parsers fail.
var parserPlusN = makeNP(function(state, scope, k, parsers){
		var i = 1,
			l = parsers.length,
			result = parsers[0];
		
		for(; i < l; ++i)
			result = parserPlus2(result, parsers[i]);

		return result(state, scope, k);
	});

var mplus = parserPlus;


var try_ = make1P(function(state, scope, k, p){
		var prevIndex = state.index,
			prevLength = state.length;

		return {func: p, args: [state, scope, function(result){
			if(result.success)
				return k(result);
			
			state.index = prevIndex;
			state.length = prevLength;
			return k(_fail(state, result.expecting));
		
		}]};
	});


//accepts multiple parsers and returns a new parser that
//evaluates them in order and
//succeeds if all the parsers succeeded
//fails when a parser fails but returns the array of previous ASTs in the result
var tokens = makeNP(function(state, scope, parsers){
		var matched = "",
			ast = [],
			i = 0,
			l = parsers.length,
			p, result;

		for(; i < l; ++i){
			p = parsers[i];
			result = (p.length ? p : p())(state, scope);
			matched += result.matched;
			if(!result.success)
				break;
			if(result.ast !== undef)
				ast.push(result.ast);
			
		}
		result = extend({}, result);
		result.matched = matched;
		result.ast = ast;

		return result;
	});


function _many(onePlusMatch){ 
	return make1P(function(state, scope, p){
		var ast = [],
			matched = "",
			prevIndex = state.index,
			result = p(state, scope);

		if(onePlusMatch && !result.success) 
			return _fail(state);
		
		while(result.success) {
			if(result.ast !== undef)
				ast.push(result.ast);
			matched += result.matched;
			if(state.index == prevIndex)
				break;
					
			prevIndex = state.index;
			result = p(state, scope);
			
		}
		result = extend({}, result);
		result.matched = matched;
		result.ast = ast;
		result.success = state.index == prevIndex;
		if(result.success)
			delete result.expecting;
		return result;	
	});
}


var many = _many(false);

var many1 = _many(true);

var skipMany = make1P(function(state, scope, p){
		var result = many(p)(state, scope);
		result = extend({}, result);
		result.ast = undef;
		return result;
	});

var = function(cond){
	return function(state, scope, k){
		return {func: function(){
			var fstchar = state.at(0);
			return k((state.length > 0 && cond(fstchar)) ?
						make_result(state.scroll(1), fstchar, fstchar) : 
						_fail(state, fstchar));
		}};
	}
};

var char_ = function(c){
	return function(state, scope, k){
		return {func: function(){
			return k((state.length > 0 && state.at(0) == c) ?
						make_result(state.scroll(1), c, c) : 
						_fail(state, c));
		}};
	}
};

var string = make(function(state, scope, s){
	var startIndex = state.index;
	var result = tokens.apply(null, map(char_, s))(state, scope);
	result.ast = result.ast.join("");
	result = extend({}, result);
	if(!result.success)
		result.expecting = {at:startIndex, expecting: s};
	else delete result.expecting;
	if(!result.ast.length)
		result.ast = undef;
	return result;
});



function range(lower, upper){
    return {
		indexOf: function(ch){ return (ch >= lower && ch <= upper) ? true : -1 },
		toString: function(){ return "range(" + lower + ", " + upper + ")" }
	};
}

var optional_old = make1P(function(state, scope, p){
		var result = p(state, scope);
		if(!result.success && !result.matched.length){
			result = extend({}, result);
			delete result.expecting;
			result.success = true;
		}
		return result;
	});


var label = make1P(function(state, scope, p, str){
		var prevIndex = state.index;
		var result = p(state, scope);
		if(!result.success){
			result = extend({}, result);
			result.expecting = {at: prevIndex, expecting: str};
		}
		return result;
	});


//accepts a regexp or a string
//in case of a string it either matches the whole string or nothing
var match = make(function(state, scope, sr){
		if(typeof sr == "string")
			return (state.substring(0, sr.length) == sr) ?
				make_result(state.scroll(sr.length), sr, sr) : _fail(state, sr);
		if(sr.exec){
			sr = new RegExp("^" + sr.source);
			var substr = state.substring(0);
			var match = sr.exec(substr);
			match = match && match[0];
			var length = match && match.length;
			var matched = substr.substr(0, length);
			return length ? make_result(state.scroll(length), matched, matched) : _fail(state, sr.source.substr(1));
		}
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
		func:	bind,
		fixity: infixr(-1) //this is a special operator, don't use negative fixity anywhere else!
		//,type:	[String, Parser, Parser]
	},
	">>=": {
		func:	parserBind,
		fixity: infixl(1)
		//,type:	[Parser, Function, Parser]
	},
	"=<<": {
		func:	flip(parserBind),
		fixity: infixr(1)
		//,type:	[Parser, Parser, Parser]
	},
	">>" : {
		func:	skip_fst,
		fixity: infixl(1)
		//,type:	[Parser, Parser, Parser]
	},
	"*>" : { //liftA2 (const id)
		func:	skip_fst,
		fixity: infixl(4)
		//,type:	[Parser, Parser, Parser]
	},
	"<*" : { //liftA2 const
		func:	skip_snd,
		fixity: infixl(4)
		//,type:	[Parser, Parser, Parser]
	},
	"<$>": {
		func:	fmap,
		fixity: infixl(4)
		//,type:	[Function, Parser, Parser]
	},
	"<*>": {
		func:	ap,
		fixity: infixl(4)
		//,type:	[Parser, Parser, Parser]
	},
	"<**>": { //liftA2 (flip ($))
		func:	curry(liftA2)(flip(call)),
		fixity: infixl(4)
		//,type:	[Parser, Parser, Parser]
	},
		//the (<$) combinator uses the value on the left 
		//if the parser on the right succeeds. x <$ p = pure x <* p
		//from Control.Applicative: (<$>) . const :: Functor f => a -> f b -> f a
	"<$" : {
		func:	function(val, parser){ return skip_snd(pure(value), parser) },
		fixity: infixl(4)
		//,type:	["*", Parser, Parser]
	},
	"<|>": {
		func:	parserPlus,
		fixity: infixr(1)
		//,type:	[Parser, Parser, Parser]
	},
	"<?>": {
		func:	label,
		fixity: infix(0)
		//,type:	[Parser, String, Parser]
	}	
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
				bind("xs", function(state){ return sepEndBy(p, sep)(state) }),
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
	var scan =	do_( 
					bind("x", p), 
					function(state, scope){ return rest(scope.x)(state) }
				);

	function rest(x){ 
		var a = do_(
					bind("f", op),
					bind("y", p),
					function(state, scope){
						return rest(scope.f(x, scope.y))(state, scope)
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
	var scan =	do_( 
					bind("x", p), 
					function(state, scope){ return rest(scope.x)(state) }
				);

	function rest(x){ 
		var a = do_(
					bind("f", op),
					bind("y", scan),
					function(state, scope){
						return make_result(s, "", scope.f(x, scope.y))
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

function anyToken(state){
	var at = state.at(0);
	return at.length ? make_result(state.scroll(1), at, at) : _fail(state);
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
function eof(state){
    return make_result(state, "", undef, !state.length, state.length ? "end of input" : undef);
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
				unexpected("c")
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

	function _scan(state){ return scan(state) }

	var scan = parserPlus(
		do_( end, return_([]) ),
		do_( bind("x", p), bind("xs", _scan), returnCall(cons, "x", "xs") )
	)

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

// -------------------------------------------------
// Token
// -------------------------------------------------

 
//-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
//-- for a description of how to use it.

//module Text.Parsec.Token
//    ( LanguageDef
//    , GenLanguageDef (..)
//    , TokenParser
//    , GenTokenParser (..)
//    , makeTokenParser
//    ) where
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
//    = LanguageDef { 
//    
//    -- | Describes the start of a block comment. Use the empty string if the
//    -- language doesn't support block comments. For example \"\/*\". 
//
//    commentStart   :: String,
commentStart: String,
//
//    -- | Describes the end of a block comment. Use the empty string if the
//    -- language doesn't support block comments. For example \"*\/\". 
//
//    commentEnd     :: String,
commentEnd: String,
//
//    -- | Describes the start of a line comment. Use the empty string if the
//    -- language doesn't support line comments. For example \"\/\/\". 
//
//    commentLine    :: String,
commentLine: String,
//
//    -- | Set to 'True' if the language supports nested block comments. 
//
//    nestedComments :: Bool,
nestedComments: Boolean,
//
//    -- | This parser should accept any start characters of identifiers. For
//    -- example @letter \<|> char \"_\"@. 
//
//    identStart     :: ParsecT s u m Char,
identStart: Parser,
//
//    -- | This parser should accept any legal tail characters of identifiers.
//    -- For example @alphaNum \<|> char \"_\"@. 
//
//    identLetter    :: ParsecT s u m Char,
identLetter: Parser,
//
//    -- | This parser should accept any start characters of operators. For
//    -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@ 
//
//    opStart        :: ParsecT s u m Char,
opStart: Parser,
//
//    -- | This parser should accept any legal tail characters of operators.
//    -- Note that this parser should even be defined if the language doesn't
//    -- support user-defined operators, or otherwise the 'reservedOp'
//    -- parser won't work correctly. 
//
//    opLetter       :: ParsecT s u m Char,
opLetter: Parser,
//
//    -- | The list of reserved identifiers. 
//
//    reservedNames  :: [String],
reservedNames: Array,
//
//    -- | The list of reserved operators. 
//
//    reservedOpNames:: [String],
reservedOpNames: Array,
//
//    -- | Set to 'True' if the language is case sensitive. 
//
//    caseSensitive  :: Bool
caseSensitive: Boolean
//
//    }

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
//    = TokenParser {
//
//        -- | This lexeme parser parses a legal identifier. Returns the identifier
//        -- string. This parser will fail on identifiers that are reserved
//        -- words. Legal identifier (start) characters and reserved words are
//        -- defined in the 'LanguageDef' that is passed to
//        -- 'makeTokenParser'. An @identifier@ is treated as
//        -- a single token using 'try'.
//
//        identifier       :: ParsecT s u m String,
identifier: Parser,
//        
//        -- | The lexeme parser @reserved name@ parses @symbol 
//        -- name@, but it also checks that the @name@ is not a prefix of a
//        -- valid identifier. A @reserved@ word is treated as a single token
//        -- using 'try'. 
//
//        reserved         :: String -> ParsecT s u m (),
reserved: Function,
//
//        -- | This lexeme parser parses a legal operator. Returns the name of the
//        -- operator. This parser will fail on any operators that are reserved
//        -- operators. Legal operator (start) characters and reserved operators
//        -- are defined in the 'LanguageDef' that is passed to
//        -- 'makeTokenParser'. An @operator@ is treated as a
//        -- single token using 'try'. 
//
//        operator         :: ParsecT s u m String,
operator: Parser,
//
//        -- |The lexeme parser @reservedOp name@ parses @symbol
//        -- name@, but it also checks that the @name@ is not a prefix of a
//        -- valid operator. A @reservedOp@ is treated as a single token using
//        -- 'try'. 
//
//        reservedOp       :: String -> ParsecT s u m (),
reservedOp: Function,
//
//
//        -- | This lexeme parser parses a single literal character. Returns the
//        -- literal character value. This parsers deals correctly with escape
//        -- sequences. The literal character is parsed according to the grammar
//        -- rules defined in the Haskell report (which matches most programming
//        -- languages quite closely). 
//
//        charLiteral      :: ParsecT s u m Char,
charLiteral: Parser,
//
//        -- | This lexeme parser parses a literal string. Returns the literal
//        -- string value. This parsers deals correctly with escape sequences and
//        -- gaps. The literal string is parsed according to the grammar rules
//        -- defined in the Haskell report (which matches most programming
//        -- languages quite closely). 
//
//        stringLiteral    :: ParsecT s u m String,
stringLiteral: Parser,
//
//        -- | This lexeme parser parses a natural number (a positive whole
//        -- number). Returns the value of the number. The number can be
//        -- specified in 'decimal', 'hexadecimal' or
//        -- 'octal'. The number is parsed according to the grammar
//        -- rules in the Haskell report. 
//
//        natural          :: ParsecT s u m Integer,
natural: Parser,
//
//        -- | This lexeme parser parses an integer (a whole number). This parser
//        -- is like 'natural' except that it can be prefixed with
//        -- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
//        -- number can be specified in 'decimal', 'hexadecimal'
//        -- or 'octal'. The number is parsed according
//        -- to the grammar rules in the Haskell report. 
//        
//        integer          :: ParsecT s u m Integer,
integer: Parser,
//
//        -- | This lexeme parser parses a floating point value. Returns the value
//        -- of the number. The number is parsed according to the grammar rules
//        -- defined in the Haskell report. 
//
//        float            :: ParsecT s u m Double,
float_: Parser,
//
//        -- | This lexeme parser parses either 'natural' or a 'float'.
//        -- Returns the value of the number. This parsers deals with
//        -- any overlap in the grammar rules for naturals and floats. The number
//        -- is parsed according to the grammar rules defined in the Haskell report. 
//
//        naturalOrFloat   :: ParsecT s u m (Either Integer Double),
naturalOrFloat: Parser,
//
//        -- | Parses a positive whole number in the decimal system. Returns the
//        -- value of the number. 
//
//        decimal          :: ParsecT s u m Integer,
decimal: Parser,
//
//        -- | Parses a positive whole number in the hexadecimal system. The number
//        -- should be prefixed with \"0x\" or \"0X\". Returns the value of the
//        -- number. 
//
//        hexadecimal      :: ParsecT s u m Integer,
hexadecimal: Parser,
//
//        -- | Parses a positive whole number in the octal system. The number
//        -- should be prefixed with \"0o\" or \"0O\". Returns the value of the
//        -- number. 
//
//        octal            :: ParsecT s u m Integer,
octal: Parser,
//
//        -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
//        -- trailing white space. 
//
//        symbol           :: String -> ParsecT s u m String,
symbol: Function,
//
//        -- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
//        -- parser, returning the value of @p@. Every lexical
//        -- token (lexeme) is defined using @lexeme@, this way every parse
//        -- starts at a point without white space. Parsers that use @lexeme@ are
//        -- called /lexeme/ parsers in this document.
//        -- 
//        -- The only point where the 'whiteSpace' parser should be
//        -- called explicitly is the start of the main parser in order to skip
//        -- any leading white space.
//        --
//        -- >    mainParser  = do{ whiteSpace
//        -- >                     ; ds <- many (lexeme digit)
//        -- >                     ; eof
//        -- >                     ; return (sum ds)
//        -- >                     }
//
//        lexeme           :: forall a. ParsecT s u m a -> ParsecT s u m a,
lexeme: Function,
//
//        -- | Parses any white space. White space consists of /zero/ or more
//        -- occurrences of a 'space', a line comment or a block (multi
//        -- line) comment. Block comments may be nested. How comments are
//        -- started and ended is defined in the 'LanguageDef'
//        -- that is passed to 'makeTokenParser'. 
//
//        whiteSpace       :: ParsecT s u m (),
whiteSpace: Parser,
//
//        -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
//        -- returning the value of @p@.
//
//        parens           :: forall a. ParsecT s u m a -> ParsecT s u m a,
parens: Function,
//
//        -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
//        -- \'}\'), returning the value of @p@. 
//
//        braces           :: forall a. ParsecT s u m a -> ParsecT s u m a,
braces: Function,
//
//        -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
//        -- and \'>\'), returning the value of @p@. 
//
//        angles           :: forall a. ParsecT s u m a -> ParsecT s u m a,
angles: Function,
//
//        -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
//        -- and \']\'), returning the value of @p@. 
//
//        brackets         :: forall a. ParsecT s u m a -> ParsecT s u m a,
brackets: Function,
//
//        -- | DEPRECATED: Use 'brackets'.
//
//        squares          :: forall a. ParsecT s u m a -> ParsecT s u m a,
squares: Function,
//
//        -- | Lexeme parser |semi| parses the character \';\' and skips any
//        -- trailing white space. Returns the string \";\". 
//
//        semi             :: ParsecT s u m String,
semi: Parser,
//
//        -- | Lexeme parser @comma@ parses the character \',\' and skips any
//        -- trailing white space. Returns the string \",\". 
//
//        comma            :: ParsecT s u m String,
comma: Parser,
//
//        -- | Lexeme parser @colon@ parses the character \':\' and skips any
//        -- trailing white space. Returns the string \":\". 
//
//        colon            :: ParsecT s u m String,
colon: Parser,
//
//        -- | Lexeme parser @dot@ parses the character \'.\' and skips any
//        -- trailing white space. Returns the string \".\". 
//
//        dot              :: ParsecT s u m String,
dot: Parser,
//
//        -- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
//        -- separated by 'semi'. Returns a list of values returned by
//        -- @p@.
//
//        semiSep          :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep: Function,
//
//        -- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
//        -- separated by 'semi'. Returns a list of values returned by @p@. 
//
//        semiSep1         :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep1: Function,
//
//        -- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
//        -- @p@ separated by 'comma'. Returns a list of values returned
//        -- by @p@. 
//
//        commaSep        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep: Function,
//
//        -- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
//        -- @p@ separated by 'comma'. Returns a list of values returned
//        -- by @p@. 
//
//        commaSep1        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep1: Function
//    }
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
//                => GenLanguageDef s u m -> GenTokenParser s u m
//makeTokenParser languageDef
//    = TokenParser{ identifier = identifier
//                 , reserved = reserved
//                 , operator = operator
//                 , reservedOp = reservedOp
//
//                 , charLiteral = charLiteral
//                 , stringLiteral = stringLiteral
//                 , natural = natural
//                 , integer = integer
//                 , float = float
//                 , naturalOrFloat = naturalOrFloat
//                 , decimal = decimal
//                 , hexadecimal = hexadecimal
//                 , octal = octal
//
//                 , symbol = symbol
//                 , lexeme = lexeme
//                 , whiteSpace = whiteSpace
//
//                 , parens = parens
//                 , braces = braces
//                 , angles = angles
//                 , brackets = brackets
//                 , squares = brackets
//                 , semi = semi
//                 , comma = comma
//                 , colon = colon
//                 , dot = dot
//                 , semiSep = semiSep
//                 , semiSep1 = semiSep1
//                 , commaSep = commaSep
//                 , commaSep1 = commaSep1
//                 }


function makeTokenParser(languageDef){
    if(!languageDef.LanguageDef)
        throw "Type error: unexpected '" + languageDef.constructor.name + "', expecting 'GenLanguageDef.LanguageDef'";


//    -----------------------------------------------------------
//    -- White space & symbols
//    -----------------------------------------------------------

//    symbol name
//        = lexeme (string name)

function symbol(name){
	return lexeme(string(name));
}


//
//    lexeme p
//        = do{ x <- p; whiteSpace; return x  }

function lexeme(p){
	return do_(bind("x", p), whiteSpace, ret("x") );
}


//
//
//    simpleSpace =
//        skipMany1 (satisfy isSpace)

var simpleSpace =
        skipMany1(satisfy(isSpace));


//
//    oneLineComment =
//        do{ try (string (commentLine languageDef))
//          ; skipMany (satisfy (/= '\n'))
//          ; return ()
//          }

var oneLineComment =
        cs( try_(string(languageDef.commentLine)) )
          ( skipMany, satisfy(function(c){ return c != '\n' }) )
          ( return_, null).resolve();


//
//    inCommentSingle
//        =   do{ try (string (commentEnd languageDef)); return () }
//        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
//        <|> do{ oneOf startEnd                      ; inCommentSingle }
//        <?> "end of comment"
//        where
//          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

var startEnd = nub( slice( languageDef.commentEnd + languageDef.commentStart ) );

function _inCommentSingle(st, sc){ return inCommentSingle(st, sc) }

var inCommentSingle
            = [ do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentSingle )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentSingle )
        ,"<?>", "end of comment"].resolve();



//    inCommentMulti
//        =   do{ try (string (commentEnd languageDef)) ; return () }
//        <|> do{ multiLineComment                     ; inCommentMulti }
//        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
//        <|> do{ oneOf startEnd                       ; inCommentMulti }
//        <?> "end of comment"
//        where
//          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

function _inCommentMulti(st){ return inCommentMulti(st) }

var inCommentMulti
            = [ do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( _multiLineComment                     , _inCommentMulti )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentMulti )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentMulti )
        ,"<?>", "end of comment"].resolve();



//    inComment
//        | nestedComments languageDef  = inCommentMulti
//        | otherwise                = inCommentSingle

var inComment = languageDef.nestedComments ? inCommentMulti : inCommentSingle;


//    multiLineComment =
//        do { try (string (commentStart languageDef))
//           ; inComment
//           }

function _multiLineComment(st){ return multiLineComment(st) }

var multiLineComment =
        do_( try_ (string (languageDef.commentStart))
           , inComment)


//    whiteSpace
//        | noLine && noMulti  = skipMany (simpleSpace <?> "")
//        | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
//        | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
//        | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
//        where
//          noLine  = null (commentLine languageDef)
//          noMulti = null (commentStart languageDef)

var noLine   = null_(languageDef.commentLine);
var noMulti  = null_(languageDef.commentStart);

var whiteSpace = (
	(noLine && noMulti) ? [skipMany, [simpleSpace ,"<?>", ""]] :
	noLine				? [skipMany, [simpleSpace ,"<|>", multiLineComment ,"<?>", ""]] :
	noMulti				? [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<?>", ""]] :
						  [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<|>", multiLineComment ,"<?>", ""]]
	).resolve();




//    -----------------------------------------------------------
//    -- Bracketing
//    -----------------------------------------------------------
//    parens p        = between (symbol "(") (symbol ")") p
//    braces p        = between (symbol "{") (symbol "}") p
//    angles p        = between (symbol "<") (symbol ">") p
//    brackets p      = between (symbol "[") (symbol "]") p
//
//    semi            = symbol ";"
//    comma           = symbol ","
//    dot             = symbol "."
//    colon           = symbol ":"
//
//    commaSep p      = sepBy p comma
//    semiSep p       = sepBy p semi
//
//    commaSep1 p     = sepBy1 p comma
//    semiSep1 p      = sepBy1 p semi



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



//    -----------------------------------------------------------
//    -- Chars & Strings
//    -----------------------------------------------------------


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


//    -- escape code tables
//    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
//    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

var escMap          = zip("abfnrtv\\\"\'", "\a\b\f\n\r\t\v\\\"\'");
var asciiMap        = zip((ascii3codes + ascii2codes), (ascii3 + ascii2));

//
//    charEsc         = choice (map parseEsc escMap)
//                    where
//                      parseEsc (c,code)     = do{ char c; return code }

var charEsc         = choice(map(parseEsc, escMap))
                    
function parseEsc(tuple){
    return do_( char_(tuple[0]), return_(tuple[1]) );
}


//    charAscii       = choice (map parseAscii asciiMap)
//                    where
//                      parseAscii (asc,code) = try (do{ string asc; return code })

var charAscii       = choice(map(parseAscii, asciiMap))

function parseAscii(tuple){
    return try_(do_( string(tuple[0]), return_(tuple[1]) ));
}


//    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

var stringLetter    = satisfy(function(c){ return (c != '"') && (c != '\\') && (c > '\026') }); //TODO: last expr.


//    escapeEmpty     = char '&'

var escapeEmpty     = char_('&');


//    escapeGap       = do{ many1 space
//                        ; char '\\' <?> "end of string gap"
//                        }

var escapeGap       = cs( many1, space )
                        ( char_('\\') ,"<?>", "end of string gap").resolve();
                        

//    charNum         = do{ code <- decimal
//                                  <|> do{ char 'o'; number 8 octDigit }
//                                  <|> do{ char 'x'; number 16 hexDigit }
//                        ; return (toEnum (fromInteger code))
//                        }

var charNum         = cs( "code" ,"<-", _decimal
                                      ,"<|>", do_( char_('o'), number(8, octDigit) )
                                      ,"<|>", do_( char_('x'), number(16, hexDigit) )
                        )
                        ( ret(function(scope){ return toEnum(fromInteger(scope.code)) }) ).resolve();


//    charControl     = do{ char '^'
//                        ; code <- upper
//                        ; return (toEnum (fromEnum code - fromEnum 'A'))
//                        }

var charControl     = cs( char_('^') )
                        ( "code" ,"<-", upper )
                        ( ret(function(scope){ return toEnum(fromEnum(scope.code) - fromEnum('A'))  }) ).resolve();
 

//    -- escape codes
//    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
//                    <?> "escape code"

var escapeCode      = [charEsc ,"<|>", charNum ,"<|>", charAscii ,"<|>", charControl
                    ,"<?>", "escape code"].resolve();


//    charEscape      = do{ char '\\'; escapeCode }

var charEscape        = do_(char_('\\'), escapeCode);



//    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

var charLetter        = satisfy(function(c){ return (c != '\'') && (c != '\\') && (c > '\026') }); //TODO: last expr.


//
//    characterChar   = charLetter <|> charEscape
//                    <?> "literal character"

var characterChar    = [charLetter ,"<|>", charEscape
                    ,"<?>", "literal character"].resolve();


//    charLiteral     = lexeme (between (char '\'')
//                                      (char '\'' <?> "end of character")
//                                      characterChar )
//                    <?> "character"

var charLiteral        = [lexeme, [between, char_('\''), 
                                    [char_('\'') ,"<?>", "end of character"],
                                    characterChar]
                    ,"<?>", "character"].resolve();


//
//    stringEscape    = do{ char '\\'
//                        ;     do{ escapeGap  ; return Nothing }
//                          <|> do{ escapeEmpty; return Nothing }
//                          <|> do{ esc <- escapeCode; return (Just esc) }
//                        }

var stringEscape    = cs( char_('\\') )
                        (         cs( escapeGap   ) ( return_, Maybe.Nothing )
                          ,"<|>", cs( escapeEmpty ) ( return_, Maybe.Nothing )
                          ,"<|>", cs( "esc" ,"<-", escapeCode) ( returnCall, Maybe.Just, "esc" )
                        ).resolve();


//    stringChar      =   do{ c <- stringLetter; return (Just c) }
//                    <|> stringEscape
//                    <?> "string character"

var stringChar      = [cs( "c" ,"<-", stringLetter )
                         ( returnCall, Maybe.Just, "c" )
                      ,"<|>", stringEscape
                      ,"<?>", "string character"].resolve();


//    stringLiteral   = lexeme (
//                      do{ str <- between (char '"')
//                                         (char '"' <?> "end of string")
//                                         (many stringChar)
//                        ; return (foldr (maybe id (:)) "" str)
//                        }
//                      <?> "literal string")

var stringLiteral   = lexeme(
                          [ cs( "str", "<-", between, char_('"'),
                                                      [char_('"') ,"<?>", "end of string"],
                                                      [many, stringChar]
                               )
                               (ret, function(scope){ return foldr(curry(maybe)(id, curry(cons)), "", scope.str) }) //TODO
                          ,"<?>", "literal string"].resolve()
                      );



//    -----------------------------------------------------------
//    -- Numbers
//    -----------------------------------------------------------


//    number base baseDigit
//        = do{ digits <- many1 baseDigit
//            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
//            ; seq n (return n)
//            }

function number(base, baseDigit){ 
    return cs( "digits" ,"<-", many1, baseDigit )
			 ( ret, function(scope){
						return foldl(function(x, d){
								  return base * x + toInteger(digitToInt(d))
							  }, 0, scope.digits);
			 }).resolve();
}


//    decimal         = number 10 digit
//    hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
//    octal           = do{ oneOf "oO"; number 8 octDigit  }

function _decimal(st, sc){ return decimal(st, sc) }

var decimal         = number(10, digit);
var hexadecimal     = cs( oneOf, "xX" ) ( number, 16, hexDigit ).resolve();
var octal           = cs( oneOf, "oO" ) ( number, 8, octDigit  ).resolve();



//
//    fraction        = do{ char '.'
//                        ; digits <- many1 digit <?> "fraction"
//                        ; return (foldr op 0.0 digits)
//                        }
//                      <?> "fraction"
//                    where
//                      op d f    = (f + fromIntegral (digitToInt d))/10.0

var fraction        = [ cs( char_('.'))
                          ( "digits" ,"<-", many1, digit ,"<?>", "fraction")
                          ( ret, function(scope){ return foldr(op, 0.0, scope.digits) })
                        ,"<?>", "fraction"].resolve();
function op(d, f){
	return (f + fromIntegral(digitToInt(d))) / 10.0
}


//
//    sign            =   (char '-' >> return negate)
//                    <|> (char '+' >> return id)
//                    <|> return id

var sign            = [[char_('-') ,">>", return_, negate]
                       ,"<|>", [char_('+') ,">>", return_, id]
                       ,"<|>", return_, id
                      ].resolve();


//
//    exponent'       = do{ oneOf "eE"
//                        ; f <- sign
//                        ; e <- decimal <?> "exponent"
//                        ; return (power (f e))
//                        }
//                      <?> "exponent"
//                    where
//                       power e  | e < 0      = 1.0/power(-e)
//                                | otherwise  = fromInteger (10^e)


var exponent_       = [ cs( oneOf, "eE" )
                          ( "f" ,"<-", sign )
                          ( "e" ,"<-", decimal ,"<?>", "exponent" )
                          ( returnCall, power, "f", "e")
                      ,"<?>", "exponent"].resolve();

function power(e){
	return (e < 0) ?  1.0 / power(-e) :  fromInteger(Math.pow(10,e));
}



//    fractExponent n = do{ fract <- fraction
//                        ; expo  <- option 1.0 exponent'
//                        ; return ((fromInteger n + fract)*expo)
//                        }
//                    <|>
//                      do{ expo <- exponent'
//                        ; return ((fromInteger n)*expo)
//                        }

function fractExponent(n){
	return [
		  cs( "fract" ,"<-", fraction )
			( "expo"  ,"<-", option, 1.0, exponent_ )
			( ret, function(scope){ return fromInteger(n + scope.fract) * scope.expo })
		,"<|>",
		  cs( "expo", "<-", exponent_ )
			( ret, function(scope){ return fromInteger(n) * scope.expo })
	].resolve();
}

//    -- floats
//    floating        = do{ n <- decimal
//                        ; fractExponent n
//                        }

var floating        = cs( "n" ,"<-", decimal)
                        ( function(state, scope){ return fractExponent(scope.n)(state) }).resolve();


//    fractFloat n    = do{ f <- fractExponent n
//                        ; return (Right f)
//                        }

function fractFloat(n){
	return cs( "f" ,"<-", fractExponent, n)
			 ( returnCall, Either.Right, "f").resolve();
}


//
//    decimalFloat    = do{ n <- decimal
//                        ; option (Left n)
//                                 (fractFloat n)
//                        }

var decimalFloat    = cs( "n" ,"<-", decimal )
                        ( function(state, scope){ 
	                           return option(Either.Left(scope.n), fractFloat(scope.n))(state, scope);
                        }).resolve();


//
//    zeroNumFloat    =  do{ n <- hexadecimal <|> octal
//                         ; return (Left n)
//                         }
//                    <|> decimalFloat
//                    <|> fractFloat 0
//                    <|> return (Left 0)


var zeroNumFloat    =  [ cs( "n" ,"<-", hexadecimal ,"<|>", octal )
                           ( returnCall, Either.Left, "n" )
                       ,"<|>", decimalFloat
                       ,"<|>", fractFloat(0)
                       ,"<|>", return_, Either.Left(0)
	                   ].resolve();




//    natFloat        = do{ char '0'
//                        ; zeroNumFloat
//                        }
//                      <|> decimalFloat

var natFloat        = [do_( char_('0'),
                            zeroNumFloat
                          )
                      ,"<|>", decimalFloat].resolve();



//    zeroNumber      = do{ char '0'
//                        ; hexadecimal <|> octal <|> decimal <|> return 0
//                        }
//                      <?> ""

var zeroNumber      = [ cs( char_, '0')
                          ( hexadecimal ,"<|>", octal ,"<|>", decimal ,"<|>", return_, 0 )
                      ,"<?>", ""].resolve();


//    nat             = zeroNumber <|> decimal

var nat             = [zeroNumber ,"<|>", decimal].resolve();

//    -- integers and naturals
//    int             = do{ f <- lexeme sign
//                        ; n <- nat
//                        ; return (f n)
//                        }

var int_            = cs( "f" ,"<-", lexeme, sign )
                        ( "n" ,"<-", nat )
                        ( ret, function(scope){ return scope.f(scope.n) })



//    naturalOrFloat  = lexeme (natFloat) <?> "number"
//
//    float           = lexeme floating   <?> "float"
//    integer         = lexeme int        <?> "integer"
//    natural         = lexeme nat        <?> "natural"

var naturalOrFloat  = [lexeme, natFloat   ,"<?>", "number" ].resolve();

var float_          = [lexeme, floating   ,"<?>", "float"  ].resolve();
var integer         = [lexeme, int_       ,"<?>", "integer"].resolve();
var natural         = [lexeme, nat        ,"<?>", "natural"].resolve();




//    -----------------------------------------------------------
//    -- Operators & reserved ops
//    -----------------------------------------------------------


//    reservedOp name =
//        lexeme $ try $
//        do{ string name
//          ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
//          }

function reservedOp(name){
	return [lexeme ,"$", try_ ,"$",
				cs( string(name) ) 
				  ( notFollowedBy, languageDef.opLetter ,"<?>", "end of " + name )
			].resolve();
}


//    oper =
//        do{ c <- (opStart languageDef)
//          ; cs <- many (opLetter languageDef)
//          ; return (c:cs)
//          }
//        <?> "operator"

var oper =
        [ cs( "c"  ,"<-", languageDef.opStart )
            ( "cs" ,"<-", many, languageDef.opLetter )
            ( returnCall, consJoin, "c", "cs" )
         ,"<?>", "operator"].resolve();


//    operator =
//        lexeme $ try $
//        do{ name <- oper
//          ; if (isReservedOp name)
//             then unexpected ("reserved operator " ++ show name)
//             else return name
//          }

var operator =
        [lexeme ,"$", try_ ,"$",
        cs( "name" ,"<-", oper )
          ( function(state, scope){
					return (isReservedOp(scope.name) ? 
						unexpected("reserved operator " + scope.name) : return_(scope.name) )(state, scope);
          })].resolve();


//
//    isReservedOp name =
//        isReserved (sort (reservedOpNames languageDef)) name

function isReservedOp(name){
        return isReserved( sort( languageDef.reservedOpNames ), name);
}


//    -----------------------------------------------------------
//    -- Identifiers & Reserved words
//    -----------------------------------------------------------


//    reserved name =
//        lexeme $ try $
//        do{ caseString name
//          ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
//          }

function reserved(name){
	return [lexeme ,"$", try_ ,"$",
			cs( caseString(name) )
			  ( notFollowedBy, languageDef.identLetter ,"<?>", "end of " + name )
			].resolve();
}


//    caseString name
//        | caseSensitive languageDef  = string name
//        | otherwise               = do{ walk name; return name }
//        where
//          walk []     = return ()
//          walk (c:cs) = do{ caseChar c <?> msg; walk cs }
//
//          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
//                      | otherwise  = char c
//
//          msg         = show name

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


//    ident
//        = do{ c <- identStart languageDef
//            ; cs <- many (identLetter languageDef)
//            ; return (c:cs)
//            }
//        <?> "identifier"

var ident
        = [ cs( "c"  ,"<-", languageDef.identStart )
              ( "cs" ,"<-", many, languageDef.identLetter )
              ( returnCall, consJoin, "c", "cs" )
           ,"<?>", "identifier"].resolve();


//    identifier =
//        lexeme $ try $
//        do{ name <- ident
//          ; if (isReservedName name)
//             then unexpected ("reserved word " ++ show name)
//             else return name
//          }

var identifier =
        [lexeme ,"$", try_ ,"$",
        cs( "name" ,"<-", ident )
          ( function(state, scope){
				return ( isReservedName(scope.name) ? 
							unexpected("reserved word " + scope.name) : 
							return_(scope.name)
						)(state, scope);
          })].resolve();



//    isReservedName name
//        = isReserved theReservedNames caseName
//        where
//          caseName      | caseSensitive languageDef  = name
//                        | otherwise               = map toLower name

function isReservedName(name){
	var caseName = languageDef.caseSensitive ? name : name.toLowerCase();

	return isReserved(theReservedNames, caseName);
}


//    isReserved names name
//        = scan names
//        where
//          scan []       = False
//          scan (r:rs)   = case (compare r name) of
//                            LT  -> scan rs
//                            EQ  -> True
//                            GT  -> False

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

//    theReservedNames
//        | caseSensitive languageDef  = sortedNames
//        | otherwise               = map (map toLower) sortedNames
//        where
//          sortedNames   = sort (reservedNames languageDef)

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
//                , identLetter	   = alphaNum <|> oneOf "_'"
//                , opStart	       = opLetter haskellStyle
//                , opLetter	   = oneOf ":!#$%&*+./<=>?@\\^|-~"
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
//		{ commentStart	 = "/*"
//		, commentEnd	 = "*/"
//		, commentLine	 = "//"
//		, nestedComments = True
//		, identStart	 = letter
//		, identLetter	 = alphaNum <|> oneOf "_'"
//		, reservedNames  = []
//		, reservedOpNames= []
//      , caseSensitive  = False
//		}

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
               , caseSensitive   : false //TODO: why?
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
//	        { identLetter	 = identLetter haskell98Def <|> char '#'
//	        , reservedNames	 = reservedNames haskell98Def ++
//    				   ["foreign","import","export","primitive"
//    				   ,"_ccall_","_casm_"
//    				   ,"forall"
//    				   ]
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

var haskell = makeTokenParser(haskellDef);


//-----------------------------------------------------------
//-- Mondrian
//-----------------------------------------------------------

//-- | The language definition for the language Mondrian.
//
//mondrianDef :: LanguageDef st
//mondrianDef = javaStyle
//		{ reservedNames = [ "case", "class", "default", "extends"
//				  , "import", "in", "let", "new", "of", "package"
//				  ]
//                , caseSensitive  = True
//		}


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

var mondrian = makeTokenParser(mondrianDef);


