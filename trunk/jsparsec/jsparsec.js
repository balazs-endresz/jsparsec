/*! 
 * JSParsec - A parser combinator library for JavaScript
 * 
 * Version: 0.0.2
 * 
 * http://code.google.com/p/jsparsec/
 * 
 * Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 * Dual licensed under the MIT and GPL licenses.
 * 
 *
 * The initial implementation of some combinators and the memoization is derived from:
 * http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html
 * The most notable difference is that this parser uses a single ParseState object with a 
 * different caching mechanism, and new state objects are never created by any combinator.
 *
 * Most functions should behave like their counterparts in Parsec:
 * http://www.haskell.org/haskellwiki/Parsec
 * 
 */

var undef,
	_toString = {}.toString,
	_slice    = [].slice;


// -------------------------------------------------
// Helper functions
// -------------------------------------------------

function curry(fn){
  function ret(){
    var args = slice(arguments);
    return args.length >= fn.length ? fn.apply(null, args) : 
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
		initial = f(initial, arr[l]);
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

	forward: function(index) {
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
	,rewind: function(index) {
		this.index  -= index;
		this.length += index;
		return this;
	}

	//returns a new state object
	,from: function(index) {
		var r = new ParseState(this.input, this.index + index);
		r.cache  = this.cache;
		r.length = this.length - index;
		return r;
	}

	,skipWhitespace: function(){
		var m = this.substring(0).match(/^\s+/);
		return m ? this.forward(m[0].length) : this;
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

function _fail(state, expecting){
	return make_result(state, "", undef, false, expecting);
}



function parserFail(msg){ return function(state){
	return make_result(state, "", undef, false, msg);
}};

var fail = parserFail;


function parserZero(state){
	return make_result(state, "", undef, false);
}

var mzero = parserZero;
var empty = mzero;


// 'end' is a parser that is successful if the input string is empty (i.e. end of parse).
function eof(state){
    return make_result(state, "", undef, state.length === 0);
}



// -------------------------------------------------
// Prim
// -------------------------------------------------


// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p){
    return (typeof p == "string") ? string(p) : 
		isArray(p) ? resolve(p) : p;
}

var run = curry(function(p, strOrState){
		return toParser(p.length ? p : p())
			(strOrState instanceof ParseState ? strOrState : ps(strOrState));
	});

var parser_id = 0;

function _make(fn, show, p1, p2, pN, action){
	return function(p, opt1){
		var pid = parser_id++;
		p = pN ? map(toParser, arguments) : p;
		p = p1 ? toParser(p) : p;
		opt1 = p2 ? toParser(opt1) : opt1;
		opt1 = action ? curry(opt1) : opt1;

		var ret = function(state) {
			var result = state.getCached(pid);
			if(result !== undef)
				return result;
			result = fn(state,
						p1 ? (p.length ? p : p()) : p,
						p2 ? (opt1.length ? opt1 : opt1()) : opt1);

			state.putCached(pid, result);

			return result;
		}
		ret.show = show;
		return ret;
	}
}


//true values apply toParser to the nth argument of the function
var make       = function(fn, show){return _make(fn, show)};
var make1P     = function(fn, show){return _make(fn, show, true)};
var make2P     = function(fn, show){return _make(fn, show, true, true)};
//apply toParser to all:
var makeNP     = function(fn, show){return _make(fn, show, false, false, true)}; 
 //curries the snd arg:
var makeAction = function(fn, show){return _make(fn, show, false, false, false, true)};


function parserBind(p,f){ 
	return function(state){ return f(p(state)) }
}

//stops when one parser has failed and returns only the last result
var do_ = makeNP(function(state, parsers){
		var bindings = {},
			matched = "",
			i = 0,
			l = parsers.length,
			p, result;

		for(; i < l; ++i){
			p = parsers[i];
			result = (p.length ? p : p())(state, bindings);
			matched += result.matched;
			if(!result.success)
				break;			
		}

		result = extend({}, result);
		result.matched = matched;

		if(result.success)
			delete result.expecting;

		return result;
	});


function bind(name, p){ return function(state, bindings){
	var result = p(state, bindings);
	if(result.success)
		bindings[name] = result.ast;
	return result;
}};

//returns the value of an identifier or applies the passed function to the bindings
function ret(name, more){
	var args;
	if(more) 
		args = slice(arguments);

	return function(state, bindings){
		var ast, type = typeof name;
		//if(args){
		//	ast =  resolve(resolveBindings(args, bindings));
		//}else 
		if(type == "string"){
			if(!(name in bindings))
				throw 'Not in scope: "' + name + '"';
			ast = bindings[name];		
		}else
			ast = name(bindings);

		return make_result(state, "", ast);
	}
}

function resolveBindings(arr, bindings){
	return isArray(arr) ?
		map(function(e){ return (e in bindings) ? bindings[e] : resolveBindings(e) }, arr)
		: arr;
}

function withBound(fn, a, b){
	var args = slice(arguments, 1)
	return function(bindings){
		return fn.apply(null, map(function(e){ return bindings[e]}, args));
	}
}

//in contrast with Haskell here's no closure in the do_ notation,
//it's simulated with `bind` and `ret`,
//this function does what `pure` and `return` do in Haskell
function parserReturn(value){ return function(state, bindings){
	return make_result(state, "", value);
}}

var return_ = parserReturn;
var pure = return_;

//executes two parsers in a sequence 
//and applies the ast of the first to the ast of the second
//the ast of the first must be a function
function ap(a, b){
	return action(tokens(a, b), function(ast){ return ast[0](ast[1]) } );
}

// Parser combinator that passes the AST generated from the parser 'p' 
// to the function 'f'. The result of 'f' is used as the AST in the result.
// the function 'f' will be curried automatically
var action = makeAction(function(state, p, f){
		var result = p(state);
		result = extend({}, result);
		result.ast = f(result.ast);
		return result;
	});

var parsecMap = flip(action);
var fmap = parsecMap;
var liftA = fmap;
var liftA2 = function(f, a, b){ return ap(fmap(f, a), b) };
var liftA3 = function(f, a, b, c){ return ap(ap(fmap(f, a), b), c) };

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
    return action(p, function(ast) { return ast.join(sep); });
}

//var skip_fst = function(p1, p2){ return liftA2(const_(id), p1, p2) };
function skip_fst(p1, p2){ return do_(p1, p2) }

//var skip_snd = function(p1, p2){ return liftA2(const_, p1, p2) };
function skip_snd(p1, p2){ return do_(bind("a", p1), p2, ret("a")) }



// 'parserPlus' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that matches some string 
// results in a successfull parse. It fails if all parsers fail.
var parserPlus = makeNP(function(state, parsers){
		var i = 0, l = parsers.length, result, ast, errors = [];
		for(; i < l; ++i){
			ast = (result = parsers[i](state)).ast;
			if(ast !== undefined)
				break;
			else
				errors.push(result.expecting);
		}
		result = extend({}, result);
		result.success = (i != l);
		if(!result.success)
			result.expecting = errors;
		else
			delete result.expecting;
		return result;
	});

var mplus = parserPlus;


var try_ = make1P(function(state, p){
		var prevIndex = state.index,
			prevLength = state.length,
			result = p(state);

		if(result.success)
			return result;
		
		state.index = prevIndex;
		state.length = prevLength;
		return _fail(state, result.expecting);

	});


//accepts multiple parsers and returns a new parser that
//evaluates them in order and
//succeeds if all the parsers succeeded
//fails when a parser fails but returns the array of previous ASTs in the result
var tokens = makeNP(function(state, parsers){
		var bindings = {},
			matched = "",
			ast = [],
			i = 0,
			l = parsers.length,
			p, result;

		for(; i < l; ++i){
			p = parsers[i];
			result = (p.length ? p : p())(state, bindings);
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
	return make1P(function(state, p){
		var ast = [],
			matched = "",
			prevIndex = state.index,
			result = p(state);

		if(onePlusMatch && !result.success) 
			return _fail(state);
		
		while(result.success) {
			if(result.ast !== undef)
				ast.push(result.ast);
			matched += result.matched;
			if(state.index == prevIndex)
				break;
					
			prevIndex = state.index;
			result = p(state);
			
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

var skipMany = make1P(function(state, p){
		var result = many(p)(state);
		result = extend({}, result);
		result.ast = undef;
		return result;
	});

var satisfy = make(function(state, cond){
		var fstchar = state.at(0);
		return (state.length > 0 && cond(fstchar)) ?
			make_result(state.forward(1), fstchar, fstchar) : 
			_fail(state, fstchar);
	});

var char_ = make(function(state, c){
		return (state.length > 0 && state.at(0) == c) ?
			make_result(state.forward(1), c, c) : 
			_fail(state, c);
	});

var string = make(function(state, s){
	var startIndex = state.index;
	var result = join_action(tokens.apply(null, map(char_, s)), "")(state);
	result = extend({}, result);
	if(!result.success)
		result.expecting = {at:startIndex, expecting: s};
	else delete result.expecting;
	if(!result.ast.length)
		result.ast = undef;
	return result;
});


// 'range' is a parser combinator that returns a single character parser
// (similar to 'char_'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
var range = make(function(state, lower, upper){
		if(state.length < 1) 
			return _fail(state);

		var ch = state.at(0);
		if(ch >= lower && ch <= upper) 
			return make_result(state.forward(1), ch, ch);

		return _fail(state, "[" + lower +"-"+ upper + "]");
	});


var optional = make1P(function(state, p){
		var result = p(state);
		if(!result.success && !result.matched.length){
			result = extend({}, result);
			delete result.expecting;
			result.success = true;
		}
		return result;
	});


var label = make1P(function(state, p, str){
		var prevIndex = state.index;
		var result = p(state);
		if(!result.success){
			result = extend({}, result);
			result.expecting = {at: prevIndex, expecting: str};
		}
		return result;
	});


//accepts a regexp or a string
//in case of a string it either matches the whole string or nothing
var match = make(function(state, sr){
		if(typeof sr == "string")
			return (state.substring(0, sr.length) == sr) ?
				make_result(state.forward(sr.length), sr, sr) : _fail(state, sr);
		if(sr.exec){
			sr = new RegExp("^" + sr.source);
			var substr = state.substring(0);
			var match = sr.exec(substr);
			match = match && match[0];
			var length = match && match.length;
			var matched = substr.substr(0, length);
			return length ? make_result(state.forward(length), matched, matched) : _fail(state, sr.source.substr(1));
		}
	});





// -------------------------------------------------
// Operators
// -------------------------------------------------

function infixl(strength){ return ["l", strength] }
function infixr(strength){ return ["r", strength] }
function infix (strength){ return ["x", strength] }

function getFixity(opstr){
	return operators[opstr] && operators[opstr].fixity;
}
function getFixityDir(opstr){ 
	return operators[opstr] && (operators[opstr].fixity[0] || "l" );
}
function getFixityStrn(opstr){
	var op = operators[opstr];
	return op && (isDefined(op.fixity[1]) ? op.fixity[1] : 9);
}


var operators = {
	"<-" : {
		func:	bind,
		fixity: infixr(0)
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
	},
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
	var op = operators[args[i]].func;
	var	item = op(args[i-1], args[i+1]);
	args.splice(i-1, 3 , item);
	return resolve(args, rec);
}

//TODO: reject multiple infix operators in the same expression
function resolve(args, rec){
	args = map(function(e){ return isArray(e) ? resolve(e) : e }, args);
	if(rec)
		args = map(function(e){return e instanceof Recurse ? rec : e}, args);

	var fna = [], fn, newfna = [], i = 0, l = args.length;
	for(; i < l; ++i){
		if(!operators[args[i]] && i != (l-1))
			fna.push(args[i]);
		else{
			if(i == (l-1))
				fna.push(args[i]);
			if(fna.length> 1)
				fn = fna[0].apply(null, fna.slice(1));
			else
				fn = fna[0];
			newfna.push(fn);
			if(i != l-1)
				newfna.push(args[i]);
			fna=[];
		}
	}
	args = newfna;


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
	return (function(args){
		function rec(s){return p(s)}

		var lines = [], p, resolved;
		lines.push(resolve(args, rec));

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

		return line;
	})(arguments);
}



// -------------------------------------------------
// Char
// -------------------------------------------------

function elemString(x, xs){
	return !!xs.match(x);
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


// -- definitions from: http://code.haskell.org/parsec3/Text/Parsec/Char.hs


// | @oneOf cs@ succeeds if the current character is in the supplied
// list of characters @cs@. Returns the parsed character. See also
// 'satisfy'.
// 
// >   vowel  = oneOf "aeiou"

//oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//oneOf cs            = satisfy (\c -> elem c cs)

var oneOf = function(cs){
	return satisfy(function(c){ return elemString(c, cs) });
};

// | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
// character /not/ in the supplied list of characters @cs@. Returns the
// parsed character.
//
// >  consonant = noneOf "aeiou"

//noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//noneOf cs           = satisfy (\c -> not (elem c cs))

var noneOf = function(cs){
	return satisfy(function(c){ return !elemString(c, cs) });
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

