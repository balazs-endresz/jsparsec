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

function run(p, strOrState, cb){
		var result = toParser(p.length ? p : p())
			(strOrState instanceof ParseState ? strOrState : ps(strOrState));
		if(!result.success){
			result.error = processError(result.expecting, result.remaining);
			cb && cb(result.error);
		}else{
			delete result.error;
			delete result.expecting;
		}
		return result;
}

function processError(e, s, i, unexp){
	var index = i === undefined ? s.index : i;

	if(typeof e == "string"){
		var lines = s.input.split("\n"),
			linecount = lines.length,
			restlc = s.input.substr(index).split("\n").length,
			line = linecount - restlc + 1,
			lindex = index - lines.splice(0,line-1).join("\n").length;
		return "Unexpected \"" + (unexp || s.input.substr(index, e.length)) +  
				(unexp ? "" : ("\", expecting \"" + e)) + 
				"\" at line " + line + " char " + lindex;
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
		opt1 = action ? curry(opt1) : opt1;

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
 //curries the snd arg:
var makeAction = function(fn, show){return _make(fn, show, false, false, false, true)};


function parserBind(p,f){ 
	return function(state, scope){ return f(p(state, scope)) }
}

//stops when one parser has failed and returns only the last result
var do_ = makeNP(function(state, _scope, parsers){
		var scope = {},
			matched = "",
			i = 0,
			l = parsers.length,
			p, result;
		
		scope.scope = _scope;

		for(; i < l; ++i){
			p = parsers[i];
			result = (p.length ? p : p())(state, scope);
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


function bind(name, p){ 
	if(name == "scope")
		throw "Can't use 'scope' as an identifier!";
	return function(state, scope){
		var result = p(state, scope);
		if(result.success)
			scope[name] = result.ast;
		return result;
	}
};

//returns the value of an identifier or applies the passed function to the bindings
function ret(name, more){
	var args;
	if(more) 
		args = slice(arguments);

	return function(state, scope){
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

		return make_result(state, "", ast);
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
function parserReturn(value){ return function(state, scope){
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
var action = makeAction(function(state, scope, p, f){
		var result = p(state, scope);
		if(!result.success)
			return result;
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
var parserPlus = makeNP(function(state, scope, parsers){
		var i = 0,
			l = parsers.length,
			result,
			ast,
			errors = [];

		for(; i < l; ++i){
			ast = (result = parsers[i](state, scope)).ast;
			result.expecting && errors.push(result.expecting);
			if(ast !== undefined)
				break;
		}
		result = extend({}, result);
		if(!result.success)
			result.expecting = (!errors.length && isArray(errors)) ? errors[0] : errors;
		else
			delete result.expecting;
		return result;
	});

var mplus = parserPlus;


var try_ = make1P(function(state, scope, p){
		var prevIndex = state.index,
			prevLength = state.length,
			result = p(state, scope);

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

var satisfy = make(function(state, scope, cond){
		var fstchar = state.at(0);
		return (state.length > 0 && cond(fstchar)) ?
			make_result(state.scroll(1), fstchar, fstchar) : 
			_fail(state, fstchar);
	});

var char_ = make(function(state, scope, c){
		return (state.length > 0 && state.at(0) == c) ?
			make_result(state.scroll(1), c, c) : 
			_fail(state, c);
	});

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
	}	
});