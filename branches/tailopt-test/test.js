
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

var undef;

function isArray(a){ return {}.toString.call(a) == "[object Array]" }

function make_result(remaining, matched, ast, success, expecting){
	success = success === undef ? true : success;
	return { remaining: remaining, matched: matched, ast: ast, 
				success: success, expecting: expecting };
}

function _fail(state, expecting){
	return make_result(state, "", undef, false, expecting);
}

function replicate(n, x){
	for (var ret = [], i = 0; i < n; ++i)
		ret[i] = x;
	return ret;
}





function trampoline(x) {
	while (x && x.func) {
		x = x.func.apply(null, x.args ||[]);
	}
}

var i = 0 , limit = Infinity;
function trampoline2(x) {
	i++;
	if(!(x && x.func) )
		return;
	if(i > limit)
		return;
	
	x = x.func.apply(null, x.args || []);
	
	if((i % 200) == 0 )
		setTimeout(function(){ trampoline2(x)}, 1);
	else
		trampoline2(x);
	window.x = x;
}

function run(p, strOrState, cb, async){
	(async ? trampoline2 : trampoline) ({func:p, args:[strOrState instanceof ParseState ? strOrState : ps(strOrState), {}, function(result){
		if(!result.success){
			//result.error = processError(result.expecting, result.remaining);
			cb(result.error);
		}else{
			delete result.error;
			delete result.expecting;
			cb(result);
		}
	}]});
}


var do2 = function(p1, p2){
	return function(state, scope, k){
		return { func: p1, args: [state, scope, function(result){
			return result.success ? p2(state, scope, k) : k(result);
		}]};
}};


var parserPlus = function(p1, p2){
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
			
			return (result.ast !== undefined) ? k(result) :
				{func: p2, args: [state, scope, function(result){
					handleError(result);
					return k(result);
				}]}
		}]};
	}
}

var char_ = function(c){
	return function(state, scope, k){
		return {func: function(){
			return k((state.length > 0 && state.at(0) == c) ?
						make_result(state.scroll(1), c, c) : 
						_fail(state, c));
		}};
	}
};

function return_(value){ return function(state, scope, k){
	return k(make_result(state, "", value));
}}


//var limit=3*9000+4; //too much recursion (802 out of range 13)
var limit=3*9000+3; //execute manually, then: InternalError: too much recursion

/**/
var st = ps(replicate(9000,"a").join("")); //ParseState object, with a string of 9000 "a"-s
function rec(st,sc,k){return parser(st,sc,k) };
var parser = do2(char_("a"),parserPlus(rec,return_("")));
run(parser, st, console.dir, true)  //if the last arg is true, then it will use timeouts, and will stop before the limit
/**/
x = x && x.func && x.func.apply(null, x.args ||[]); //comment out above and execute the last few thunks
st;