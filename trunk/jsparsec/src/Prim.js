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
    
    dropped: 0, //TODO: cut input periodically if there's no try_

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
        return 'Unexpected "' + (unexpMsg.length ? unexpMsg : "end of input") +  
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
            return result.success ? p2(state, scope, k) : k(result); //TODO: p2
        }]};
    }
    fn.constructor = Parser;
    return fn;
};

//TODO: foldl
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
                else
                    result.ast = undef;
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
    result.ast = undef;
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
        result = extend({}, result);
        result.ast = result.ast.join("");
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
        return f(scope)(state, scope, k);
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
