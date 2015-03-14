
---



**This project has been moved to: http://code.google.com/p/jshaskell/**



---




The initial implementation of some combinators and the memoization is derived from:
http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html

Functions with similar names as in Parsec should work correspondingly: http://hackage.haskell.org/package/parsec

The [jsparsec-javascript](http://code.google.com/p/jsparsec/source/browse/#svn/trunk/jsparsec-javascript) module is based entirely on the [WebBits](http://hackage.haskell.org/package/WebBits-2.0) library.


---


# Features #

  * due to tail call optimization recursive parsers work in infinite depth, and long running computations can be automatically interrupted with setTimeout, so that it won't block the browser UI
  * there's do-notation with variable binding
  * Haskell-like expressions (called "array-expressions")
    * operators with fixity, grouping via square brackets
    * -> applicative style parsing
  * algebraic data types with some type checking (see [r11](https://code.google.com/p/jsparsec/source/detail?r=11))
  * alternative recursion syntaxes for parsers
  * a [callstream](http://dbj.org/dbj/?p=514) syntax that directly accepts array-expressions and provides a nice and efficient way to express self-recursion
  * optional caching of the results of combinators and the corresponding ParseState index
  * error messages including position, expected/unexpected tokens and multiple choices

# API #
  * The underscores are needed for names that would be otherwise reserved words in JavaScript. Functions listed in parens are exactly the same. As of [r50](https://code.google.com/p/jsparsec/source/detail?r=50) `fmap` no longer curries functions, so you have to do that manually: `[curry(add2) ,"<$>", digit ,"<*>", digit]` but `liftA2(add2, digit, digit)` works too.
  * generic functions: `id, const_, curry, call ($), cons (:), consJoin (if the result is an array then joins it), sequence, replicate, nub, sort, maybe, compare, digitToInt, zip, fst, snd, uncurry, negate, null_, compose1 (.), compose, elem, isArray, range`
  * ADTs: `Maybe, Either, Ordering` (with a "show instance" automatically derived by inheriting a `toString` function from the `ADT` class/function)
  * **Parsec.Prim**:
    * `parsecMap (fmap, liftM, liftA, <$>), ap, liftA2 (liftM2), liftA3 (liftM3), parserReturn (pure, return_), parserBind (>>=), parserZero (mzero, empty), parserFail (fail), >>, =<<, *>, <*, <$, <**>`
    * `tokens, parserPlus (mplus, <|>), try_, skipMany, satisfy, many, label (<?>), unexpected`
  * other functions: `do_, bind (<-), ret, run, ParseState, ps (= new ParseState), toParser`
  * **Parsec.Char** (complete): `char_, string, alphaNum, anyChar, digit, hexDigit, label, letter, lower, newline, noneOf, octDigit, oneOf, space, spaces, tab, upper`
  * other combinators: `match` (it's a different, faster `string`, and accepts a regex as well)
  * **Parsec.Combinator** (complete): `choice, between, option, optional, sepBy, sepBy1, endBy, endBy1, sepEndBy, sepEndBy1, chainl, chainl1, chainr, chainr1, manyTill, lookAhead, count, many1, skipMany1, notFollowedBy, eof, anyToken, optionMaybe`
  * **Parsec.Token** and **Parsec.Language** (complete): instead of `haskell` and `mondrian` use `getHaskell()` and `getMondrian()`
  * **Parsec.Expr** (complete): `Assoc, Operator, buildExpressionParser`
  * `resolve` and `Array.prototype.resolve` (and `toParser`) is used for evaluating expressions

  * **jsparsec-javascript**: the `parseScript` parser can be used for parsing arbitrary JavaScript code

# Usage #

```
JSParsec.extend(window, JSParsec); //this makes all functions global

run(parser, input, complete [, error [, async]]);
//where the input can be either text or a ParseState object
//the complete callback will be always called with the final result
//the error callback receives only an error message, and only if the parser failed
//if async is true then there will be a 1 millisecond timeout
//  after every 500 function call, so that it won't block the browser for too long
```

```

var chars = do_(
many(char_("_")),
["x", "<-", try_, [string, "aa"], "<|>", string, "ab"].resolve(),
ret("x")
);

function p_(st, sc, k){return p(st, sc, k)} //needed for late binding

var p = do_(
char_("("),
bind("x", chars),
bind("xs", parserPlus(p_, return_([]))),
char_(")"),
returnCall(cons, "x", "xs")
);

run(p, "(__ab(ab(_ab)))", console.dir );
```

You can also use the so-called [callstream](http://dbj.org/dbj/?p=514) syntax, which accepts array-expressions directly and with the `recurse` function you can make a parser self-recursive:

```

var chars = cs
  (many, char_("_"))
  ("x", "<-", try_, [string, "aa"], "<|>", string, "ab")
  (ret, "x");


var p = cs
  (char_, "(")
  ("x", "<-", chars )
  ("xs", "<-", recurse ,"<|>", return_([]))
  (char_, ")")
  (returnCall, cons, "x", "xs"); //cons concatenates strings (or unshifts an array)

run(p, "(__ab(ab(_ab)))", console.dir );

//the last line can be also written like this: `(returnCall, [cons, "x", "xs"])`
//but `return_,[]` wouldn't work because square brackets are for grouping expressions
//that's why it has to be called directly: `return_([])`
```

And there's even another method of defining recursive parsers, which adds some runtime overhead but it's somewhat easier to read or write than the first one.
Since [r28](https://code.google.com/p/jsparsec/source/detail?r=28) this is no longer supported but might be added later.
```
// `function() expr` means `function(){ return expr }` in Firefox
var p = function() do_(
char_("("),
bind("x", chars),
bind("xs", parserPlus(p, return_([]))),
char_(")"),
returnCall(cons, "x", "xs")
);
```


---


You can enable caching with
`ParseState.prototype.memoize = true;`
but occasionally it crashed Firefox for me during testing, so that might still happen. This might be useful if some part of the text is parsed multiple times (<|>) or if you want to run multiple parsers on the same string.

If you want to do the latter then instead of a string you have to apply the same ParseState object to each of the parsers:
```
var state = ps("some text"); //or: new ParseState("some text");
run(p1, state);
run(p2, state.scrollTo(0));
```