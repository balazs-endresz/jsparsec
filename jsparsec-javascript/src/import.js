//import functions from JSParsec

/*
var ns = [];
for(var name in JSParsec)
    ns.push(name);

ns = JSParsec.map(function(name){
    return "var " + name + " = JSParsec." + name;
}, ns).join(";");

eval(ns);
*/


var
curry = JSParsec.curry,
const_ = JSParsec.const_,
isArray = JSParsec.isArray,
isDefined = JSParsec.isDefined,
slice = JSParsec.slice,
foldl = JSParsec.foldl,
foldr = JSParsec.foldr,
map = JSParsec.map,
filter = JSParsec.filter,
indexOf = JSParsec.indexOf,
lastIndexOf = JSParsec.lastIndexOf,
zip = JSParsec.zip,
sort = JSParsec.sort,
nub = JSParsec.nub,
maybe = JSParsec.maybe,
compare = JSParsec.compare,
compose = JSParsec.compose,
compose1 = JSParsec.compose1,
call = JSParsec.call,
id = JSParsec.id,
flip = JSParsec.flip,
cons = JSParsec.cons,
consJoin = JSParsec.consJoin,
replicate = JSParsec.replicate,
negate = JSParsec.negate,
null_ = JSParsec.null_,
elem = JSParsec.elem,
isSpace = JSParsec.isSpace,
isUpper = JSParsec.isUpper,
isLower = JSParsec.isLower,
isAlpha = JSParsec.isAlpha,
isAlphaNum = JSParsec.isAlphaNum,
isDigit = JSParsec.isDigit,
isHexDigit = JSParsec.isHexDigit,
isOctDigit = JSParsec.isOctDigit,
digitToInt = JSParsec.digitToInt,
range = JSParsec.range,
extend = JSParsec.extend,
namespace = JSParsec.namespace,
toInteger = JSParsec.toInteger,
fromInteger = JSParsec.fromInteger,
fromIntegral = JSParsec.fromIntegral,
fst = JSParsec.fst,
snd = JSParsec.snd,
uncurry = JSParsec.uncurry,
lookup = JSParsec.lookup,
readHex = JSParsec.readHex,
readOct = JSParsec.readOct,
chr = JSParsec.chr,
round = JSParsec.round,
data = JSParsec.data,
ADT = JSParsec.ADT,
record = JSParsec.record,
Maybe = JSParsec.Maybe,
Ordering = JSParsec.Ordering,
Either = JSParsec.Either,
operators = JSParsec.operators,
infix = JSParsec.infix,
infixl = JSParsec.infixl,
infixr = JSParsec.infixr,
arr = JSParsec.arr,
op = JSParsec.op,
str = JSParsec.str,
ex = JSParsec.ex,
resolve = JSParsec.resolve,
recurse = JSParsec.recurse,
Recurse = JSParsec.Recurse,
cs = JSParsec.cs,
sequence = JSParsec.sequence,
run = JSParsec.run,
Parser = JSParsec.Parser,
ParseState = JSParsec.ParseState,
ps = JSParsec.ps,
toParser = JSParsec.toParser,
unexpected = JSParsec.unexpected,
parsecMap = JSParsec.parsecMap,
fmap = JSParsec.fmap,
liftM = JSParsec.liftM,
liftM2 = JSParsec.liftM2,
liftM3 = JSParsec.liftM3,
liftA = JSParsec.liftA,
liftA2 = JSParsec.liftA2,
liftA3 = JSParsec.liftA3,
ap = JSParsec.ap,
parserBind = JSParsec.parserBind,
parserReturn = JSParsec.parserReturn,
return_ = JSParsec.return_,
pure = JSParsec.pure,
parserFail = JSParsec.parserFail,
fail = JSParsec.fail,
parserZero = JSParsec.parserZero,
mzero = JSParsec.mzero,
empty = JSParsec.empty,
parserPlus = JSParsec.parserPlus,
parserPlusN = JSParsec.parserPlusN,
mplus = JSParsec.mplus,
do_ = JSParsec.do_,
do2 = JSParsec.do2,
bind = JSParsec.bind,
ret = JSParsec.ret,
withBound = JSParsec.withBound,
returnCall = JSParsec.returnCall,
lazy = JSParsec.lazy,
getPosition = JSParsec.getPosition,
setPosition = JSParsec.setPosition,
getParserState = JSParsec.getParserState,
setParserState = JSParsec.setParserState,
tokens = JSParsec.tokens,
many = JSParsec.many,
many1 = JSParsec.many1,
string = JSParsec.string,
char_ = JSParsec.char_,
satisfy = JSParsec.satisfy,
label = JSParsec.label,
try_ = JSParsec.try_,
skipMany = JSParsec.skipMany,
match = JSParsec.match,
oneOf = JSParsec.oneOf,
noneOf = JSParsec.noneOf,
space = JSParsec.space,
spaces = JSParsec.spaces,
newline = JSParsec.newline,
tab = JSParsec.tab,
upper = JSParsec.upper,
lower = JSParsec.lower,
alphaNum = JSParsec.alphaNum,
letter = JSParsec.letter,
digit = JSParsec.digit,
hexDigit = JSParsec.hexDigit,
octDigit = JSParsec.octDigit,
anyChar = JSParsec.anyChar,
choice = JSParsec.choice,
count = JSParsec.count,
between = JSParsec.between,
option = JSParsec.option,
optionMaybe = JSParsec.optionMaybe,
optional = JSParsec.optional,
skipMany1 = JSParsec.skipMany1,
sepBy = JSParsec.sepBy,
sepBy1 = JSParsec.sepBy1,
endBy = JSParsec.endBy,
endBy1 = JSParsec.endBy1,
sepEndBy = JSParsec.sepEndBy,
sepEndBy1 = JSParsec.sepEndBy1,
chainl = JSParsec.chainl,
chainl1 = JSParsec.chainl1,
chainr = JSParsec.chainr,
chainr1 = JSParsec.chainr1,
eof = JSParsec.eof,
notFollowedBy = JSParsec.notFollowedBy,
manyTill = JSParsec.manyTill,
lookAhead = JSParsec.lookAhead,
anyToken = JSParsec.anyToken,
GenLanguageDef = JSParsec.GenLanguageDef,
GenTokenParser = JSParsec.GenTokenParser,
makeTokenParser = JSParsec.makeTokenParser,
emptyDef = JSParsec.emptyDef,
haskellStyle = JSParsec.haskellStyle,
javaStyle = JSParsec.javaStyle,
haskellDef = JSParsec.haskellDef,
mondrianDef = JSParsec.mondrianDef,
getHaskell = JSParsec.getHaskell,
getMondrian = JSParsec.getMondrian,
Assoc = JSParsec.Assoc,
Operator = JSParsec.Operator,
buildExpressionParser = JSParsec.buildExpressionParser
;
