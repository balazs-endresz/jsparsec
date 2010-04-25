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

