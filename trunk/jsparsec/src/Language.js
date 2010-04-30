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
               , identStart     : [letter   ,"<|>", char_, '_'].resolve()
               , identLetter    : [alphaNum ,"<|>", oneOf, "_'"].resolve()
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
	        { identLetter   : [haskell98Def.identLetter ,"<|>", char_, '#'].resolve()
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
