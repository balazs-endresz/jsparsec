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
            prefixOp   = label(choice(prefix), ""),
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
            (ret, function(scope){ return scope.post(scope.pre(scope.x)) }).resolve();
        
        
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