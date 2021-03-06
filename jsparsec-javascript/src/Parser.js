//module BrownPLT.JavaScript.Parser
//  (parseScript
//  , parseExpression
//  , parseString
//  , parseScriptFromString
//  , emptyParsedJavaScript
//  , ParsedStatement
//  , ParsedExpression
//  , parseJavaScriptFromFile
//  , parseSimpleExpr'
//  , parseBlockStmt
//  , parseStatement
//  , StatementParser
//  , ExpressionParser
//  , assignExpr
//  ) where
//
//import BrownPLT.JavaScript.Lexer hiding (identifier)
//import qualified BrownPLT.JavaScript.Lexer as Lexer
//import BrownPLT.JavaScript.Syntax
//import Text.ParserCombinators.Parsec
//import Text.ParserCombinators.Parsec.Expr
//import Control.Monad(liftM,liftM2)
//import Control.Monad.Trans (MonadIO,liftIO)
//import Numeric(readDec,readOct,readHex)
//import Data.Char(chr)
//import Data.Char

/*

Changes to the original code:
 * added octal number parser (octLit, parseNumLit)
 * parseScript: parseStatement `sepBy` whitespace -> many(parseStatement)
   statements consume all trailing whitespace
 * Lexer: reservedOpNames and reservedNames was switched,
   removed "</" and added "in" (since "instanceof" was already there)
 * parseIfStmt: removed optional semicolon
 * parseContinueStmt: added optional semicolon
 * added automatic semicolon insertion for throw and return
 * unaryAssignExpr: added automatic semicolon insertion
   before prefix ++ or -- preceeded by a newline:
        //var a = 10, b = 3;
        a
        ++b
   actual: [VarRef "a", PrefixInc (VarRef "b")]
   original: [PostfixInc (VarRef "a"), VarRef "b"]
 * ASI with multiline comments is not consistent
 * optinal semicolons have been replaced with autoEndStmt:
   instead of optional(lex.semi) it checks the consumed(!) whitespace for a
   newline, if there's none then for a semi, closing brace, or eof
   
TODO:
 * throw should be followed by an expression (on the same line), i.e.
   "throw;" or "throw \n error;" is not allowed, see `onSameLine1`
 * future reserved names (?)
 * function declaration in blocks (implementation dependent)
    if(true)
        function f(){} 
   function expressions without name as statements
    function(){}    //Syntax error
    (function(){})  //OK
   ExpressionStatements cannot start with "function" or "{" (spec 12.4)
   FunctionDeclaration as Statements (spec 12.0)
   add SourceElement: Statement or  FunctionDeclaration
   add FunctionBody: SourceElements
   
*/

var parseListExpr   = lazy(function(){ return parseListExpr   });
var parseStatement  = lazy(function(){ return parseStatement  });
var parseParenExpr  = lazy(function(){ return parseParenExpr  });
var parseNewExpr    = lazy(function(){ return parseNewExpr    });
var parseExpression = lazy(function(){ return parseExpression });
var assignExpr = parseExpression;


//chr.fst.head.readHex
function toUnicodeString(charArrayOrStr){
    return chr(readHex(charArrayOrStr));
    //var nums = charArrayOrStr.join ? charArrayOrStr.join("") : charArrayOrStr;
    //return String.fromCharCode(parseInt(nums, 16));
}

//\fst snd -> (chr.fst.head.readHex) (fst:snd:"")
function toAsciiString(fst, snd){
    return chr(readHex("" + fst + snd));
    //return String.fromCharCode(parseInt("" + fst + snd, 16));
}

function onSameLine(pos0, pos1, parser){
    return function(state, scope, k){
        return ((state.sourceLine(scope[pos0]) == state.sourceLine(scope[pos1])) ? 
                parserPlus(liftM(Maybe.Just, parser), return_(Maybe.Nothing)) :
                return_(Maybe.Nothing))
            (state, scope, k);
    }
}

//this is used with parseThrowStmt,
//but the error message appears in the beginning of the enclosing statement
//if followed by a semicolon, else if there's a new line it's skipped entirely!
function onSameLine1(pos0, pos1, parser){
    return function(state, scope, k){
        return ((state.sourceLine(scope[pos0]) == state.sourceLine(scope[pos1])) ? 
                parser :
                unexpected("end of line"))
            (state, scope, k);
    }
}


function autoEndStmt(state, scope, k){
    var ws = state.input.substring(0, state.index - state.dropped).match(/\s*$/),
        ended = ws && /\n|\r/.test(ws[0]),
        semi;
        
    if(!ended){
        var nextChar = state.at(0);
        if(nextChar == "" || nextChar == "}"){
            ended = true;
        }else if(nextChar == ";"){
            ended = true;
            semi = true;
        }
    }
    
    return (ended ?
                (semi ? lex.semi : return_(null)) :
                fail("end of statement"))
        (state, scope, k);
}



//-- We parameterize the parse tree over source-locations.
//type ParsedStatement = Statement SourcePos
//type ParsedExpression = Expression SourcePos


//-- These parsers can store some arbitrary state
//type StatementParser state = CharParser state ParsedStatement
//type ExpressionParser state = CharParser state ParsedExpression

//identifier =
//  liftM2 Id getPosition Lexer.identifier
var identifier = liftM2(Id.Id, getPosition, lex.identifier);


//--{{{ Statements
//
//-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
//-- consume any input on failure.  Note that all statements (expect for labelled
//-- and expression statements) begin with a reserved-word.  If we fail to parse
//-- this reserved-word, no input is consumed.  Hence, we can have the massive or
//-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
//-- must be whatever statement the reserved-word indicates.  If we fail after the
//-- reserved-word, we truly have a syntax error.  Since input has been consumed,
//-- <|> will not try its alternate in parseExpression, and we will fail.

//parseIfStmt:: StatementParser st  
//parseIfStmt = do
//  pos <- getPosition
//  reserved "if"
//  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
//  consequent <- parseStatement <?> "true-branch of if statement"
//  optional semi -- TODO: in spec?
//  ((do reserved "else"
//       alternate <- parseStatement
//       return (IfStmt pos test consequent alternate))
//    <|> return (IfSingleStmt pos test consequent))

var parseIfStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "if")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in if statement")
  ("consequent" ,"<-", parseStatement ,"<?>", "true-branch of if statement")
  //the empty statement matches a single semi, but one more would be 
  //a syntax error if it's followed by `else`
  //(optional, lex.semi) //-- TODO: in spec?
  (   cs(lex.reserved, "else")
        ("alternate" ,"<-", parseStatement)
        (ret, function(scope){
            return Statement.IfStmt(scope.scope.pos, scope.scope.test,
                                    scope.scope.consequent, scope.alternate);
        })
    ,"<|>", returnCall(Statement.IfSingleStmt, "pos", "test", "consequent")
  )


//parseSwitchStmt:: StatementParser st
//parseSwitchStmt =
//  let parseDefault = do
//        pos <- getPosition
//        reserved "default"
//        colon
//        statements <- many parseStatement
//        return (CaseDefault pos statements)
//      parseCase = do
//        pos <- getPosition
//        reserved "case"
//        condition <- parseListExpr
//        colon
//        actions <- many parseStatement
//        return (CaseClause pos condition actions)
//  in do pos <- getPosition
//        reserved "switch"
//        test <- parseParenExpr
//        clauses <- braces $ many $ parseDefault <|> parseCase
//        return (SwitchStmt pos test clauses)
var _parseDefault = cs
  ("pos" ,"<-", getPosition)
  (lex.reserved, "default")
  (lex.colon)
  ("statements" ,"<-", many, parseStatement)
  (returnCall, CaseClause.CaseDefault, "pos",  "statements")
  
var _parseCase = cs
  ("pos" ,"<-", getPosition)
  (lex.reserved, "case")
  ("condition" ,"<-", parseListExpr)
  (lex.colon)
  ("actions"   ,"<-", many, parseStatement)
  (returnCall, CaseClause.CaseClause, "pos", "condition", "actions")
  
var parseSwitchStmt = cs
  ("pos"     ,"<-", getPosition)
  (lex.reserved, "switch")
  ("test"    ,"<-", parseParenExpr)
  ("clauses" ,"<-", lex.braces ,"$", many ,"$", _parseDefault ,"<|>", _parseCase)
  (returnCall, Statement.SwitchStmt, "pos", "test", "clauses")

//parseWhileStmt:: StatementParser st
//parseWhileStmt = do
//  pos <- getPosition
//  reserved "while"
//  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
//  body <- parseStatement
//  return (WhileStmt pos test body)
var parseWhileStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "while")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in while loop")
  ("body" ,"<-", parseStatement)
  (returnCall, Statement.WhileStmt, "pos", "test", "body")


//parseBlockStmt:: StatementParser st
//parseBlockStmt = do
//  pos <- getPosition
//  statements <- braces (many parseStatement)
//  return (BlockStmt pos statements)
var parseBlockStmt = cs
  ("pos" ,"<-", getPosition)
  ("statements" ,"<-", lex.braces, [many, parseStatement])
  (returnCall, Statement.BlockStmt, "pos", "statements")


//parseDoWhileStmt:: StatementParser st
//parseDoWhileStmt = do
//  pos <- getPosition
//  reserved "do"
//  body <- parseBlockStmt
//  reserved "while" <?> "while at the end of a do block"
//  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
//  optional semi
//  return (DoWhileStmt pos body test)
var parseDoWhileStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "do")
  ("body" ,"<-", parseBlockStmt)
  (lex.reserved, "while" ,"<?>", "while at the end of a do block")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in do loop")
  (autoEndStmt)
  (returnCall, Statement.DoWhileStmt, "pos", "body", "test")


//parseContinueStmt:: StatementParser st
//parseContinueStmt = do
//  pos <- getPosition
//  reserved "continue"
//  pos' <- getPosition
//  -- Ensure that the identifier is on the same line as 'continue.'
//  id <- (if (sourceLine pos == sourceLine pos')
//           then (liftM Just identifier) <|> (return Nothing)
//           else return Nothing)
//  return (ContinueStmt pos id)
var parseContinueStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "continue")
  ("pos_" ,"<-", getPosition)
  // Ensure that the identifier is on the same line as 'continue.'
  ("id"   ,"<-", onSameLine, "pos", "pos_", identifier)
  (autoEndStmt)
  (returnCall, Statement.ContinueStmt, "pos", "id")


//parseBreakStmt:: StatementParser st
//parseBreakStmt = do
//  pos <- getPosition
//  reserved "break"
//  pos' <- getPosition
//  -- Ensure that the identifier is on the same line as 'break.'
//  id <- (if (sourceLine pos == sourceLine pos')
//           then (liftM Just identifier) <|> (return Nothing)
//           else return Nothing)
//  optional semi           
//  return (BreakStmt pos id)
var parseBreakStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "break")
  ("pos_" ,"<-", getPosition)
  // Ensure that the identifier is on the same line as 'break.'
  ("id"   ,"<-", onSameLine, "pos", "pos_", identifier)
  (autoEndStmt)
  (returnCall, Statement.BreakStmt, "pos", "id")


//parseEmptyStmt:: StatementParser st 
//parseEmptyStmt = do
//  pos <- getPosition
//  semi
//  return (EmptyStmt pos)
var parseEmptyStmt = cs
  ("pos" ,"<-", getPosition)
  (lex.semi)
  (returnCall, Statement.EmptyStmt, "pos")


//parseLabelledStmt:: StatementParser st
//parseLabelledStmt = do
//  pos <- getPosition
//  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
//  -- for an expression statement.
//  label <- try (do label <- identifier
//                   colon
//                   return label)
//  statement <- parseStatement
//  return (LabelledStmt pos label statement)
var parseLabelledStmt = cs
  ("pos"   ,"<-", getPosition)
  // Lookahead for the colon.  If we don't see it, we are parsing an identifier
  // for an expression statement.
  ("label" ,"<-", try_, cs("label" ,"<-", identifier)
                          (lex.colon)
                          (ret, "label")
  )
  ("statement" ,"<-", parseStatement)
  (returnCall, Statement.LabelledStmt, "pos", "label", "statement")


//parseExpressionStmt:: StatementParser st
//parseExpressionStmt = do
//  pos <- getPosition
//  expr <- parseListExpr -- TODO: spec 12.4?
//  optional semi
//  return (ExprStmt pos expr)
var parseExpressionStmt = cs
  ("pos"  ,"<-", getPosition)
  ("expr" ,"<-", parseListExpr) // TODO: spec 12.4?
  (autoEndStmt)
  (returnCall, Statement.ExprStmt, "pos", "expr")


//parseForInStmt:: StatementParser st
//parseForInStmt =
//  let parseInit = (reserved "var" >> liftM ForInVar identifier)
//                  <|> (liftM ForInNoVar identifier)
//    in do pos <- getPosition
//          -- Lookahead, so that we don't clash with parseForStmt
//          (init,expr) <- try (do reserved "for"
//                                 parens (do init <- parseInit
//                                            reserved "in"
//                                            expr <- parseExpression
//                                            return (init,expr)))
//          body <- parseStatement
//          return (ForInStmt pos init expr body) 
var _parseInit = ex([lex.reserved, "var" ,">>", liftM, ForInInit.ForInVar, identifier]
    ,"<|>", [liftM, ForInInit.ForInNoVar, identifier]);

var parseForInStmt = cs
  ("pos" ,"<-", getPosition)
   // Lookahead, so that we don't clash with parseForStmt
  ("init_expr" ,"<-", try_, cs (lex.reserved, "for")
                               (lex.parens, cs("init" ,"<-", _parseInit)
                                          (lex.reserved, "in")
                                          ("expr" ,"<-", parseExpression)
                                          (ret, function(scope){ return [scope.init, scope.expr] })
                                )
  )
  ("body" ,"<-", parseStatement)
  (ret, function(scope){
    return Statement.ForInStmt(scope.pos, scope.init_expr[0], scope.init_expr[1], scope.body);
  })

//parseVarDecl = do
//  pos <- getPosition
//  id <- identifier
//  init <- (reservedOp "=" >> liftM Just parseExpression) <|> (return Nothing)
//  return (VarDecl pos id init)
var parseVarDecl = cs
  ("pos"  ,"<-", getPosition)
  ("id"   ,"<-", identifier)
  ("init" ,"<-", [lex.reservedOp("=") ,">>", liftM, Maybe.Just, parseExpression]
                  ,"<|>", return_(Maybe.Nothing))
  (returnCall, VarDecl.VarDecl, "pos", "id", "init")


//parseVarDeclStmt:: StatementParser st
//parseVarDeclStmt = do 
//  pos <- getPosition
//  reserved "var"
//  decls <- parseVarDecl `sepBy` comma
//  optional semi
//  return (VarDeclStmt pos decls)
var parseVarDeclStmt = cs 
  ("pos" ,"<-", getPosition)
  (lex.reserved, "var")
  ("decls" ,"<-", parseVarDecl, op(sepBy), lex.comma)
  (autoEndStmt)
  (returnCall, Statement.VarDeclStmt, "pos",  "decls")

//parseForStmt:: StatementParser st
//parseForStmt =
//  let parseInit =
//        (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma)) <|>
//        (liftM ExprInit parseListExpr) <|>
//        (return NoInit)
//    in do pos <- getPosition
//          reserved "for"
//          reservedOp "("
//          init <- parseInit
//          semi
//          test <- (liftM Just parseExpression) <|> (return Nothing)
//          semi
//          iter <- (liftM Just parseListExpr) <|> (return Nothing)
//          reservedOp ")" <?> "closing paren"
//          stmt <- parseStatement
//          return (ForStmt pos init test iter stmt)
var _parseInit2 = ex(
    [lex.reserved, "var" ,">>", liftM, ForInit.VarInit, [parseVarDecl ,op(sepBy), lex.comma]]
    ,"<|>", [liftM, ForInit.ExprInit, parseListExpr]
    ,"<|>", return_(ForInit.NoInit)
  );
  
var parseForStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "for")
  (lex.reservedOp("("))
  ("init" ,"<-", _parseInit2)
  (lex.semi)
  ("test" ,"<-", [liftM, Maybe.Just, parseExpression] ,"<|>", return_(Maybe.Nothing))
  (lex.semi)
  ("iter" ,"<-", [liftM, Maybe.Just, parseListExpr] ,"<|>", return_(Maybe.Nothing))
  (lex.reservedOp(")") ,"<?>", "closing paren")
  ("stmt" ,"<-", parseStatement)
  (returnCall, Statement.ForStmt, "pos", "init", "test", "iter", "stmt")

//parseTryStmt:: StatementParser st
//parseTryStmt =
//  let parseCatchClause = do
//        pos <- getPosition
//        reserved "catch"
//        id <- parens identifier
//        stmt <- parseStatement
//        return (CatchClause pos id stmt)
//    in do reserved "try"
//          pos <- getPosition
//          guarded <- parseStatement
//          catches <- many parseCatchClause
//          finally <- (reserved "finally" >> liftM Just parseStatement) 
//                      <|> (return Nothing)
//          return (TryStmt pos guarded catches finally)
var _parseCatchClause = cs
    ("pos"  ,"<-", getPosition)
    (lex.reserved, "catch")
    ("id"   ,"<-", lex.parens, identifier)
    ("stmt" ,"<-", parseStatement)
    (returnCall, CatchClause.CatchClause, "pos", "id", "stmt")
            
var parseTryStmt =cs
  (lex.reserved, "try")
  ("pos"     ,"<-", getPosition)
  ("guarded" ,"<-", parseStatement)
  ("catches" ,"<-", many, _parseCatchClause)
  ("finally" ,"<-", [lex.reserved, "finally" ,">>", liftM, Maybe.Just, parseStatement]
               ,"<|>", return_(Maybe.Nothing))
  (returnCall, Statement.TryStmt, "pos", "guarded", "catches", "finally")


//parseThrowStmt:: StatementParser st
//parseThrowStmt = do
//  pos <- getPosition
//  reserved "throw"
//  expr <- parseExpression
//  optional semi
//  return (ThrowStmt pos expr)
var parseThrowStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "throw")
  ("pos_" ,"<-", getPosition)
  ("expr" ,"<-", onSameLine1, "pos", "pos_", parseExpression)
  (autoEndStmt)
  (returnCall, Statement.ThrowStmt, "pos", "expr")


//parseReturnStmt:: StatementParser st
//parseReturnStmt = do
//  pos <- getPosition
//  reserved "return"
//  expr <- (liftM Just parseListExpr) <|> (return Nothing)
//  optional semi
//  return (ReturnStmt pos expr)
var parseReturnStmt = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "return")
  ("pos_" ,"<-", getPosition)
  ("expr" ,"<-", onSameLine, "pos", "pos_", parseListExpr)
  (autoEndStmt)
  (returnCall, Statement.ReturnStmt, "pos", "expr")


//parseWithStmt:: StatementParser st
//parseWithStmt = do
//  pos <- getPosition
//  reserved "with"
//  context <- parseParenExpr
//  stmt <- parseStatement
//  return (WithStmt pos context stmt)
var parseWithStmt = cs
  ("pos"     ,"<-", getPosition)
  (lex.reserved, "with")
  ("context" ,"<-", parseParenExpr)
  ("stmt"    ,"<-", parseStatement)
  (returnCall, Statement.WithStmt, "pos", "context", "stmt")


//parseFunctionStmt:: StatementParser st
//parseFunctionStmt = do
//  pos <- getPosition
//  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
//  args <- parens (identifier `sepBy` comma)
//  body <- parseBlockStmt <?> "function body in { ... }"
//  return (FunctionStmt pos name args body)
var parseFunctionStmt = cs
  ("pos"  ,"<-", getPosition)
  ("name" ,"<-", try_, [lex.reserved, "function", ">>", identifier]) // ambiguity with FuncExpr
  ("args" ,"<-", lex.parens, [identifier ,op(sepBy), lex.comma])
  ("body" ,"<-", parseBlockStmt ,"<?>", "function body in { ... }")
  (returnCall, Statement.FunctionStmt, "pos", "name", "args", "body")


//parseStatement:: StatementParser st
//parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
//  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
//  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
//  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
//  <|> parseVarDeclStmt  <|> parseFunctionStmt
//  -- labelled, expression and the error message always go last, in this order
//  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"
parseStatement = ex(
            parseIfStmt
    ,"<|>", parseSwitchStmt
    ,"<|>", parseWhileStmt
    ,"<|>", parseDoWhileStmt
    ,"<|>", parseContinueStmt
    ,"<|>", parseBreakStmt
    ,"<|>", parseBlockStmt
    ,"<|>", parseEmptyStmt
    ,"<|>", parseForInStmt
    ,"<|>", parseForStmt
    ,"<|>", parseTryStmt
    ,"<|>", parseThrowStmt
    ,"<|>", parseReturnStmt
    ,"<|>", parseWithStmt
    ,"<|>", parseVarDeclStmt
    ,"<|>", parseFunctionStmt 
    // labelled, expression and the error message always go last, in this order
    ,"<|>", parseLabelledStmt 
    ,"<|>", parseExpressionStmt
    ,"<?>", "statement"
);

//--{{{ Expressions

//-- References used to construct this stuff:
//-- + http://developer.mozilla.org/en/docs/
//--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
//-- + http://www.mozilla.org/js/language/grammar14.html
//--
//-- Aren't expression tables nice?  Well, we can't quite use them, because of 
//-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
//-- We use one expression table for the assignment operators that bind looser 
//-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
//-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
//-- production for ?:, and is defined over the second expression table, 
//-- exprTable, which consists of operators that bind tighter than ?:.  The terms
//-- of exprTable are atomic expressions, parenthesized expressions, functions and
//-- array references.

//--{{{ Primary expressions

//parseThisRef:: ExpressionParser st
//parseThisRef = do
//  pos <- getPosition
//  reserved "this"
//  return (ThisRef pos)
var parseThisRef = cs
  ("pos" ,"<-", getPosition)
  (lex.reserved, "this")
  (returnCall, Expression.ThisRef, "pos")
//ex(Expression.ThisRef ,"<$>", [getPosition ,"<*", lex.reserved, "this"])

//parseNullLit:: ExpressionParser st
//parseNullLit = do
//  pos <- getPosition
//  lex.reserved "null"
//  return (NullLit pos)
var parseNullLit = cs
  ("pos" ,"<-", getPosition)
  (lex.reserved, "null")
  (returnCall, Expression.NullLit, "pos")
//ex(Expression.NullLit ,"<$>", [getPosition ,"<*", lex.reserved, "null"])


//parseBoolLit:: ExpressionParser st
//parseBoolLit = do
//    pos <- getPosition
//    let parseTrueLit  = reserved "true"  >> return (BoolLit pos True)
//        parseFalseLit = reserved "false" >> return (BoolLit pos False)
//    parseTrueLit <|> parseFalseLit
var parseBoolLit = cs
  ("pos"   ,"<-", getPosition)
  ("true"  ,"<-", return_, true)
  ("false" ,"<-", return_, false)
  ([lex.reserved, "true"  ,">>", returnCall(Expression.BoolLit, "pos", "true")]
   ,"<|>",
   [lex.reserved, "false" ,">>", returnCall(Expression.BoolLit, "pos", "false")]
  )


//parseVarRef:: ExpressionParser st
//parseVarRef = liftM2 VarRef getPosition identifier
var parseVarRef = liftM2(Expression.VarRef, getPosition, identifier);


//parseArrayLit:: ExpressionParser st
//parseArrayLit = liftM2 ArrayLit getPosition (squares (parseExpression `sepEndBy` comma))
var parseArrayLit = liftM2(Expression.ArrayLit, getPosition, lex.squares(sepEndBy(parseExpression, lex.comma)));


//parseFuncExpr = do
//  pos <- getPosition
//  reserved "function"
//  name <- (identifier >>= return . Just) <|> return Nothing
//  args <- parens (identifier `sepBy` comma)
//  body <- parseBlockStmt
//  return $ FuncExpr pos name args body
var parseFuncExpr = cs
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "function")
  ("name" ,"<-", [identifier, ">>=", return_ ,".", Maybe.Just] ,"<|>", return_, Maybe.Nothing)
  ("args" ,"<-", lex.parens, [identifier ,op(sepBy), lex.comma])
  ("body" ,"<-", parseBlockStmt)
  (returnCall, Expression.FuncExpr, "pos", "name", "args", "body")


//--{{{ parsing strings

//escapeChars =
// [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
//  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]
var escapeChars =
 [['\'','\''],['\"','\"'],['\\','\\'],['b','\b'],['f','\f'],['n','\n'],
  ['r','\r'],['t','\t'],['v','\v'],['/','/'],[' ',' '],['0','\0']];


//allEscapes:: String
//allEscapes = map fst escapeChars
var allEscapes = map(fst, escapeChars);


//parseEscapeChar = do
//  c <- oneOf allEscapes
//  let (Just c') = lookup c escapeChars -- will succeed due to line above
//  return c'
var parseEscapeChar = cs
  ("c" ,"<-", oneOf(allEscapes))
  (ret, function(scope){
    return lookup(scope.c, escapeChars)[0]; // will succeed due to line above
  })

//parseAsciiHexChar = do
//  char 'x'
//  d1 <- hexDigit
//  d2 <- hexDigit
//  return ((chr.fst.head.readHex) (d1:d2:""))
var parseAsciiHexChar = cs
  (char_, 'x')
  ("d1" ,"<-", hexDigit)
  ("d2" ,"<-", hexDigit)
  (returnCall, toAsciiString, "d1", "d2")


//parseUnicodeHexChar = do
//  char 'u'
//  liftM (chr.fst.head.readHex) 
//        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
var parseUnicodeHexChar = cs
  (char_('u'))
  (liftM, toUnicodeString, sequence([hexDigit, hexDigit, hexDigit, hexDigit]))


//isWhitespace ch = ch `elem` " \t"
function isWhitespace(ch){ return (ch == " ") || (ch == "\t") }


//-- The endWith argument is either single-quote or double-quote, depending on how
//-- we opened the string.
//parseStringLit' endWith =
//  (char endWith >> return "") <|>
//  (do try (string "\\'")
//      cs <- parseStringLit' endWith
//      return $ "'" ++ cs) <|>
//  (do char '\\'
//      c <- parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> 
//           char '\r' <|> char '\n'
//      cs <- parseStringLit' endWith
//      if c == '\r' || c == '\n' 
//        then return (c:(dropWhile isWhitespace cs)) 
//        else return (c:cs)) <|>
//   (liftM2 (:) anyChar (parseStringLit' endWith))

function lazyParseStringLit_(endWith){
    return function(state, scope, k){
        return parseStringLit_(endWith)(state, scope, k);
    }
}


function parseStringLit_(endWith){
  return ex(
    [char_(endWith) ,">>", return_, ""] ,"<|>",
    [cs (try_, [string, "\\'"])
        ("cs" ,"<-", lazyParseStringLit_(endWith))
        (ret, function(scope){ return "'" + scope.cs })
    ] ,"<|>",
    [cs (char_('\\'))
        ("c"  ,"<-", parseEscapeChar ,"<|>", parseAsciiHexChar ,"<|>", parseUnicodeHexChar ,"<|>",
                     char_('\r') ,"<|>", char_('\n'))
        ("cs" ,"<-", lazyParseStringLit_(endWith))
        (ret, function(scope){
                return (scope.c == '\r' || scope.c == '\n' ) ?
                      cons(scope.c, dropWhile(isWhitespace, scope.cs)) :
                      cons(scope.c, scope.cs);
        })
    ],"<|>",
    [liftM2, cons, anyChar, lazyParseStringLit_(endWith)]
  );
}


//parseStringLit:: ExpressionParser st
//parseStringLit = do
//  pos <- getPosition
//  -- parseStringLit' takes as an argument the quote-character that opened the
//  -- string.
//  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
//  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
//  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
//  -- above, expressions like:
//  --   var s = "string"   ;
//  -- do not parse.
//  return $ StringLit pos str
var parseStringLit = cs
  ("pos" ,"<-", getPosition)
  // parseStringLit' takes as an argument the quote-character that opened the)
  // string.)
  ("str" ,"<-", lex.lexeme, "$", [char_('\'') ,">>=", parseStringLit_]
                            ,"<|>", [char_('"') ,">>=", parseStringLit_])
  // CRUCIAL: Parsec.Token parsers expect to find their token on the first
  // character, and read whitespaces beyond their tokens.  Without 'lexeme'
  // above, expressions like:
  //   var s = "string"   ;
  // do not parse.
  (returnCall, Expression.StringLit, "pos", "str")



//parseRegexpLit:: ExpressionParser st
//parseRegexpLit = do
//  let parseFlags = do
//        flags <- many (oneOf "mgi")
//        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
//  let parseEscape = char '\\' >> anyChar
//  let parseChar = noneOf "/"
//  let parseRe = (char '/' >> return "") <|> 
//                (do char '\\'
//                    ch <- anyChar -- TOOD: too lenient
//                    rest <- parseRe
//                    return ('\\':ch:rest)) <|> 
//                (liftM2 (:) anyChar parseRe)
//  pos <- getPosition
//  char '/'
//  pat <- parseRe --many1 parseChar
//  flags <- parseFlags
//  spaces -- crucial for Parsec.Token parsers
//  return $ flags (RegexpLit pos pat)
var parseFlags = cs
  ("flags" ,"<-", many, oneOf("mgi"))
  (ret, function(scope){
    return function(f){
        return f( elem('g', scope.flags), elem('i', scope.flags) );
    }
  })
var parseEscape = ex(char_('\\') ,">>", anyChar).resolve();
var parseChar = noneOf("/");
var _parseRe = function(state, scope, k){ return parseRe(state, scope, k) }
var parseRe = ex([char_('/') ,">>", return_, ""] ,"<|>", 
  cs (char_('\\'))
     ("ch"   ,"<-", anyChar) // TOOD: too lenient
     ("rest" ,"<-", _parseRe)
     (ret, function(scope){ return '\\' + scope.ch + scope.rest }) ,"<|>",
  [liftM2, cons, anyChar, _parseRe]
);

var parseRegexpLit = cs
  ("pos"   ,"<-", getPosition)
  (char_('/'))
  ("pat"   ,"<-", parseRe) //many1 parseChar
  ("flags" ,"<-", parseFlags)
  (spaces) // crucial for Parsec.Token parsers
  (ret, function(scope){
        return scope.flags(function(global, ci){
            return Expression.RegexpLit(scope.pos, scope.pat, global, ci);
        });
  })

//parseObjectLit:: ExpressionParser st
//parseObjectLit =
//  let parseProp = do
//        -- Parses a string, identifier or integer as the property name.  I
//        -- apologize for the abstruse style, but it really does make the code
//        -- much shorter.
//        name <- (liftM (uncurry PropString) 
//                       (liftM (\(StringLit p s) -> (p,s)) parseStringLit))
//                <|> (liftM2 PropId getPosition identifier)
//                <|> (liftM2 PropNum getPosition decimal)
//        colon
//        val <- assignExpr
//        return (name,val)
//    in do pos <- getPosition
//          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
//          return $ ObjectLit pos props
// Parses a string, identifier or integer as the property name. I apologize
// for the abstruse style, but it really does make the code much shorter. 
var _parseProp = cs
  ("name" ,"<-", [liftM, uncurry(Prop.PropString), 
                       [liftM, function(stringLit){
                            return [stringLit[0], stringLit[1]];
                        }, parseStringLit]]
                ,"<|>", [liftM2, Prop.PropId, getPosition, identifier]
                ,"<|>", [liftM2, Prop.PropNum, getPosition, lex.decimal]
  )
  (lex.colon)
  ("val" ,"<-", assignExpr)
  (ret, function(scope){ return [scope.name, scope.val] })
  
var parseObjectLit =
  cs("pos"   ,"<-", getPosition)
    ("props" ,"<-", lex.braces, [_parseProp ,op(sepEndBy), lex.comma] ,"<?>", "object literal")
    (returnCall, Expression.ObjectLit, "pos", "props")

//--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.

//hexLit = do
//  try (string "0x")
//  digits <- many1 (oneOf "0123456789abcdefABCDEF")
//  [(hex,"")] <- return $ Numeric.readHex digits
//  return (True, hex)
var hexLit = cs
  (try_, string("0x"))
  ("digits" ,"<-", many1, oneOf("0123456789abcdefABCDEF"))
  ("hex"    ,"<-", returnCall, readHex, "digits")
  (ret, function(scope){ return [true, scope.hex] })


var octLit = cs
  ("digits" ,"<-", try_ ,"$",
            [char_('0') ,">>", [many1, oneOf("01234567") ,"<|>", return_("0")] ]
            ,"<*", notFollowedBy, oneOf("89")
  )
  ("oct"    ,"<-", returnCall, readOct, "digits")
  (ret, function(scope){ return [true, scope.oct] })


//-- Creates a decimal value from a whole, fractional and exponent part.
//mkDecimal:: Double -> Double -> Int -> Double
//mkDecimal w f e =
//  if (f >= 1.0)
//    then mkDecimal w (f / 10.0) e
//    else (w + f) * (10.0 ^^ e)
function mkDecimal(w, f, e){
  //return parseFloat(w + "." + f) * Math.pow(10.0, e);
  return (f >= 1.0) ?
    mkDecimal(w, f / 10.0, e) :
    (w + f) * Math.pow(10.0, e);
}


//exponentPart = do
//  oneOf "eE"
//  (char '+' >> decimal) <|> (char '-' >> negate `fmap` decimal) <|> decimal
var exponentPart = cs
  (oneOf, "eE")
  ([char_('+') ,">>", lex.decimal]
    ,"<|>", [char_('-') ,">>",  negate ,op(fmap), lex.decimal]
    ,"<|>", lex.decimal)


//--wrap a parser's result in a Just:
//jparser p = p >>= (return . Just) 
function jparser(p){
    return ex(p ,">>=", [return_ ,".", Maybe.Just]);
}


//decLit = 
//  (do whole <- decimal
//      mfrac <- option Nothing (jparser (char '.' >> decimal))
//      mexp <-  option Nothing (jparser exponentPart)
//      if (mfrac == Nothing && mexp == Nothing)
//        then return (True, fromIntegral whole)
//        else return (False, mkDecimal (fromIntegral whole) 
//                                      (fromIntegral (maybe 0 id mfrac))
//                                      (fromIntegral (maybe 0 id mexp)))) <|>
//  (do frac <- char '.' >> decimal
//      exp <- option 0 exponentPart
//      return (False, mkDecimal 0.0 (fromIntegral frac) (fromIntegral exp)))
var decLit = ex(cs
  ("whole" ,"<-", lex.decimal)
  ("mfrac" ,"<-", option, Maybe.Nothing, [jparser, [char_('.') ,">>", lex.decimal]])
  ("mexp"  ,"<-", option, Maybe.Nothing, [jparser, exponentPart])
  (ret, function(scope){
   return (scope.mfrac == Maybe.Nothing && scope.mexp == Maybe.Nothing) ?
            [true, scope.whole] :
            [false, mkDecimal(scope.whole,
                              maybe(0, id, scope.mfrac),
                              maybe(0, id, scope.mexp)
                              )];
  })
  ,"<|>", cs
  ("frac" ,"<-", char_('.') ,">>", lex.decimal)
  ("exp"  ,"<-", option, 0, exponentPart)
  (ret, function(scope){
    return [false, mkDecimal(0.0, scope.frac, scope.exp)];
  })
);

//parseNumLit:: ExpressionParser st
//parseNumLit = do
//    pos <- getPosition
//    (isint, num) <- lexeme $ hexLit <|> decLit
//    notFollowedBy identifierStart <?> "whitespace"
//    if isint
//      then return $ IntLit pos (round num) 
//      else return $ NumLit pos num
var parseNumLit = cs
  ("pos" ,"<-", getPosition)
  ("isint_num" ,"<-", lex.lexeme ,"$", hexLit ,"<|>", octLit ,"<|>", decLit)
  (notFollowedBy, identifierStart ,"<?>", "whitespace")
  (ret, function(scope){
    var isint = scope.isint_num[0];
    var num = scope.isint_num[1];
    return isint ?
            Expression.IntLit(scope.pos, round(num)) :
            Expression.NumLit(scope.pos, num)
  })
    

//------------------------------------------------------------------------------
//-- Position Helper
//------------------------------------------------------------------------------

//withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }
function withPos(cstr, p){
    return do_(
               bind("pos", getPosition),
               bind("e", p),
               ret(function(scope){ return cstr(scope.pos, scope.e) })
              );
}

//-------------------------------------------------------------------------------
//-- Compound Expression Parsers
//-------------------------------------------------------------------------------

//dotRef e = (reservedOp "." >> withPos cstr identifier) <?> "property.ref"
//    where cstr pos key = DotRef pos e key
function dotRef(e){
    function cstr(pos, key){ return Expression.DotRef(pos, e, key) }
    return ex([lex.reservedOp(".") ,">>", withPos, cstr, identifier]
            ,"<?>", "property.ref");
}


//funcApp e = (parens $ withPos cstr (parseExpression `sepBy` comma)) <?> "(function application)"
//    where cstr pos args = CallExpr pos e args
function funcApp(e){
    function cstr(pos, args){ return Expression.CallExpr(pos, e, args) }   
    return ex([lex.parens ,"$", withPos, cstr, [parseExpression ,op(sepBy), lex.comma]]
            ,"<?>", "(function application)");
}


//bracketRef e = (brackets $ withPos cstr parseExpression) <?> "[property-ref]"
//    where cstr pos key = BracketRef pos e key
function bracketRef(e){
    function cstr(pos, key){ return Expression.BracketRef(pos, e, key) }
    return ex([lex.brackets ,"$", withPos, cstr, parseExpression]
            ,"<?>", "[property-ref]");
}

//-------------------------------------------------------------------------------
//-- Expression Parsers
//-------------------------------------------------------------------------------

//parseParenExpr:: ExpressionParser st
//parseParenExpr = withPos ParenExpr (parens parseListExpr)
parseParenExpr = withPos(Expression.ParenExpr, lex.parens(parseListExpr));

//-- everything above expect functions
//parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
//  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
//  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef
var parseExprForNew = ex(
            parseThisRef
    ,"<|>", parseNullLit
    ,"<|>", parseBoolLit
    ,"<|>", parseStringLit 
    ,"<|>", parseArrayLit
    ,"<|>", parseParenExpr
    ,"<|>", parseNewExpr
    ,"<|>", parseNumLit
    ,"<|>", parseRegexpLit
    ,"<|>", parseObjectLit
    ,"<|>", parseVarRef
);
  

//-- all the expression parsers defined above
//parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
//  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
//  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
//  <|> parseVarRef
var parseSimpleExpr_ = ex(
            parseThisRef
    ,"<|>", parseNullLit
    ,"<|>", parseBoolLit 
    ,"<|>", parseStringLit
    ,"<|>", parseArrayLit
    ,"<|>", parseParenExpr
    ,"<|>", parseFuncExpr
    ,"<|>", parseNumLit
    ,"<|>", parseRegexpLit
    ,"<|>", parseObjectLit
    ,"<|>", parseVarRef
);

//parseSimpleExprForNew (Just e) = (do
//    e' <- dotRef e <|> bracketRef e
//    parseSimpleExprForNew $ Just e') <|> (return e)
//parseSimpleExprForNew Nothing = do
//  e <- parseNewExpr <?> "expression (3)"
//  parseSimpleExprForNew (Just e)
function parseSimpleExprForNew(maybeVal){
    if(maybeVal.Just){
        var e = maybeVal[0];
        return ex(cs
            ("e_" ,"<-", dotRef(e) ,"<|>", bracketRef(e))
            (function(state, scope, k){
                return parseSimpleExprForNew(Maybe.Just(scope.e_))(state, scope, k);
            }) ,"<|>", return_(e))
    }
    if(maybeVal.Nothing){
        return cs
            ("e"  ,"<-", parseNewExpr ,"<?>", "expression (3)")
            (function(state, scope, k){
                return parseSimpleExprForNew(Maybe.Just(scope.e))(state, scope, k);
            })
    }
}

//parseNewExpr =
//  (do pos <- getPosition
//      reserved "new"
//      constructor <- parseSimpleExprForNew Nothing -- right-associativity
//      arguments <- (try (parens (parseExpression `sepBy` comma))) <|> (return [])
//      return (NewExpr pos constructor arguments)) <|>
//  parseSimpleExpr'
parseNewExpr = ex(
  cs("pos" ,"<-", getPosition)
     (lex.reserved, "new")
     ("constructor_" ,"<-", parseSimpleExprForNew, Maybe.Nothing) // right-associativity
     ("arguments" ,"<-", [try_, [lex.parens, [parseExpression ,op(sepBy), lex.comma]]] ,"<|>", return_([]))
     (returnCall, Expression.NewExpr, "pos", "constructor_", "arguments")
  ,"<|>", parseSimpleExpr_
  );
   

//parseSimpleExpr (Just e) = (do
//    e' <- dotRef e <|> funcApp e <|> bracketRef e
//    parseSimpleExpr $ Just e') <|> (return e)
//parseSimpleExpr Nothing = do
//  e <- parseNewExpr <?> "expression (3)"
//  parseSimpleExpr (Just e)
function parseSimpleExpr(maybeVal){
    if(maybeVal.Just){
        var e = maybeVal[0];
        return ex(cs
            ("e_" ,"<-", dotRef(e) ,"<|>", funcApp(e) ,"<|>", bracketRef(e))
            (function(state, scope, k){
                return parseSimpleExpr(Maybe.Just(scope.e_))(state, scope, k);
            }) ,"<|>", return_(e))
    }
    if(maybeVal.Nothing){
        return cs
            ("e" ,"<-", parseNewExpr ,"<?>", "expression (3)")
            (function(state, scope, k){
                return parseSimpleExpr(Maybe.Just(scope.e))(state, scope, k);
            })
    }
}


//makeInfixExpr str constr = Infix parser AssocLeft where
//  parser:: CharParser st (Expression SourcePos -> Expression SourcePos -> Expression SourcePos)
//  parser = do
//    pos <- getPosition
//    reservedOp str
//    return (InfixExpr pos constr)  -- points-free, returns a function
function makeInfixExpr(str, constr){
    var parser = cs
        ("pos" ,"<-", getPosition)
        (lex.reservedOp(str))
        (ret, function(scope){
            return function(exp1, exp2){
                return Expression.InfixExpr(scope.pos, constr, exp1, exp2);
            }
        })
    
    return Operator.Infix(parser, Assoc.AssocLeft);
}

//-- apparently, expression tables can't handle immediately-nested prefixes
//parsePrefixedExpr = do
//  pos <- getPosition
//  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
//                      (reservedOp "~" >> return PrefixBNot) <|>
//                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
//                       return PrefixMinus) <|>
//                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
//                       return PrefixPlus) <|>
//                      (reserved "typeof" >> return PrefixTypeof) <|>
//                      (reserved "void" >> return PrefixVoid) <|>
//                      (reserved "delete" >> return PrefixDelete)
//  case op of
//    Nothing -> unaryAssignExpr
//    Just op -> do
//      innerExpr <- parsePrefixedExpr
//      return (PrefixExpr pos op innerExpr)
var parsePrefixedExpr = cs
  ("pos" ,"<-", getPosition)
  ("op"  ,"<-", optionMaybe ,"$",
                        [lex.reservedOp("!")    ,">>", return_, PrefixOp.PrefixLNot]
                ,"<|>", [lex.reservedOp("~")    ,">>", return_, PrefixOp.PrefixBNot]
                ,"<|>", [try_, [lex.lexeme ,"$", char_('-') ,">>", notFollowedBy, char_('-')]
                                                ,">>", return_, PrefixOp.PrefixMinus]
                ,"<|>", [try_, [lex.lexeme ,"$", char_('+') ,">>", notFollowedBy, char_('+')]
                                                ,">>", return_, PrefixOp.PrefixPlus]
                ,"<|>", [lex.reserved, "typeof" ,">>", return_, PrefixOp.PrefixTypeof]
                ,"<|>", [lex.reserved, "void"   ,">>", return_, PrefixOp.PrefixVoid]
                ,"<|>", [lex.reserved, "delete" ,">>", return_, PrefixOp.PrefixDelete]
  )
  (function(state, scope, k){
    var op = scope.op, res;
    if(op.Nothing)
        res = unaryAssignExpr;
    if(op.Just)
        res = cs
            ("innerExpr" ,"<-", parsePrefixedExpr)
            (ret, function(_scope){ return Expression.PrefixExpr(scope.pos, op[0], _scope.innerExpr) })
    return res(state, scope, k);
  })

//exprTable:: [[Operator Char st ParsedExpression]]
//exprTable = 
//  [
//   [makeInfixExpr "*" OpMul, makeInfixExpr "/" OpDiv, makeInfixExpr "%" OpMod],
//   [makeInfixExpr "+" OpAdd, makeInfixExpr "-" OpSub],
//   [makeInfixExpr "<<" OpLShift, makeInfixExpr ">>" OpSpRShift,
//    makeInfixExpr ">>>" OpZfRShift],
//   [makeInfixExpr "<" OpLT, makeInfixExpr "<=" OpLEq, makeInfixExpr ">" OpGT,
//    makeInfixExpr ">=" OpGEq, 
//    makeInfixExpr "instanceof" OpInstanceof, makeInfixExpr "in" OpIn],
//   [makeInfixExpr "&" OpBAnd], 
//   [makeInfixExpr "^" OpBXor], 
//   [makeInfixExpr "|" OpBOr],
//   [makeInfixExpr "&&" OpLAnd],
//   [makeInfixExpr "||" OpLOr],  
//   [makeInfixExpr "==" OpEq, makeInfixExpr "!=" OpNEq,
//    makeInfixExpr "===" OpStrictEq, makeInfixExpr "!==" OpStrictNEq]
//    ]
var exprTable = 
  [
   [makeInfixExpr("*",  InfixOp.OpMul),
    makeInfixExpr("/",  InfixOp.OpDiv),
    makeInfixExpr("%",  InfixOp.OpMod)],
   [makeInfixExpr("+",  InfixOp.OpAdd),
    makeInfixExpr("-",  InfixOp.OpSub)],
   [makeInfixExpr("<<", InfixOp.OpLShift),
    makeInfixExpr(">>", InfixOp.OpSpRShift),
    makeInfixExpr(">>>",InfixOp.OpZfRShift)],
   [makeInfixExpr("<",  InfixOp.OpLT),
    makeInfixExpr("<=", InfixOp.OpLEq),
    makeInfixExpr(">",  InfixOp.OpGT),
    makeInfixExpr(">=", InfixOp.OpGEq),
    makeInfixExpr("instanceof", InfixOp.OpInstanceof),
    makeInfixExpr("in", InfixOp.OpIn)],
   [makeInfixExpr("&",  InfixOp.OpBAnd)],
   [makeInfixExpr("^",  InfixOp.OpBXor)],
   [makeInfixExpr("|",  InfixOp.OpBOr)],
   [makeInfixExpr("&&", InfixOp.OpLAnd)],
   [makeInfixExpr("||", InfixOp.OpLOr)],
   [makeInfixExpr("==", InfixOp.OpEq),
    makeInfixExpr("!=", InfixOp.OpNEq),
    makeInfixExpr("===",InfixOp.OpStrictEq),
    makeInfixExpr("!==",InfixOp.OpStrictNEq)]
  ].reverse();


//parseExpression' = 
//  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"
var parseExpression_ = 
  label(buildExpressionParser(exprTable, parsePrefixedExpr), "simple expression");


//asLValue :: SourcePos
//         -> Expression SourcePos 
//         -> CharParser st (LValue SourcePos)
//asLValue p' e = case e of
//  VarRef p (Id _ x) -> return (LVar p x)
//  DotRef p e (Id _ x) -> return (LDot p e x)
//  BracketRef p e1 e2 -> return (LBracket p e1 e2)
//  otherwise -> fail $ "expeceted l-value at " ++ show p'
function asLValue(p_, e){
    if(e.VarRef)
        if(e[1].Id)
            return return_(LValue.LVar(e[0], e[1][1]));
    if(e.DotRef)
        if(e[2].Id)
            return return_(LValue.LDot(e[0], e[1], e[2][1]));
    if(e.BracketRef)
        return return_(LValue.LBracket(e[0], e[1], e[2]));
    return fail("expeceted l-value at " + p_)
}

//lvalue :: CharParser st (LValue SourcePos)
//lvalue = do
//  p <- getPosition
//  e <- parseSimpleExpr Nothing
//  asLValue p e
var lvalue = cs
  ("p" ,"<-", getPosition)
  ("e" ,"<-", parseSimpleExpr, Maybe.Nothing)
  (function(state, scope, k){ return asLValue(scope.p, scope.e)(state, scope, k) })
  
  
//unaryAssignExpr :: CharParser st ParsedExpression
//unaryAssignExpr = do
//  p <- getPosition
//  let prefixInc = do
//        reservedOp "++"
//        liftM (UnaryAssignExpr p PrefixInc) lvalue
//  let prefixDec = do
//        reservedOp "--"
//        liftM (UnaryAssignExpr p PrefixDec) lvalue
//  let postfixInc e = do
//        reservedOp "++"
//        liftM (UnaryAssignExpr p PostfixInc) (asLValue p e)
//  let postfixDec e = do
//        reservedOp "--"
//        liftM (UnaryAssignExpr p PostfixDec) (asLValue p e)
//  let other = do
//        e <- parseSimpleExpr Nothing
//        postfixInc e <|> postfixDec e <|> return e
//  prefixInc <|> prefixDec <|> other

function _createExpr(pos, ctr){
    return function(lval){
        return Expression.UnaryAssignExpr(pos, UnaryAssignOp[ctr], lval)
    }
}

function onSameLineExp(postfix, notpostfix){
    return function(state, scope, k){
        var op = state.substring(0, 2);
            
        if(op == "--" || op == "++"){
            var m = state.input.substring(0, state.index - state.dropped).match(/\s*$/);
            var autosemi = m && /\n|\r/.test(m[0]);
            return (autosemi ? notpostfix : postfix)(state, scope, k);
        }else
            return notpostfix(state, scope, k);
    }
}

var unaryAssignExpr = parserBind(getPosition, function(pos){
    
    var prefixInc = do_(lex.reservedOp("++"),
                        liftM(_createExpr(pos, "PrefixInc"), lvalue));
                    
    var prefixDec = do_(lex.reservedOp("--"),
                        liftM(_createExpr(pos, "PrefixDec"), lvalue));
                    
    function postfixInc(e){
        return do_(lex.reservedOp("++"),
                   liftM(_createExpr(pos, "PostfixInc"), asLValue(pos, e)));
    }
    
    function postfixDec(e){
        return do_(lex.reservedOp("--"),
                   liftM(_createExpr(pos, "PostfixDec"), asLValue(pos, e)));
    }
    
    //var other = parserBind(parseSimpleExpr(Maybe.Nothing), function(e){
    //    return parserPlus(postfixInc(e), parserPlus(postfixDec(e), return_(e)));
    //});
    var postFix = parserBind(parseSimpleExpr(Maybe.Nothing), function(e){
        return  onSameLineExp(
                    //if ++ or -- is on the same line
                    //then they are postfix operators
                    parserPlus(postfixInc(e), parserPlus(postfixDec(e), return_(e))),
                    //otherwise a semicolon is needed before the ops
                    //so they will be consumed by the next parser,
                    //and the current expression is returned
                    return_(e)
                )
    });
    
    return parserPlus(prefixInc, parserPlus(prefixDec, postFix));
  
})


//parseTernaryExpr':: CharParser st (ParsedExpression,ParsedExpression)
//parseTernaryExpr' = do
//    reservedOp "?"
//    l <- assignExpr
//    colon
//    r <- assignExpr
//    return $(l,r)
var parseTernaryExpr_ = cs
  (lex.reservedOp("?"))
  ("l" ,"<-", assignExpr)
  (lex.colon)
  ("r" ,"<-", assignExpr)
  (ret, function(scope){ return [scope.l, scope.r] })
  

//parseTernaryExpr:: ExpressionParser st
//parseTernaryExpr = do
//  e <- parseExpression'
//  e' <- optionMaybe parseTernaryExpr'
//  case e' of
//    Nothing -> return e
//    Just (l,r) -> do p <- getPosition
//                     return $ CondExpr p e l r
var parseTernaryExpr = cs
  ("e"  ,"<-", parseExpression_)
  ("e_" ,"<-", optionMaybe, parseTernaryExpr_)
  (function(state, scope, k){
    var e_ = scope.e_,
        e = scope.e,
        res;
    if(e_.Nothing)
        res = return_(scope.e);
    if(e_.Just){
        var l = e_[0][0],
            r = e_[0][1];
        res = cs("p" ,"<-", getPosition)
                 (ret, function(scope){ return Expression.CondExpr(scope.p, e, l, r) })
    }
    return res(state, scope, k);
  })


//assignOp :: CharParser st AssignOp
//assignOp = 
//  (reservedOp "=" >> return OpAssign) <|>
//  (reservedOp "+=" >> return OpAssignAdd) <|>
//  (reservedOp "-=" >> return OpAssignSub) <|>
//  (reservedOp "*=" >> return OpAssignMul) <|>
//  (reservedOp "/=" >> return OpAssignDiv) <|>
//  (reservedOp "%=" >> return OpAssignMod) <|>
//  (reservedOp "<<=" >> return OpAssignLShift) <|>
//  (reservedOp ">>=" >> return OpAssignSpRShift) <|>
//  (reservedOp ">>>=" >> return OpAssignZfRShift) <|>
//  (reservedOp "&=" >> return OpAssignBAnd) <|>
//  (reservedOp "^=" >> return OpAssignBXor) <|>
//  (reservedOp "|=" >> return OpAssignBOr)
var assignOp = ex(
            [lex.reservedOp("=")   ,">>", return_, AssignOp.OpAssign     ]
    ,"<|>", [lex.reservedOp("+=")  ,">>", return_, AssignOp.OpAssignAdd  ]
    ,"<|>", [lex.reservedOp("-=")  ,">>", return_, AssignOp.OpAssignSub  ]
    ,"<|>", [lex.reservedOp("*=")  ,">>", return_, AssignOp.OpAssignMul  ]
    ,"<|>", [lex.reservedOp("/=")  ,">>", return_, AssignOp.OpAssignDiv  ]
    ,"<|>", [lex.reservedOp("%=")  ,">>", return_, AssignOp.OpAssignMod  ]
    ,"<|>", [lex.reservedOp("<<=") ,">>", return_, AssignOp.OpAssignLShift   ]
    ,"<|>", [lex.reservedOp(">>=") ,">>", return_, AssignOp.OpAssignSpRShift ]
    ,"<|>", [lex.reservedOp(">>>="),">>", return_, AssignOp.OpAssignZfRShift ]
    ,"<|>", [lex.reservedOp("&=")  ,">>", return_, AssignOp.OpAssignBAnd ]
    ,"<|>", [lex.reservedOp("^=")  ,">>", return_, AssignOp.OpAssignBXor ]
    ,"<|>", [lex.reservedOp("|=")  ,">>", return_, AssignOp.OpAssignBOr  ]
);

//assignExpr :: ExpressionParser st
//assignExpr = do
//  p <- getPosition
//  lhs <- parseTernaryExpr
//  let assign = do
//        op <- assignOp
//        lhs <- asLValue p lhs
//        rhs <- assignExpr
//        return (AssignExpr p op lhs rhs)
//  assign <|> (return lhs)
assignExpr = cs
  ("p"   ,"<-", getPosition)
  ("lhs" ,"<-", parseTernaryExpr)
  (cs("op"  ,"<-", assignOp)
     ("lhs" ,"<-", lazy, function(scope){
        //bring p to the current scope, so that returnCall can be used:
        scope.p = scope.scope.p;
        return asLValue(scope.scope.p, scope.scope.lhs);
     })
     ("rhs" ,"<-", lazy, function(){ return assignExpr })
     (returnCall, Expression.AssignExpr, "p", "op", "lhs", "rhs")
  ,"<|>", ret, "lhs")


//parseExpression:: ExpressionParser st
//parseExpression = assignExpr
parseExpression = assignExpr;

//parseListExpr =
//  liftM2 ListExpr getPosition (assignExpr `sepBy1` comma)
parseListExpr =
    liftM2(Expression.ListExpr, getPosition, sepBy1(assignExpr, lex.comma));


//parseScript:: CharParser state (JavaScript SourcePos)
//parseScript = do
//  whiteSpace
//  liftM2 Script getPosition (parseStatement `sepBy` whiteSpace)
/*
var parseScript = cs
  (lex.whiteSpace)
  (liftM2, JavaScript.Script, getPosition, [parseStatement ,op(sepBy1), lex.whiteSpace])
*/
var parseScript = cs
  (lex.whiteSpace)
  (liftM2, JavaScript.Script, getPosition, many(parseStatement))
  

//parseJavaScriptFromFile :: MonadIO m => String -> m [Statement SourcePos]
//parseJavaScriptFromFile filename = do
//  chars <- liftIO $ readFile filename
//  case parse parseScript filename chars of
//    Left err               -> fail (show err)
//    Right (Script _ stmts) -> return stmts


//parseScriptFromString :: String -> String 
//                      -> Either ParseError (JavaScript SourcePos)
//parseScriptFromString src script = parse parseScript src script


//emptyParsedJavaScript = 
//  Script (error "Parser.emptyParsedJavaScript--no annotation") []
  
  
//parseString :: String -> [Statement SourcePos]
//parseString str = case parse parseScript "" str of
//  Left err -> error (show err)
//  Right (Script _ stmts) -> stmts


JSParsec.JavaScript = {
     parseScript            : parseScript
    ,parseExpression        : parseExpression
    //,parseString            : parseString
    //,parseScriptFromString  : parseScriptFromString
    //,emptyParsedJavaScript  : emptyParsedJavaScript
    //,ParsedStatement        : ParsedStatement
    //,ParsedExpression       : ParsedExpression
    //,parseJavaScriptFromFile: parseJavaScriptFromFile
    ,parseSimpleExpr_       : parseSimpleExpr_
    ,parseBlockStmt         : parseBlockStmt
    ,parseStatement         : parseStatement
    //,StatementParser        : StatementParser
    //,ExpressionParser       : ExpressionParser
    ,assignExpr             : assignExpr
};