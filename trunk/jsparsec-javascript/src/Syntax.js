//-- |JavaScript's syntax.
//module BrownPLT.JavaScript.Syntax(Expression(..),CaseClause(..),Statement(..),
//         InfixOp(..),CatchClause(..),VarDecl(..),JavaScript(..),
//         AssignOp(..),Id(..),PrefixOp(..),Prop(..),
//         ForInit(..),ForInInit(..),unId
//  , UnaryAssignOp (..)
//  , LValue (..)
//  ) where
//
//import Text.ParserCombinators.Parsec(SourcePos) -- used by data JavaScript
//import Data.Generics(Data,Typeable)
//
//data JavaScript a
//  -- |A script in <script> ... </script> tags.  This may seem a little silly,
//  -- but the Flapjax analogue has an inline variant and attribute-inline 
//  -- variant.
//  = Script a [Statement a] 
//  deriving (Show,Data,Typeable,Eq,Ord)

function Statement(){}

function JavaScript(){}
data(JavaScript, [["Script", "a", Array]]);

//data Id a = Id a String deriving (Show,Eq,Ord,Data,Typeable)

function Id(){}
data(Id, [["Id", "a", String]]);


//unId :: Id a -> String
//unId (Id _ s) = s

function unId(idVal){
    if(!(idVal instanceof Id))
        throw "Type error: expecting type of 'Id' instead of " + idVal.constructor;
    return idVal.Id ? idVal[1] : null;
}


//-- http://developer.mozilla.org/en/docs/
//--   Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
//data InfixOp = OpLT | OpLEq | OpGT | OpGEq  | OpIn  | OpInstanceof | OpEq | OpNEq
//             | OpStrictEq | OpStrictNEq | OpLAnd | OpLOr 
//             | OpMul | OpDiv | OpMod  | OpSub | OpLShift | OpSpRShift
//             | OpZfRShift | OpBAnd | OpBXor | OpBOr | OpAdd
//    deriving (Show,Data,Typeable,Eq,Ord,Enum)

function InfixOp(){}
data(InfixOp, [
    "OpLT", "OpLEq", "OpGT", "OpGEq ", "OpIn ", "OpInstanceof", "OpEq", "OpNEq",
    "OpStrictEq", "OpStrictNEq", "OpLAnd", "OpLOr",
    "OpMul", "OpDiv", "OpMod ", "OpSub", "OpLShift", "OpSpRShift",
    "OpZfRShift", "OpBAnd", "OpBXor", "OpBOr", "OpAdd"
]);


//data AssignOp = OpAssign | OpAssignAdd | OpAssignSub | OpAssignMul | OpAssignDiv
//  | OpAssignMod | OpAssignLShift | OpAssignSpRShift | OpAssignZfRShift
//  | OpAssignBAnd | OpAssignBXor | OpAssignBOr
//  deriving (Show,Data,Typeable,Eq,Ord)

function AssignOp(){}
data(AssignOp, [
    "OpAssign", "OpAssignAdd", "OpAssignSub", "OpAssignMul", "OpAssignDiv",
    "OpAssignMod", "OpAssignLShift", "OpAssignSpRShift", "OpAssignZfRShift",
    "OpAssignBAnd", "OpAssignBXor", "OpAssignBOr"
]);


//data UnaryAssignOp
//  = PrefixInc | PrefixDec | PostfixInc | PostfixDec
//  deriving (Show, Data, Typeable, Eq, Ord)

function UnaryAssignOp(){}
data(UnaryAssignOp, [
    "PrefixInc", "PrefixDec", "PostfixInc", "PostfixDec"
]);

//data PrefixOp = PrefixLNot | PrefixBNot | PrefixPlus
//  | PrefixMinus | PrefixTypeof | PrefixVoid | PrefixDelete
//  deriving (Show,Data,Typeable,Eq,Ord)

function PrefixOp(){}
data(PrefixOp, [
    "PrefixLNot", "PrefixBNot", "PrefixPlus",
    "PrefixMinus", "PrefixTypeof", "PrefixVoid", "PrefixDelete"
]);


//data Prop a 
//  = PropId a (Id a) | PropString a String | PropNum a Integer
//  deriving (Show,Data,Typeable,Eq,Ord)

function Prop(){}
data(Prop, [
    ["PropId", "a", Id] , ["PropString", "a", String] , ["PropNum", "a", Number]
]);


//data LValue a
//  = LVar a String
//  | LDot a (Expression a) String
//  | LBracket a (Expression a) (Expression a)
//  deriving (Show, Eq, Ord, Data, Typeable)

function LValue(){}
function Expression(){}

data(LValue, [
    ["LVar", "a", String],
    ["LDot", "a", Expression, String],
    ["LBracket", "a", Expression, Expression]
]);


//data Expression a
//  = StringLit a String
//  | RegexpLit a String Bool {- global? -} Bool {- case-insensitive? -}
//  | NumLit a Double
//  | IntLit a Int
//  | BoolLit a Bool
//  | NullLit a
//  | ArrayLit a [Expression a]
//  | ObjectLit a [(Prop a, Expression a)]
//  | ThisRef a
//  | VarRef a (Id a)
//  | DotRef a (Expression a) (Id a)
//  | BracketRef a (Expression a) {- container -} (Expression a) {- key -}
//  | NewExpr a (Expression a) {- constructor -} [Expression a]
//  | PrefixExpr a PrefixOp (Expression a)
//  | UnaryAssignExpr a UnaryAssignOp (LValue a)
//  | InfixExpr a InfixOp (Expression a) (Expression a)
//  | CondExpr a (Expression a) (Expression a) (Expression a)
//  | AssignExpr a AssignOp (LValue a) (Expression a)
//  | ParenExpr a (Expression a)
//  | ListExpr a [Expression a]
//  | CallExpr a (Expression a) [Expression a]
//  --funcexprs are optionally named
//  | FuncExpr a (Maybe (Id a)) [(Id a)] (Statement a)
//  deriving (Show,Data,Typeable,Eq,Ord)

data(Expression, [
    ["StringLit", "a", String]
   ,["RegexpLit", "a", String, Boolean /* global? */, Boolean /* case-insensitive? */]
   ,["NumLit", "a", Number]
   ,["IntLit", "a", Number]
   ,["BoolLit", "a", Boolean]
   ,["NullLit", "a"]
   ,["ArrayLit", "a", Array]
   ,["ObjectLit", "a", Array]
   ,["ThisRef", "a"]
   ,["VarRef", "a", Id]
   ,["DotRef", "a", Expression, Id]
   ,["BracketRef", "a", Expression /* container */, Expression /* key */]
   ,["NewExpr", "a", Expression /* constructor */, Array]
   ,["PrefixExpr", "a", PrefixOp, Expression]
   ,["UnaryAssignExpr", "a", UnaryAssignOp, LValue]
   ,["InfixExpr", "a", InfixOp, Expression, Expression]
   ,["CondExpr", "a", Expression, Expression, Expression]
   ,["AssignExpr", "a", AssignOp, LValue, Expression]
   ,["ParenExpr", "a", Expression]
   ,["ListExpr", "a", Array]
   ,["CallExpr", "a", Expression, Expression]
   ,["FuncExpr", "a", Maybe, Array, Statement] //funcexprs are optionally named
]);


//data CaseClause a 
//  = CaseClause a (Expression a) [Statement a]
//  | CaseDefault a [Statement a]
//  deriving (Show,Data,Typeable,Eq,Ord

function CaseClause(){}
data(CaseClause, [
    ["CaseClause", "a", Expression, Array],
    ["CaseDefault", "a", Array]                  
]);


//data CatchClause a 
//  = CatchClause a (Id a) (Statement a) 
//  deriving (Show,Data,Typeable,Eq,Ord)

function CatchClause(){}
data(CatchClause, [
    ["CatchClause", "a", Id, Statement]
]);


//data VarDecl a 
//  = VarDecl a (Id a) (Maybe (Expression a)) 
//  deriving (Show,Data,Typeable,Eq,Ord)

function VarDecl(){}
data(VarDecl, [
    ["VarDecl", "a", Id, Maybe] 
]);


//data ForInit a
//  = NoInit
//  | VarInit [VarDecl a]
//  | ExprInit (Expression a)
//  deriving (Show,Data,Typeable,Eq,Ord)

function ForInit(){}
data(ForInit, [
    "NoInit",
    ["VarInit", Array],
    ["ExprInit", Expression]
]);


//data ForInInit a
// = ForInVar (Id a)
// | ForInNoVar (Id a)
// deriving (Show,Data,Typeable,Eq,Ord)

function ForInInit(){}
data(ForInInit, [
    ["ForInVar", Id],
    ["ForInNoVar", Id]
]);


//data Statement a
//  = BlockStmt a [Statement a]
//  | EmptyStmt a
//  | ExprStmt a (Expression a)
//  | IfStmt a (Expression a) (Statement a) (Statement a)
//  | IfSingleStmt a (Expression a) (Statement a)
//  | SwitchStmt a (Expression a) [CaseClause a]
//  | WhileStmt a (Expression a) (Statement a)
//  | DoWhileStmt a (Statement a) (Expression a)
//  | BreakStmt a (Maybe (Id a))
//  | ContinueStmt a (Maybe (Id a))
//  | LabelledStmt a (Id a) (Statement a)
//  | ForInStmt a (ForInInit a) (Expression a) (Statement a)
//  | ForStmt a (ForInit a)        
//              (Maybe (Expression a)) -- test
//              (Maybe (Expression a)) -- increment
//              (Statement a)          -- body
//  | TryStmt a (Statement a) {-body-} [CatchClause a] {-catches-}
//      (Maybe (Statement a)) {-finally-}
//  | ThrowStmt a (Expression a)
//  | ReturnStmt a (Maybe (Expression a))
//  | WithStmt a (Expression a) (Statement a)
//  | VarDeclStmt a [VarDecl a]
//  | FunctionStmt a (Id a) {-name-} [(Id a)] {-args-} (Statement a) {-body-}
//  deriving (Show,Data,Typeable,Eq,Ord)

data(Statement, [
    ,["BlockStmt"   , "a", Array]
    ,["EmptyStmt"   , "a"]
    ,["ExprStmt"    , "a", Expression]
    ,["IfStmt"      , "a", Expression, Statement, Statement]
    ,["IfSingleStmt", "a", Expression, Statement]
    ,["SwitchStmt"  , "a", Expression, Array]
    ,["WhileStmt"   , "a", Expression, Statement]
    ,["DoWhileStmt" , "a", Statement, Expression]
    ,["BreakStmt"   , "a", Maybe]
    ,["ContinueStmt", "a", Maybe]
    ,["LabelledStmt", "a", Id, Statement]
    ,["ForInStmt"   , "a", ForInInit, Expression, Statement]
    ,["ForStmt"     , "a", ForInit, 
                           Maybe,     //test
                           Maybe,     //increment
                           Statement] //body
    ,["TryStmt"     , "a", Statement, //body
                           Array,     //catches
                           Maybe]     //finally
    ,["ThrowStmt"   , "a", Expression]
    ,["ReturnStmt"  , "a", Maybe]
    ,["WithStmt"    , "a", Expression, Statement]
    ,["VarDeclStmt" , "a", Array]
    ,["FunctionStmt", "a", Id /*name*/, Array /*args*/, Statement /*body*/]
]);

