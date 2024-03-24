module LibAstatine.AST (
    Module(..),
    Decl(..),
    Expr(..),
    IdentKind(..),
    parseModule,
    checkModule,
    isConst
) where

import LibAstatine.Error (Result (..), CompilerError (..))
import LibAstatine.SExpr (SExpr (..), PairKind(..))

data Module = Module {
        modName :: String,
        modDecls :: [Decl],
        modDepends :: [String]
    }
    deriving Show

data Decl = Function {
        funcName :: String,
        funcParams :: [String],
        funcBody :: Expr
    }
    | Global {
        globalName :: String,
        globalValue :: Expr
    }
    deriving Show

data IdentKind = LocalIdent | GlobalIdent | UnresolvedIdent
    deriving (Show, Eq)

data Expr = ConstInt Integer
    | ConstFloat Double
    | ConstString String
    | ConstChar Char
    | ConstTrue
    | ConstFalse
    | ConstNil
    | IdentExpr IdentKind String
    | ListExpr [Expr]
    | CallExpr Expr [Expr]
    | DoExpr [Expr]
    deriving Show

isConst :: Expr -> Bool
isConst (ConstInt _) = True
isConst (ConstFloat _) = True
isConst (ConstString _) = True
isConst (ConstChar _) = True
isConst ConstTrue = True
isConst ConstFalse = True
isConst ConstNil = True
isConst (IdentExpr _ _) = True
isConst _ = False

unexpectedEof :: String -> Result a
unexpectedEof = Err . UnexpectedEof

unexpectedSExpr :: SExpr -> String -> Result a
unexpectedSExpr expr string = Err $ UnexpectedSExpr $ show expr ++ ": " ++ string

parseModule :: [SExpr] -> Result Module
parseModule [] = unexpectedEof "expect module declaration"
parseModule (x:xs) = do 
    mod <- parseModuleDecl x
    decls <- mapM parseDecl xs
    Ok $ mod { modDecls = decls }

parseModuleDecl :: SExpr -> Result Module
parseModuleDecl (Pair Round (Identifier "module") (Pair Round (Identifier name) imports)) = case imports of
    Pair Round next (PairNil Round) -> Module name [] <$> parseImport next
    PairNil Round -> Ok $ Module name [] []
    _ -> unexpectedSExpr imports "expect module imports of form (<import> ...)"
parseModuleDecl sexp = unexpectedSExpr sexp "expect module declaration of form (module <name> ...)"

parseImport :: SExpr -> Result [String]
parseImport (PairNil Round) = Ok []
parseImport (Pair Round (Identifier dep) next) = (dep:) <$> parseImport next
parseImport sexp = unexpectedSExpr sexp "expect module import of form (<import> ...)"

parseDecl :: SExpr -> Result Decl
parseDecl (Pair Round (Identifier "defun") (Pair Round (Identifier name) (Pair Round params (Pair Round body (PairNil _))))) = Function name <$> parseParams params <*> parseExpr body
parseDecl (Pair Round (Identifier "let") (Pair Round (Identifier name) (Pair Round value (PairNil _)))) = Global name <$> parseExpr value
parseDecl sexp = unexpectedSExpr sexp "expect top-level declaration like (defun <name> (<params>...) <body>)"

parseParams :: SExpr -> Result [String]
parseParams (PairNil Round) = Ok []
parseParams (Pair Round (Identifier param) next) = (param:) <$> parseParams next
parseParams sexp = unexpectedSExpr sexp "expect parameters of for (<param> ...)"

parseExpr :: SExpr -> Result Expr
parseExpr (IntegerLiteral i) = Ok $ ConstInt i
parseExpr (FloatLiteral f) = Ok $ ConstFloat f
parseExpr (StringLiteral s) = Ok $ ConstString s
parseExpr (CharLiteral c) = Ok $ ConstChar c
parseExpr STrue = Ok ConstTrue
parseExpr SFalse = Ok ConstFalse
parseExpr Nil = Ok ConstNil
parseExpr (Identifier s) = Ok $ IdentExpr UnresolvedIdent s
parseExpr (PairNil Square) = Ok $ ListExpr []
parseExpr sexp@(Pair Square _ _) = ListExpr <$> parseList sexp
    where parseList (PairNil Square) = Ok []
          parseList (Pair Square fst snd) = parseExpr fst >>= \fst' -> (fst':) <$> parseList snd
          parseList sexp = unexpectedSExpr sexp "expect list expression"
parseExpr sexp@(PairNil Round) = unexpectedSExpr sexp "empty call expression"
parseExpr sexp@(Pair Round _ _) = do
    call <- parseCall sexp
    case call of
        ((IdentExpr _ "do"):args) -> Ok $ DoExpr args 
        (expr:args) -> Ok $ CallExpr expr args
        [] -> undefined
    where parseCall (PairNil Round) = Ok []
          parseCall (Pair Round fst snd) = parseExpr fst >>= \fst' -> (fst':) <$> parseCall snd
          parseCall sexp = unexpectedSExpr sexp "expect call expression"
parseExpr sexp = unexpectedSExpr sexp "expect expression"

checkModule :: Module -> Result Module
checkModule = Ok -- TODO: check AST

