module LibAstatine.IR (
    Decl(..),
    normalize
) where

import qualified LibAstatine.AST as AST;
import Data.Char (isAlphaNum, ord)
import Numeric (showHex)

data Decl = Function {
        funcName :: String,
        funcParams :: [String],
        funcLocals :: [Decl],
        funcBody :: [Stmt]
    }
    | Global {
        globalName :: String,
        globalValue :: AST.Expr
    }
    deriving Show

data Stmt = Label String
    | Goto String
    | GotoIf AST.Expr String
    | LocalVar String AST.Expr
    | Expr AST.Expr
    deriving Show

mangleIdent :: AST.Module -> AST.IdentKind -> String -> String
mangleIdent mod kind s = if kind == AST.GlobalIdent 
    then mangleString (AST.modName mod) ++ '_' : mangleString s 
    else mangleString s
    where 
        mangleString [] = []
        mangleString (x:xs) | isAlphaNum x || x == '_' = x : mangleString xs
                            | otherwise = '$' : showHex (ord x) (mangleString xs)

normalize :: AST.Module -> [Decl]
normalize mod = map (normalizeDecl mod) $ AST.modDecls mod
    where 
        normalizeDecl mod (AST.Function name params body) = uncurry (Function (mangleIdent mod AST.GlobalIdent name) (map (mangleIdent mod AST.LocalIdent) params)) (normalizeExpr body)
        normalizeDecl mod (AST.Global name value) = Global (mangleIdent mod AST.GlobalIdent name) value
        normalizeExpr expr = ([], [Expr expr])

