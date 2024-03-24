module LibAstatine.IR (
    Decl(..),
    Procedure(..),
    Stmt(..),
    normalize
) where

import qualified LibAstatine.AST as AST;
import Data.Char (isAlphaNum, ord)
import Numeric (showHex)

data Decl = Function {
        funcName :: String,
        funcParams :: [String],
        funcBody :: Procedure AST.Expr
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

data Procedure a = Procedure [Decl] [Stmt] a
    deriving Show

instance Functor Procedure where
    fmap f p = let Procedure d s a = p in 
                   Procedure d s $ f a

instance Applicative Procedure where
    pure = Procedure [] []
    f <*> p = let Procedure d s f' = f
                  Procedure d' s' a = fmap f' p
              in Procedure (d ++ d') (s ++ s') a

instance Monad Procedure where
    p >>= f = let Procedure d s a = p
                  Procedure d' s' a' = f a
              in Procedure (d ++ d') (s ++ s') a' 

mangleIdent :: AST.Module -> AST.IdentKind -> String -> String
mangleIdent mod kind s = if kind == AST.GlobalIdent 
    then mangleString (AST.modName mod) ++ '_' : mangleString s 
    else mangleString s
    where 
        mangleString [] = []
        mangleString (x:xs) | isAlphaNum x || x == '_' = x : mangleString xs
                            | x == '.' = '_' : mangleString xs
                            | otherwise = '$' : showHex (ord x) (mangleString xs)

normalize :: AST.Module -> [Decl]
normalize mod = map (normalizeDecl mod) $ AST.modDecls mod

normalizeDecl :: AST.Module -> AST.Decl -> Decl
normalizeDecl mod (AST.Function name params body) = Function (mangleIdent mod AST.GlobalIdent name) (map (mangleIdent mod AST.LocalIdent) params) (normalizeExpr mod body)
normalizeDecl mod (AST.Global name value) = Global (mangleIdent mod AST.GlobalIdent name) value

normalizeExpr :: AST.Module -> AST.Expr -> Procedure AST.Expr
normalizeExpr mod (AST.CallExpr x xs) = do
            x' <- normalizeExpr mod x
            xs' <- mapM (normalizeExpr mod) xs
            return $ AST.CallExpr x' xs'
normalizeExpr mod (AST.DoExpr xs) = normalizeDo mod xs
normalizeExpr mod (AST.IdentExpr k id) = return $ AST.IdentExpr k $ mangleIdent mod k id
normalizeExpr _ expr | AST.isConst expr = return expr
                     | otherwise = undefined

normalizeDo :: AST.Module -> [AST.Expr] -> Procedure AST.Expr
normalizeDo _ [] = return AST.ConstNil
normalizeDo mod [x] = normalizeExpr mod x
normalizeDo mod (x:xs) = let Procedure d s exp = normalizeExpr mod x
                             Procedure d' s' exp' = normalizeDo mod xs 
                         in Procedure (d ++ d') (s ++ Expr exp : s') exp'

