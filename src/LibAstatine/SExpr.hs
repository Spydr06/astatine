module LibAstatine.SExpr (
    parse,
    parseAll,
    SExpr(..)
) where

import qualified LibAstatine.Token as Token

import LibAstatine.Util.Position (Positioned (positioned))
import LibAstatine.Error (Result (..), CompilerError(UnexpectedEof, ParseError))

data PairKind = Round | Curly | Square
    deriving Show

data SExpr = Identifier String
    | IntegerLiteral Integer
    | FloatLiteral Double
    | StringLiteral String
    | CharLiteral Char
    | STrue
    | SFalse
    | Nil
    | PairNil PairKind
    | Quote SExpr
    | Pair PairKind SExpr SExpr
    deriving Show

parseAll :: [Positioned Token.Token] -> Result [SExpr]
parseAll [] = Ok []
parseAll tokens = let (result, remaining) = parse tokens in
    result >>= \expr -> (expr:) <$> parseAll remaining

parse :: [Positioned Token.Token] -> (Result SExpr, [Positioned Token.Token])
parse [] = (Ok Nil, [])
parse (t:ts) = case positioned t of
    Token.StringLiteral s -> (Ok $ StringLiteral s, ts)
    Token.FloatLiteral f -> (Ok $ FloatLiteral $ read f, ts)
    Token.IntegerLiteral i -> (Ok $ IntegerLiteral $ read i, ts)
    Token.CharLiteral c -> (Ok $ CharLiteral c, ts)
    Token.EscapedCharLiteral c -> (CharLiteral <$> escapeChar c, ts)
    Token.Identifier "nil" -> (Ok Nil, ts)
    Token.Identifier "true" -> (Ok STrue, ts)
    Token.Identifier "false" -> (Ok SFalse, ts)
    Token.Identifier id -> (Ok $ Identifier id, ts)
    Token.QuoteChar -> let (result, ts') = parse ts in (Quote <$> result, ts')
    Token.LParen -> parsePair ts Round Token.RParen
    Token.LBrace -> parsePair ts Curly Token.RBrace
    Token.LBracket -> parsePair ts Square Token.RBracket
    _ -> (Err $ ParseError t "unexpected token", ts)

parsePair :: [Positioned Token.Token] -> PairKind -> Token.Token -> (Result SExpr, [Positioned Token.Token])
parsePair [] _ _ = (Err $ UnexpectedEof "expected `)`", [])
parsePair (t:ts) kind closing | positioned t == closing = (Ok $ PairNil kind, ts)
parsePair ts kind closing = let (fst, ts') = parse ts in case fst of
    Ok fst' -> let (snd, ts'') = parsePair ts' kind closing in case snd of
        Ok snd' -> (Ok $ Pair kind fst' snd', ts'')
        Err err -> (Err err, ts'')
    Err err -> (Err err, ts')

escapeChar :: Char -> Result Char
escapeChar = Ok -- TODO: resolve escape-codes

