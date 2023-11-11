module LibAstatine.Lexer (
    lexFile
) where

import LibAstatine.Context
import LibAstatine.File (SourceFile (sourceLines, sourceFilePath))
import LibAstatine.Error
import LibAstatine.Token
import LibAstatine.Util.Position

lexFile :: Context -> SourceFile -> Result [Token]
lexFile ctx sourcefile = Err $ LexerError (Token (Keyword "import") (Position (sourceFilePath sourcefile) 0 0) $ Span 0 6) "Test error" (Just $ head $ sourceLines sourcefile) $ Just "Bla bla bla"

