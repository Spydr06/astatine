{-# LANGUAGE TupleSections #-}

module LibAstatine.Backend (
    generate    
) where

import LibAstatine.Error (Result (..))
import LibAstatine.Context

import qualified LibAstatine.IR as IR;
import qualified LibAstatine.AST as AST;

import qualified GccJit
import GccJit.Utils (release)

import Control.Monad
import Foreign.C (CString, newCString)
import Foreign (FunPtr, Ptr, nullPtr, castPtr)
import Foreign.Marshal.Array
import System.Environment (getEnvironment)
import System.Exit (die)
import System.IO

import Data.Functor ((<&>))
import Data.Maybe (fromJust)

type JIT = Ptr GccJit.Context

data PrimTypes = PrimTypes {
    uInt8 :: Ptr GccJit.Type,
    uInt64 :: Ptr GccJit.Type,
    sizeT :: Ptr GccJit.Type,
    uChar :: Ptr GccJit.Type,
    double :: Ptr GccJit.Type,
    bool :: Ptr GccJit.Type,
    voidPtr :: Ptr GccJit.Type,
    charPtr :: Ptr GccJit.Type
}

buildPrimTypes :: JIT -> IO PrimTypes
buildPrimTypes jit = do
    uInt8 <- GccJit.contextGetType jit GccJit.UInt8
    uInt64 <- GccJit.contextGetType jit GccJit.UInt64
    sizeT <- GccJit.contextGetType jit GccJit.SizeT
    double <- GccJit.contextGetType jit GccJit.Double
    uChar <- GccJit.contextGetType jit GccJit.UnsignedChar
    bool <- GccJit.contextGetType jit GccJit.Bool
    voidPtr <- GccJit.contextGetType jit GccJit.VoidPtr
    charPtr <- GccJit.contextGetType jit GccJit.Char >>= GccJit.typeGetPointer 
    return $ PrimTypes uInt8 uInt64 sizeT uChar double bool voidPtr charPtr 

data Datatype = ATNil
    | ATInt
    | ATFlt
    | ATChar
    | ATBool
    | ATPtr
    | ATList
    deriving Enum

builtinIntrns :: PrimTypes -> [(String, [Ptr GccJit.Type])]
builtinIntrns p = [
        ("at_slit", [charPtr p]),
        ("at_list_from_arr", [sizeT p, voidPtr p])
    ]

buildBuiltins :: JIT -> Ptr GccJit.Type -> [(String, [Ptr GccJit.Type])] -> IO [(String, Ptr GccJit.Function)]
buildBuiltins jit at = mapM (\(name, arity) -> build jit at name arity <&> (name,))
    where build jit at name params = do
            params <- mapM (\(i, typ) -> GccJit.contextNewParam jit nullPtr typ $ show i) $ zip [0 :: Int ..] params
            GccJit.contextNewFunction jit nullPtr GccJit.FunctionImported at name params False
 
data Generator = Generator {
    primTypes :: PrimTypes,
    atType :: Ptr GccJit.Type,
    atStruct :: Ptr GccJit.Struct,
    atData :: Ptr GccJit.Type,
    
    intrinsics :: [(String, Ptr GccJit.Function)],

    currentFunc :: Maybe (Ptr GccJit.Function),
    currentBlock :: Maybe (Ptr GccJit.Block)
}

newGen :: JIT -> IO Generator
newGen jit = do
        prims <- buildPrimTypes jit
        (atData, at) <- buildATType jit prims
        intrns <- buildBuiltins jit (castPtr at) $ builtinIntrns prims
        return $ Generator prims (castPtr at) at atData intrns Nothing Nothing
    where buildATType jit prims = do
            unFields <- mapM (\(t, id) -> GccJit.contextNewField jit nullPtr (t prims) id) 
                        [(uInt64, "integer"), (double, "floating"), (bool, "boolean"), (voidPtr, "ptr")]
            valUnion <- GccJit.contextNewUnionType jit nullPtr "at_val_data_t" unFields
            valField <- GccJit.contextNewField jit nullPtr valUnion "value"

            dtField <- GccJit.contextNewField jit nullPtr (uInt8 prims) "datatype" 
            valStruct <- GccJit.contextNewStructType jit nullPtr "at_val_t" [valField, dtField]
            return (valUnion, valStruct)
         

type MainFunction = Int -> Ptr CString -> Ptr CString -> IO Int
foreign import ccall "dynamic" mkFunc :: FunPtr MainFunction -> MainFunction

toCArgs :: [String] -> IO (Ptr CString)
toCArgs args = do
    cstrs <- mapM newCString args
    newArray0 nullPtr (cstrs ++ [nullPtr])

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

generate :: Context -> [IR.Decl] -> IO (Result ())
generate ctx mod = do
    jit <- unwrapOrDie GccJit.contextAcquire "Could not acquire libgccjit context" 
    GccJit.setBoolOption jit GccJit.DumpGeneratedCode $ verbose ctx

    print mod
    gen <- newGen jit

    mapM_ (generateDecl jit gen) mod  
    mapM_ (GccJit.contextAddDriverOption jit) $ driverOpts ctx
    GccJit.contextAddDriverOption jit $ runtimeFile ctx
    
    GccJit.contextDumpToFile jit "dump.out" 1
    case outputFile ctx of
        Executable path -> GccJit.contextCompileToFile jit GccJit.Executable path
        SharedLibrary path -> GccJit.contextCompileToFile jit GccJit.DynamicLibrary path
        ObjectFile path -> GccJit.contextCompileToFile jit GccJit.ObjectFile path
        AssemblyFile path -> GccJit.contextCompileToFile jit GccJit.Assembler path
        RunInPlace args -> do
            result <- unwrapOrDie (GccJit.contextCompile jit) "Could not compile"
            main <- unwrapOrDie (GccJit.resultGetCode result "main") "No `main` function found."

            let argv = inputFilePath (inputFile ctx) : args
            let argc = length argv
            
            argv' <- toCArgs argv
            envp <- getEnvironment >>= \env -> toCArgs [key ++ "=" ++ value | (key, value) <- env]
            exitCode <- mkFunc main argc argv' envp
            unless (silent ctx) $ do
                putStrLn $ '[' : head argv ++ " exited with code " ++ show exitCode ++ "]"

            hFlush stdout
            release result

    release jit
    return $ Ok ()

generateVal :: JIT -> Generator -> Datatype -> Maybe (Ptr GccJit.RValue) -> IO (Ptr GccJit.RValue)
generateVal jit gen dt val = do 
        dtField <- unwrapOrDie (GccJit.structGetField (atStruct gen) 1) "NULL at_val_t.datatype field"
        dtVal <- GccJit.contextNewRValueFromInt jit (uInt8 $ primTypes gen) $ fromEnum dt

        fields <- case val of
            Nothing -> return [(dtField, Just dtVal)]
            Just val' -> do
                valField <- unwrapOrDie (GccJit.structGetField (atStruct gen) 0) "NULL at_val_t.value field"
                valVal <- unwrapOrDie (packVal jit gen dt val') "NULL at_val_t.value constructor"
                return [(valField, Just valVal), (dtField, Just dtVal)]
        unwrapOrDie (GccJit.contextNewStructConstructor jit nullPtr (atType gen) (Right fields)) "NULL struct"
    where
        packVal _ _ ATNil _ = undefined
        packVal jit gen _ val = GccJit.contextNewUnionConstructor jit nullPtr (atData gen) Nothing $ Just val

generateNil :: JIT -> Generator -> IO (Ptr GccJit.RValue)
generateNil jit gen = generateVal jit gen ATNil Nothing

generateInt :: JIT -> Generator -> Integer -> IO (Ptr GccJit.RValue)
generateInt jit gen i = do
    iVal <- GccJit.contextNewRValueFromLong jit (uInt64 $ primTypes gen) $ fromIntegral i
    generateVal jit gen ATInt $ Just iVal

generateString :: JIT -> Generator -> String -> IO (Ptr GccJit.RValue)
generateString jit gen s = do
    sVal <- GccJit.contextNewStringLiteral jit s
    GccJit.contextNewCall jit nullPtr (fromJust $ lookup "at_slit" $ intrinsics gen) [sVal]

generateList :: JIT -> Generator -> [AST.Expr] -> IO (Ptr GccJit.RValue)
generateList jit gen elems = do
    let len = length elems
    arrType <- GccJit.contextNewArrayType jit nullPtr (atType gen) len
    arr <- GccJit.functionNewLocal (fromJust $ currentFunc gen) nullPtr arrType ""
    
    mapM (generateExpr jit gen) elems >>=
        GccJit.contextNewArrayConstructor jit nullPtr arrType >>=
        GccJit.blockAddAssignment (fromJust $ currentBlock gen) nullPtr arr . fromJust

    arrVal <- GccJit.lValueGetAddress arr nullPtr
    lenVal <- GccJit.contextNewRValueFromInt jit (sizeT $ primTypes gen) len
    GccJit.contextNewCall jit nullPtr (fromJust $ lookup "at_list_from_arr" $ intrinsics gen) [lenVal, arrVal]
     
generateChar :: JIT -> Generator -> Char -> IO (Ptr GccJit.RValue)
generateChar jit gen c = do
    cVal <- GccJit.contextNewRValueFromInt jit (uChar $ primTypes gen) $ fromEnum c
    generateVal jit gen ATChar $ Just cVal

generateBool :: JIT -> Generator -> Bool -> IO (Ptr GccJit.RValue)
generateBool jit gen b = do
    bVal <- GccJit.contextNewRValueFromInt jit (bool $ primTypes gen) $ fromEnum b
    generateVal jit gen ATBool $ Just bVal

generateDecl :: JIT -> Generator -> IR.Decl -> IO ()
generateDecl jit gen (IR.Function name params body) = do
    params' <- mapM (GccJit.contextNewParam jit nullPtr $ atType gen) params
    func <- GccJit.contextNewFunction jit nullPtr GccJit.FunctionExported (atType gen) name params' False
    block <- unwrapOrDie (GccJit.functionNewBlock func Nothing) "NULL block"
    generateProcedure jit gen { currentFunc = Just func, currentBlock = Just block } body >>= GccJit.blockEndWithReturn block nullPtr

generateDecl _ _ (IR.Global _ _) = undefined

generateProcedure :: JIT -> Generator -> IR.Procedure AST.Expr -> IO (Ptr GccJit.RValue)
generateProcedure jit gen (IR.Procedure decls stmts ret) = do
        mapM_ (generateLocal jit gen) decls
        mapM_ (generateStmt jit gen) stmts
        generateExpr jit gen ret
    where generateLocal jit gen decl = return ()

generateStmt :: JIT -> Generator -> IR.Stmt -> IO (Ptr GccJit.RValue)
generateStmt jit gen (IR.Expr exp) = generateExpr jit gen exp
generateStmt _ _ _ = undefined

generateExpr :: JIT -> Generator -> AST.Expr -> IO (Ptr GccJit.RValue)
generateExpr jit gen (AST.ConstInt i) = generateInt jit gen i
-- generateExpr jit gen (AST.ConstFloat f) = generateFloat jit gen f
generateExpr jit gen (AST.ConstString s) = generateString jit gen s
generateExpr jit gen (AST.ConstChar c) = generateChar jit gen c
generateExpr jit gen AST.ConstTrue = generateBool jit gen True
generateExpr jit gen AST.ConstFalse = generateBool jit gen False
generateExpr jit gen AST.ConstNil = generateNil jit gen
generateExpr jit gen (AST.ListExpr elems) = generateList jit gen elems
generateExpr _ _ _ = undefined


