module LlvmCompiler where

import Instant.Abs
    ( Stmt(..), Program(..), Exp(..), Ident(Ident))
import Control.Monad.Trans.Except
    ( ExceptT,
      throwE )
import Control.Monad.Trans.Class ( MonadTrans(lift))
import Control.Monad.Trans.State (StateT, get, put, execStateT)
import qualified Data.Set as Set
import Data.Foldable (find)

data CompilerS = CompilerS {idents :: Set.Set String, tmpReg :: Int, code :: String}
type CompilerStateT = StateT CompilerS (ExceptT String IO)

initCompilerS :: CompilerS
initCompilerS = CompilerS {
    idents = Set.empty,
    tmpReg = 0,
    code = ""
}

generateCode :: String -> CompilerStateT ()
generateCode s = do
    st <- get
    put $ CompilerS {idents = idents st, tmpReg = tmpReg st, code = code st ++ "  " ++ s ++ "\n"}

addIdent :: String -> CompilerStateT ()
addIdent ident = do
    st <- get
    if isMember ident (idents st) then
        return ()
    else
        put $ CompilerS {idents = Set.insert (reg ident) (idents st), tmpReg = tmpReg st, code = code st}

isMember :: String -> Set.Set String -> Bool
isMember s = Set.member (reg s)

nextReg :: CompilerStateT String
nextReg = do
    st <- get
    put $ CompilerS {idents = idents st, tmpReg = tmpReg st + 1, code = code st}
    return $ "%tmp" ++ show (tmpReg st)

reg :: String -> String
reg s = "%" ++ s
-----------------------------------------------------------------------------------------------------------------------

compile :: Program -> ExceptT String IO String
compile (Prog stmts) = do
    endState <- execStateT (mapM_ compileStmt stmts) initCompilerS
    return $ boilerplate (code endState)

compileStmt :: Stmt -> CompilerStateT ()
compileStmt (SAss (Ident ident) e) = do
    val <- compileExpr e
    st <- get
    if isMember ident (idents st) then
        generateCode $ "store i32 " ++ val ++ ", i32* " ++ reg ident
    else do
        addIdent ident
        generateCode $ reg ident ++ " = alloca i32"
        generateCode $ "store i32 " ++ val ++ ", i32* " ++ reg ident

compileStmt (SExp e) = do
    val <- compileExpr e
    generateCode $ "call void @printInt(i32 " ++ val ++ ")"


compileAExpr :: Exp -> Exp -> String -> CompilerStateT String
compileAExpr e1 e2 ins = do
    val1 <- compileExpr e1
    val2 <- compileExpr e2
    register <- nextReg
    generateCode $ register ++ " = " ++ ins ++ " i32 " ++ val1 ++ ", " ++ val2
    return register


compileExpr :: Exp -> CompilerStateT String
compileExpr (ExpAdd e1 e2) = do
    compileAExpr e1 e2 "add"

compileExpr (ExpSub e1 e2) = do
    compileAExpr e1 e2 "sub"

compileExpr (ExpMul e1 e2) = do
    compileAExpr e1 e2 "mul"

compileExpr (ExpDiv e1 e2) = do
    compileAExpr e1 e2 "sdiv"

compileExpr (ExpLit x) = return $ show x

compileExpr (ExpVar (Ident ident)) = do
    st <- get
    if isMember ident (idents st) then do
        register <- nextReg
        generateCode $ register ++ " = load i32, i32* " ++ reg ident
        return register
    else
        lift $ throwE "Undefined identifier"

boilerplate :: String -> String
boilerplate code =
    "declare i32 @printf(i8*, ...)\n\n" ++
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n" ++
    "define void @printInt(i32 %x) {\n" ++
    "  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
    "  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
    "  ret void\n" ++
    "}\n\n" ++
    "define i32 @main() {\n" ++
    "entry:  " ++
    code ++
    "  ret i32 0\n" ++
    "}"

