module JvmCompiler where

import Instant.Abs
    ( Stmt(..), Program(..), Exp(..), Ident(Ident))
import Control.Monad.Trans.Except
    ( ExceptT,
      throwE )
import Control.Monad.Trans.Class ( MonadTrans(lift))
import qualified Data.Map as Map
import Control.Monad.Trans.State (StateT, get, put, execStateT)

data CompilerS = CompilerS {idents :: Map.Map String Int, stack :: Int, maxStack :: Int, code :: String}
type CompilerStateT = StateT CompilerS (ExceptT String IO)

data HExp
    = HExpAdd HExp HExp Int 
    | HExpSub HExp HExp Int 
    | HExpMul HExp HExp Int 
    | HExpDiv HExp HExp Int 
    | HExpLit Integer Int 
    | HExpVar Ident Int 
  deriving (Eq, Ord, Show, Read)

initCompilerS :: CompilerS
initCompilerS = CompilerS {
    idents = Map.fromList [("__________args", 0)],
    stack = 0,
    maxStack = 0,
    code = ""
}

incStack:: CompilerStateT ()
incStack = do
    st <- get
    put $ CompilerS {idents = idents st, stack = stack st + 1, maxStack = max (maxStack st) (stack st + 1), code = code st}

decStack :: CompilerStateT ()
decStack = do
    st <- get
    put $ CompilerS {idents = idents st, stack = max 0 (stack st - 1), maxStack = maxStack st, code = code st}

generateCode :: String -> CompilerStateT ()
generateCode s = do
    st <- get
    put $ CompilerS {idents = idents st, stack = stack st, maxStack = maxStack st, code = code st ++ "  " ++ s ++ "\n"}

addIdent :: String -> CompilerStateT ()
addIdent ident = do
    st <- get
    case Map.lookup ident $ idents st of
      Nothing -> do
        put $ CompilerS {idents = Map.insert ident (Map.size $ idents st) $ idents st, stack = stack st, maxStack = maxStack st, code = code st}
      Just _ -> return ()
        
m1 = -1
ifive =  5
i8 = 127
mi8 = -128
i16 = 32767
mi16 = -32768
-----------------------------------------------------------------------------------------------------------------------

compile :: Program -> String -> ExceptT String IO String
compile (Prog stmts) className = do
    endState <- execStateT (mapM_ compileStmt stmts) initCompilerS
    return $ boilerplate className (code endState) (Map.size $ idents endState) (maxStack endState)

compileStmt :: Stmt -> CompilerStateT ()
compileStmt (SAss (Ident ident) e) = do
    compileExpr $ heightSynthesis e
    addIdent ident
    st <- get
    case Map.lookup ident $ idents st of
      Nothing -> lift $ throwE "Undefined error"
      Just index -> generateCode $ "istore" ++ (if index <= 3 then "_" else " ") ++ show index
    decStack

compileStmt (SExp e) = do
    incStack
    generateCode "getstatic java/lang/System/out Ljava/io/PrintStream;"
    compileExpr $ heightSynthesis e
    generateCode "invokevirtual java/io/PrintStream/println(I)V"
    decStack
    decStack

compileAExpr :: HExp -> HExp -> String -> CompilerStateT ()
compileAExpr e1 e2 ins = do
    let h1 = height e1
    let h2 = height e2
    if h1 < h2 && (ins == "iadd" || ins == "imul") then do
        compileExpr e2
        compileExpr e1
    else do
        compileExpr e1
        compileExpr e2
    generateCode ins
    decStack

compileExpr :: HExp -> CompilerStateT ()
compileExpr (HExpAdd e1 e2 _) = do
    compileAExpr e1 e2 "iadd"

compileExpr (HExpSub e1 e2 _) = do
    compileAExpr e1 e2 "isub"

compileExpr (HExpMul e1 e2 _) = do
    compileAExpr e1 e2 "imul"

compileExpr (HExpDiv e1 e2 _) = do
    compileAExpr e1 e2 "idiv"

compileExpr (HExpLit x _) = do
    incStack
    generateCode $
        if x == m1 then "iconst_m1"
        else if 0 <= x && x <= ifive then "iconst_" ++ show x
        else if mi8 <= x && x <= i8 then "bipush " ++ show x
        else if mi16 <= x && x <= i16 then "sipush " ++ show x
        else "ldc " ++ show x

compileExpr (HExpVar (Ident ident) _) = do
    st <- get
    case Map.lookup ident $ idents st of
      Nothing -> lift $ throwE "Undefined identifier"
      Just i -> do
        incStack
        generateCode $ ("iload" ++ if i <= 3 then "_" else " ") ++ show i

--------------------------------------------------------------------------------------------------------------------

heightSynthesis :: Exp -> HExp
heightSynthesis (ExpAdd e1 e2) =
    let es1 = heightSynthesis e1 in
    let es2 = heightSynthesis e2 in
    let h1 = height es1 in
    let h2 = height es2 in
    HExpAdd es1 es2 $ max h1 h2 + 1

heightSynthesis (ExpSub e1 e2) =
    let es1 = heightSynthesis e1 in
    let es2 = heightSynthesis e2 in
    let h1 = height es1 in
    let h2 = height es2 in
    HExpSub es1 es2 $ max h1 h2 + 1

heightSynthesis (ExpMul e1 e2) =
    let es1 = heightSynthesis e1 in
    let es2 = heightSynthesis e2 in
    let h1 = height es1 in
    let h2 = height es2 in
    HExpMul es1 es2 $ max h1 h2 + 1

heightSynthesis (ExpDiv e1 e2) =
    let es1 = heightSynthesis e1 in
    let es2 = heightSynthesis e2 in
    let h1 = height es1 in
    let h2 = height es2 in
    HExpDiv es1 es2 $ max h1 h2 + 1

heightSynthesis (ExpLit x) = HExpLit x 1
heightSynthesis (ExpVar ident) = HExpVar ident 1

height :: HExp -> Int
height (HExpAdd _ _ h) = h
height (HExpSub _ _ h) = h
height (HExpDiv _ _ h) = h
height (HExpMul _ _ h) = h
height (HExpLit _ h) = h
height (HExpVar _ h) = h

boilerplate :: String -> String -> Int -> Int -> String
boilerplate className code localNum stackSize =
    ".class public " ++ className ++
    "\n.super java/lang/Object\n\n" ++

    ".method public <init>()V\n" ++
        "  aload_0\n" ++
        "  invokespecial java/lang/Object/<init>()V\n" ++
        "  return\n" ++
    ".end method\n\n" ++

    ".method public static main([Ljava/lang/String;)V\n" ++
    ".limit locals " ++ show localNum ++ "\n" ++
    ".limit stack " ++ show stackSize ++ "\n" ++
    code ++
    "  return" ++
    "\n.end method"

