-- Cont.hs
-- A simple language with continuations

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Strict as M

data Expr   = ILit Int
            | BLit Bool
            | Var Int
            | Plus Expr Expr
            | Cond Expr Expr Expr
            | Lam Int Expr
            | App Expr Expr
            | Unit
            | CallCC Int Expr
            -- commands
            | Print Expr
            | Seq [Expr]
            deriving (Show)

type Env    = M.Map Int Expr
type Cont   = Expr -> Expr

-- stack should be a list of conts but also an environment
-- we should be able to recreate the current continuation,
-- which is a closure, from the stack
-- type Stack  = (Env, [Cont])
data InterpState = InterpState Env [Cont]

isValue :: Expr -> Bool
isValue expr = case expr of
  ILit _ -> True
  Var _ -> True
  Lam _ _ -> True
  App _ _ -> False
  Unit -> True
  CallCC _ _ -> False
  Print _ -> False
  Seq _ -> False

type InterpM a = ExceptT String (ReaderT Env (StateT InterpState IO)) a

pushToStack :: Cont -> InterpM ()
pushToStack cont = do
  InterpState _ st <- get
  env <- ask
  put $ InterpState env (cont:st)

eval :: Expr -> InterpM Expr
eval expr
  | ILit n <- expr = ret expr

  | Var v <- expr = do
    env <- ask
    case M.lookup v env of
      Nothing -> throwError $ "unexpected free variable: v" ++ (show v)
      Just val -> eval val

  | Lam param body <- expr = do
      ret expr

  | Unit <- expr = do
      ret expr

  -- eager evaluation
  | App (Lam var body) arg <- expr, isValue arg = do
    local (M.insert var arg) (eval body)

  | App (Lam var body) arg <- expr, not (isValue arg) = do
    pushToStack $ App (Lam var body)
    eval arg

  | App func arg <- expr = do
    env <- ask
    let cont f = App f arg
    pushToStack cont
    eval func

  | CallCC k body <- expr = do
    -- reify the current continuation into a function
    InterpState env st <- get 
    let contf = foldr (.) id st
    let cont = Lam (-1) (contf (Var (-1)))

    -- smash the control stack
    put $ InterpState M.empty []

    -- evaluate the body
    local (M.insert k cont . const env) (eval body)

  -- only intgers are allowed as direct arguments to plus;
  -- e.g. instead of writing Plus 2 (Plus 3 4) you should write
  -- App (Lam 0 (Plus 2 (Var 0))) (Plus 3 4)
  | Plus (ILit x) (ILit y) <- expr = ret (ILit $ x + y)

  | Plus (Var x) (ILit y) <- expr = do
    env <- ask
    case M.lookup x env of
      Nothing -> throwError $ "unexpected free variable: v" ++ (show x)
      Just (ILit xval) -> ret (ILit $ xval + y)
      otherwise -> throwError "expected integer argument to Plus"

  | Plus (ILit x) (Var y) <- expr = eval (Plus (Var y) (ILit x))
  
  | Plus (Var x) (Var y) <- expr = do
    env <- ask
    case M.lookup x env of
      Nothing -> throwError $ "unexpected free variable: v" ++ (show x)
      Just (ILit xval) -> eval (Plus (ILit xval) (Var y))
      otherwise -> throwError "expected integer argument to Plus"

  | Print pexpr <- expr, isValue pexpr = do
      liftIO $ print pexpr
      ret Unit

  | Print pexpr <- expr, not (isValue pexpr) = do
      pushToStack Print
      eval pexpr

  | Seq (cmd:tlcmds) <- expr = do
      InterpState _ st <- get
      pushToStack $ const (Seq tlcmds)
      eval cmd

  | Seq [] <- expr = do
      InterpState _ st <- get
      ret Unit

ret :: Expr -> InterpM Expr
ret expr = do
  InterpState env st <- get
  case st of
    [] -> return expr
    (c:cs) -> do
      put $ InterpState env cs
      eval (c expr)

main = do
  let expr = Seq [CallCC 0 (Seq [App (Var 0) Unit, App (Var 0) Unit, Print (ILit 98)]), Print (ILit 99)]
  let initState = InterpState M.empty []
  res <- evalStateT (runReaderT (runExceptT (eval expr)) M.empty) initState
  case res of
    Left err -> putStrLn err
    Right rval -> print rval
    
