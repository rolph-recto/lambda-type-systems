-- Cont.hs
-- A simple language with continuations

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Strict as M

data Expr   = ILit Int
            | Var Int
            | Plus Expr Expr
            | Lam Int Expr
            | App Expr Expr
            | Unit
            -- commands
            | Print Expr
            | Seq [Expr]
            | Cont Int Expr
            | CallCC Int Expr
            deriving (Show)

type Env    = M.Map Int Expr  -- environment
type DCont   = Expr -> Expr   -- delimited continuation
type InterpState = [DCont]    -- control stack

isValue :: Expr -> Bool
isValue expr = case expr of
  ILit _ -> True
  Var _ -> True
  Lam _ _ -> True
  App _ _ -> False
  Unit -> True
  Print _ -> False
  Seq [] -> True
  Seq _ -> False
  Cont _ _ -> True
  CallCC _ _ -> False

type InterpM a = ExceptT String (ReaderT Env (StateT InterpState IO)) a

pushToStack :: DCont -> InterpM ()
pushToStack cont = (cont:) <$> get >>= put

eval :: Expr -> InterpM Expr
eval expr
  | ILit n <- expr = ret expr

  | Var v <- expr = do
    env <- ask
    case M.lookup v env of
      Nothing -> throwError $ "unexpected free variable: v" ++ (show v)
      Just val -> ret val

  | Lam _ _ <- expr = ret expr

  | Cont _ _ <- expr = do
    ret expr

  | Unit <- expr = ret expr

  -- eager evaluation

  | App (Lam var body) arg <- expr, isValue arg = do
    local (M.insert var arg) (eval body)

  | App (Lam var body) arg <- expr, not (isValue arg) = do
    pushToStack $ App (Lam var body)
    eval arg

  -- Conts are basically the same as lambdas, except when arguments are
  -- applied to it we erase the control stack; ie Conts don't return
  | App (Cont var body) arg <- expr, isValue arg = do
    put []
    local (M.insert var arg) (eval body)

  | App (Cont var body) arg <- expr, not (isValue arg) = do
    pushToStack $ App (Cont var body)
    eval arg

  | App func arg <- expr = do
    let cont f = App f arg
    pushToStack cont
    eval func

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
    pushToStack $ const (Seq tlcmds)
    eval cmd

  | Seq [] <- expr = ret Unit

  | CallCC k body <- expr = do
    -- reify the current continuation into a function
    st <- get 
    let contf = foldr (.) id $ reverse st
    let cont = Cont (-1) (contf (Var (-1)))

    -- evaluate the body
    local (M.insert k cont) (eval body)

ret :: Expr -> InterpM Expr
ret expr = do
  st <- get
  case st of
    [] -> return expr
    (c:cs) -> do
      put cs
      eval (c expr)

main = do
  let subexpr = Seq [App (Var 0) Unit, Print (ILit 98)]
  let expr = Seq [CallCC 0 subexpr, Print (ILit 99)]
  let expr2 = Seq [CallCC 0 (Seq [Print (ILit 98), App (Var 0) Unit]), Print (ILit 99)]
  let initState = []
  res <- evalStateT (runReaderT (runExceptT (eval expr)) M.empty) initState
  putStrLn $ either id show res
