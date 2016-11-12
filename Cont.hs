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
            | CallCC Int Expr
            deriving (Show)

type Cont   = Expr -> Expr
type Stack  = [Cont]
type Env    = M.Map Int Expr

isValue :: Expr -> Bool
isValue expr = case expr of
  ILit _ -> True
  Var _ -> True
  Lam _ _ -> True
  App _ _ -> False
  CallCC _ _ -> False

eval :: Expr -> ExceptT String (ReaderT Env (State Stack)) Expr
eval expr
  | ILit n <- expr = ret expr

  | Var v <- expr = do
    env <- ask
    case M.lookup v env of
      Nothing -> throwError $ "unexpected free variable: v" ++ (show v)
      Just val -> ret val

  | Lam param body <- expr = ret expr

  | App (Lam var body) arg <- expr, isValue arg = do
    local (M.insert var arg) (eval body)

  | App (Lam var body) arg <- expr, not (isValue arg) = do
    let appcont carg = App (Lam var body) carg
    st <- get
    put (appcont:st)
    eval arg

  | CallCC k body <- expr = do
    -- reify the current continuation into a function
    st <- get 
    let contf = foldr (.) id st
    let cont = Lam (-1) (contf (Var (-1)))

    -- smash the control stack
    put []

    -- evaluate the body
    local (M.insert k cont) (eval body)

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

ret :: Expr -> ExceptT String (ReaderT Env (State Stack)) Expr
ret expr = do
  st <- get
  case st of
    [] -> return expr
    (c:cs) -> do
      put cs
      eval (c expr)

main = do
  let expr = App (Lam 0 (Plus (ILit 2) (Var 0))) (CallCC 0 (ILit 10))
  let res = evalState (runReaderT (runExceptT (eval expr)) M.empty) []
  case res of
    Left err -> putStrLn err
    Right rval -> print rval
    
