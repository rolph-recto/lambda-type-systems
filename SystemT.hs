{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- SystemT.hs
-- Typechecker for Goedel's System T (Simply Typed Lambda Calculus)

type Name = String

data Type = TNum
          | TBool
          | TFun Type Type
          deriving (Eq)

instance Show Type where
  show t = case t of
    TNum  -> "Num"
    TBool -> "Bool"
    TFun t1 t2 -> "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"

newtype Bind = Bind (Name, Type) deriving (Eq)

instance Show Bind where
  show (Bind (v,ty)) = v ++ ":" ++ (show ty)

data Term = Num Int
          | Plus Term Term
          | Boolean Bool
          | Lam Bind Term
          | App Term Term
          | Var Name
          | Iter Term Bind Term
          | Annot Term Type
            deriving (Eq)

instance Show Term where
  show t = case t of
    Num n -> show n
    Plus t1 t2 -> (show t1) ++ "+" ++ (show t2)
    Boolean b -> show b
    Lam b t -> "(\\" ++ (show b) ++ "." ++ (show t) ++ ")"
    App t1 t2 -> (show t1) ++ " " ++ (show t2)
    Var name -> name
    Annot t ty -> (show t) ++ " : " ++ (show ty)

type Context = [Bind]

typeErrMsg :: Term -> Type -> String
typeErrMsg t ty = (show t) ++ " does not have type " ++ (show ty)

getBind :: Context -> Name -> Maybe Type
getBind [] _ = Nothing
getBind (Bind (v,vty):cs) name = if v == name then Just vty else getBind cs name

-- we can recursively descend down the AST of a System T expression
-- because lambda abstraction bindings are annotated with types.
-- if they are not, we would need to resort to bidirectional
-- typechecking to allow "global" annotations guide typechecking.
inferType :: Context -> Term -> Maybe Type
inferType ctx t = case t of
  Num _ -> return TNum

  Plus t1 t2 -> do
    ty1 <- inferType ctx t1
    ty2 <- inferType ctx t2
    case (ty1, ty2) of
      (TNum, TNum) -> return TNum
      otherwise    -> Nothing

  Boolean _ -> return TBool

  Lam b@(Bind (_,ty1)) t2 -> do
    ty2 <- inferType (b:ctx) t2
    return (TFun ty1 ty2)

  App f t1 -> do
    fty <- inferType ctx f
    -- f must be a lambda abstraction!
    case fty of
      TFun ty1 ty2 -> do
        ty1' <- inferType ctx t1
        if ty1 == ty1' then return ty2 else Nothing
      _ -> Nothing

  Var v -> getBind ctx v

  Iter n x body -> do
    nty <- inferType ctx n
    if nty == TNum then inferType (x:ctx) body else Nothing

  Annot t' ty -> do
    ty' <- inferType ctx t'
    if ty == ty' then return ty else Nothing

main = do
  let t = App (App (Lam (Bind ("x",TNum)) (Lam (Bind ("y",TNum)) (Plus (Var "x") (Var "y")))) (Num 3)) (Num 2)
  case inferType [] t of
    Just ty -> putStrLn $ (show t) ++ " has type " ++ (show ty)
    Nothing -> putStrLn $ (show t) ++ " is not well-typed!"

          
