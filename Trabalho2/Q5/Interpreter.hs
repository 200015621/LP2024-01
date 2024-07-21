module Q5.Interpreter where

import Q5.AbsLI

-- As definições deste arquivo são as mínimas para compilar os testes.
-- Você deverá completar todo o restante do código.
-- Dica: se você fez os exercícios anteriores, boa parte do código
-- pode ser reutilizado neste exercício.

import Prelude hiding (lookup)

type RContext = [(String, Valor)]

type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool



execute :: RContext -> Stm ->  Either ErrorMessage RContext
execute context x = case x of
          SAss id exp -> case eval context exp of 
                Right ve1 -> Right (update context (getStr id) ve1)
                Left msg -> Left msg
          SBlock [] -> Right context
          SBlock (s:stms) -> case execute context s of
                Right vcontext -> execute vcontext (SBlock stms)
                Left msg -> Left msg
          SWhile exp stm -> case eval context exp of
                Right ve1 -> if i ve1 /= 0
                                then case execute context stm of 
                                        Right vcontext -> execute vcontext (SWhile exp stm)
                                        Left msg -> Left msg
                                else Right context
                Left msg -> Left msg
          SdoWhile stm exp -> case execute context stm of
                Right vcontext -> execute vcontext (SWhile exp stm)
                Left msg -> Left msg
          STry [] _ stmsF -> execute context (SBlock stmsF)
          STry (s:stms) stmsC stmsF -> case execute context s of
                Right vcontext ->execute vcontext (STry stms stmsC stmsF)
                Left msg -> case execute context (SBlock stmsC) of
                        Right vcontext -> execute vcontext (SBlock stmsF)
                        Left msg -> Left msg



eval :: RContext -> Exp ->  Either ErrorMessage Valor
eval context x = let normal_eval = \f v s exp0 exp -> case eval context exp0 of 
                              Right ve1 -> case eval context exp of
                                Right ve2 -> Right (v (f (s ve1) (s ve2)))
                                Left msg -> Left msg
                              Left msg -> Left msg
                 in case x of
  EAdd exp0 exp -> normal_eval (+) ValorInt i exp0 exp
  ESub exp0 exp -> normal_eval (-) ValorInt i exp0 exp
  EMul exp0 exp -> normal_eval (*) ValorInt i exp0 exp
  ECon exp0 exp -> normal_eval (++) ValorStr s exp0 exp
  EDiv e1 e2 -> case eval context e1 of
                    Right ve1 -> case eval context e2 of
                      Right ve2 -> if i ve2 == 0
                                   then Left "divisao por 0"
                                   else Right (ValorInt ( i ve1 `div` i ve2))
                      Left msg -> Left msg
                    Left msg -> Left msg
  EInt n   -> Right (ValorInt n)
  EVar id  -> Right (lookup context (getStr id))
  EStr str -> Right (ValorStr str)
  EOr exp0 exp -> normal_eval (||) ValorBool b exp0 exp
  EAnd exp0 exp -> normal_eval (&&) ValorBool b exp0 exp
  ENot exp -> case eval context exp of 
        Right ve1 -> Right (ValorBool (not (b ve1)))
        Left msg -> Left msg
  ETrue    -> Right (ValorBool True)
  EFalse   -> Right (ValorBool False)


getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv