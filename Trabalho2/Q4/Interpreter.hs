module Q4.Interpreter where

import Q4.AbsLI
import Prelude hiding (lookup)

type RContext = [(String,Integer)]
type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar (conforme abaixo),
   mas a sua definicao (corpo) pode continuar a mesma dos exercícios anteriores
-}

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}

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
                Right ve1 -> if ve1 /= 0
                                then case execute context stm of 
                                        Right vcontext -> execute vcontext (SWhile exp stm)
                                        Left msg -> Left msg
                                else Right context
                Left msg -> Left msg
        -- Dica: insira aqui o tratamento do STry.
          STry [] _ stmsF -> execute context (SBlock stmsF)
          STry (s:stms) stmsC stmsF -> case execute context s of
                Right vcontext ->execute vcontext (STry stms stmsC stmsF)
                Left msg -> case execute context (SBlock stmsC) of
                        Right vcontext -> execute vcontext (SBlock stmsF)
                        Left msg -> Left msg



{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}

eval :: RContext -> Exp ->  Either ErrorMessage Integer
eval context x = let normal_eval = \f exp0 exp -> case eval context exp0 of 
                              Right ve1 -> case eval context exp of
                                Right ve2 -> Right (f ve1 ve2)
                                Left msg -> Left msg
                              Left msg -> Left msg 
                 in case x of
  EAdd exp0 exp -> normal_eval (+) exp0 exp
  ESub exp0 exp -> normal_eval (-) exp0 exp
  EMul exp0 exp -> normal_eval (*) exp0 exp
  EDiv e1 e2 -> case eval context e1 of
                    Right ve1 -> case eval context e2 of
                      Right ve2 -> if ve2 == 0
                                   then Left "divisao por 0"
                                   else Right (ve1 `div` ve2)
                      Left msg -> Left msg
                    Left msg -> Left msg
  EInt n -> Right n
  EVar id -> Right (lookup context (getStr id))
    

{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of 
                    Right ve1 -> case eval context e2 of 
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: " 
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg  
                    Left msg -> Left msg  
    EInt n  ->  Right n 
-}                


-- Dica: voce nao precisa mudar o codigo a partir daqui

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv