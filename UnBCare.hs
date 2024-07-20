module UnBCare where

import ModeloDados

{-
 *** Aluno: Bruno Barros Xavier
 *** Matricula: 200015621
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

                                
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med1 quant1 [] = [(med1, quant1)]
comprarMedicamento med1 quant1 ((med2, quant2):meds_stock)
        | med1 > med2 = (med2, quant2):(comprarMedicamento med1 quant1 meds_stock)
        | med1 == med2 = (med1, quant1+quant2):meds_stock
        | otherwise = (med1, quant1):(med2, quant2):meds_stock

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med1 [] = Nothing
tomarMedicamento med1 ((med2, quant2):meds_stock) 
        | med1 == med2 = Just ((med2, quant2-1):meds_stock) 
        | otherwise = tomarMedicamento med1 meds_stock

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med1 [] = 0
consultarMedicamento med1 ((med2, quant2):meds_stock) 
        | med1 == med2 = quant2
        | otherwise = consultarMedicamento med1 meds_stock

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med1, horarios):meds) = (med1, length horarios):(demandaMedicamentos meds)

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
checkListOrd :: (Ord a) => [a] -> Bool
checkListOrd [] = True
checkListOrd [_] = True
checkListOrd (value1:value2:values) = (value1 < value2) && (checkListOrd (value2:values))

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido [(_, horarios)] = checkListOrd horarios
receituarioValido ((med1, horarios1):(med2, horarios2):receituarios) = (med1 < med2) && (checkListOrd horarios1) && (receituarioValido ((med2, horarios2):receituarios))

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido [(_, meds)] = checkListOrd meds
planoValido ((horario1, meds1):(horario2, meds2):planos) = (horario1 < horario2) && (checkListOrd meds1) && (planoValido ((horario2, meds2):planos))

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

cuidadosValido :: [Cuidado] -> Bool
cuidadosValido [] = True
cuidadosValido [_] = True
cuidadosValido ((Medicar med1):(Medicar med2):xs)             = (med1 < med2) && (cuidadosValido ((Medicar med2):xs))
cuidadosValido ((Medicar med1):(Comprar med2 quat2):xs)       = (med1 /= med2)  && (cuidadosValido ((Medicar med1):xs)) && (cuidadosValido ((Comprar med2 quat2):xs))
cuidadosValido ((Comprar med1 quat1):(Comprar med2 quat2):xs) = (cuidadosValido ((Comprar med1 quat1):xs)) && (cuidadosValido ((Comprar med2 quat2):xs))
cuidadosValido ((Comprar med2 quat2):(Medicar med1):xs)       = (med1 /= med2)  && (cuidadosValido ((Medicar med1):xs)) && (cuidadosValido ((Comprar med2 quat2):xs))


plantaoValido :: Plantao -> Bool
plantaoValido [] = True
plantaoValido [(_, cuidados)] = cuidadosValido cuidados
plantaoValido ((horario1, cuidados1):(horario2, cuidados2):planos) = (horario1 < horario2) && (cuidadosValido cuidados1) && (plantaoValido ((horario2, cuidados2):planos))

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}


insertMedSorted :: Medicamento -> [Medicamento] -> [Medicamento]
insertMedSorted med1 [] = [med1]
insertMedSorted med1 (med2:meds)
                        | med1 <= med2 = med1:med2:meds
                        | otherwise = med2:(insertMedSorted med1 meds)

insertPlanoMedicamento :: Horario -> Medicamento -> PlanoMedicamento -> PlanoMedicamento
insertPlanoMedicamento h  med [] = [(h, [med])]
insertPlanoMedicamento h1 med1 ((h2, med2:meds):ps) 
                                | h1 > h2 = (h2, med2:meds):(insertPlanoMedicamento h1 med1 ps)
                                | h1 == h2 = (h2, insertMedSorted med1 (med2:meds)):ps 
                                | otherwise = (h1, [med1]):(h2, med2:meds):ps 
                                

geraPlanoReceituarioAcc :: Receituario -> PlanoMedicamento -> PlanoMedicamento
geraPlanoReceituarioAcc [] acc = acc
geraPlanoReceituarioAcc ((med, []):rs) acc = geraPlanoReceituarioAcc rs acc
geraPlanoReceituarioAcc ((med, h:hs):rs) acc = geraPlanoReceituarioAcc ((med, hs):rs) (insertPlanoMedicamento h med acc)


geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario rec = geraPlanoReceituarioAcc rec [] 


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

insertHorarioSorted :: Horario -> [Horario] -> [Horario]
insertHorarioSorted h1 [] = [h1]
insertHorarioSorted h1 (h2:hs)
                        | h1 <= h2 = h1:h2:hs
                        | otherwise = h2:(insertHorarioSorted h1 hs)

insertReceituario :: Medicamento -> Horario ->  Receituario -> Receituario
insertReceituario med1 h1 [] = [(med1, [h1])]
insertReceituario med1 h1 ((med2, h2:hs):rs) 
                                | med1 > med2 = (med2, h2:hs):(insertReceituario med1 h1 rs)
                                | med1 == med2 = (med2, insertHorarioSorted h1 (h2:hs)):rs 
                                | otherwise = (med1, [h1]):(med2, h2:hs):rs 
                                

geraReceituarioPlanoAcc ::  PlanoMedicamento -> Receituario -> Receituario
geraReceituarioPlanoAcc [] acc = acc
geraReceituarioPlanoAcc ((h, []):ps) acc = geraReceituarioPlanoAcc ps acc
geraReceituarioPlanoAcc ((h, med:meds):ps) acc = geraReceituarioPlanoAcc ((h, meds):ps) (insertReceituario med h acc)


geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = geraReceituarioPlanoAcc plano [] 


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

updateEstoque :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
updateEstoque (Medicar med1) [] = Nothing
updateEstoque (Comprar med1 quat1) [] = Just [(med1, quat1)]
updateEstoque (Medicar med1) ((med2, quant2):meds)
                        | (med1 == med2)&&(quant2>0) = Just ((med2, quant2-1):meds) 
                        | otherwise = case (updateEstoque (Medicar med1) meds) of
                                        Nothing -> Nothing
                                        Just estoque -> Just ((med2, quant2):estoque)
updateEstoque (Comprar med1 quat1) estoque = Just (comprarMedicamento med1 quat1 estoque)

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((h1, []):ps) estoque = executaPlantao ps estoque
executaPlantao ((h1, c1:cs):ps) estoque = case (updateEstoque c1 estoque) of
                                                Nothing -> Nothing
                                                Just estoque -> executaPlantao ((h1, cs):ps) estoque

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

checkPlantaoPlano :: Plantao -> PlanoMedicamento -> Bool
checkPlantaoPlano [] [] = True
checkPlantaoPlano [] _ = False
checkPlantaoPlano _  [] = False
checkPlantaoPlano ((h1, []):xs) ((h2, []):ys) = True
checkPlantaoPlano ((h1, _):xs) ((h2, []):ys) = False
checkPlantaoPlano ((h1, []):xs) ((h2, _):ys) = False
checkPlantaoPlano ((h1, c1:cs):xs) ((h2, med2:meds):ys) = case c1 of
                                                        Medicar med1 -> (med1 == med2)&&(checkPlantaoPlano ((h1, cs):xs) ((h2, meds):ys))
                                                        Comprar med1 quat1 -> checkPlantaoPlano ((h1, cs):xs) ((h2, med2:meds):ys)

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = ((executaPlantao plantao estoque) /= Nothing) -- && (checkPlantaoPlano plantao plano)

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

-- type PlanoMedicamento = [(Horario, [Medicamento])]
-- type EstoqueMedicamentos = [(Medicamento, Quantidade)]
-- type Plantao = [(Horario, [Cuidado])]


gerarCompra :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
gerarCompra med1 [] = [(med1, -1)]
gerarCompra med1 ((med2, quant2):meds_stock)
                    | med1 == med2 = ((med2, quant2-1):meds_stock)
                    | otherwise = (med2, quant2):(gerarCompra med1 meds_stock)

gerarCompras :: PlanoMedicamento -> EstoqueMedicamentos -> [Cuidado]
gerarCompras [] [] = []
gerarCompras [] ((m, q):meds)
                | q<0 = (Comprar m (-q)):(gerarCompras [] meds)
                | otherwise  = (gerarCompras [] meds)
gerarCompras ((h, []):pls) estoque = gerarCompras pls estoque
gerarCompras ((h, med1:meds):pls) estoque = gerarCompras ((h, meds):pls) (gerarCompra med1 estoque)
            
gerarMedicamentos :: PlanoMedicamento -> Plantao
gerarMedicamentos [] = []
gerarMedicamentos ((h, meds):pms) = (h, (map (\med->(Medicar med)) meds)):(gerarMedicamentos pms)

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto planomed estoque = (0, (gerarCompras planomed estoque)):(gerarMedicamentos planomed)


