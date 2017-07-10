main = do string <- getLine
          putStrLn(cifra2a2 (colocaFlag(colocaX (tiraEspacos string))))


tiraEspacos :: String -> String
tiraEspacos [] = ""
tiraEspacos(s:cs) = 
    if s == ' ' then "w" ++ tiraEspacos(cs)
    else s : tiraEspacos(cs)



colocaX :: String -> String
colocaX(s: cs) = 
    if  length cs == 0 then [s]
    else if s == head cs  then s : "x" ++ colocaX(cs)
    else s : colocaX(cs)
    


colocaFlag :: String -> String
colocaFlag mensagem = 
    if (length mensagem `mod` 2) == 0 then mensagem
    else mensagem ++ "k"
    


printaBool :: String -> IO()
printaBool(s: cs) = 
    if  length cs == 0 then print(length cs)
    else if s == head cs  then do
      putStrLn(s : "x" ++ cs)
      printaBool cs
    else do
      print((length (s:cs) `mod` 2) == 0)
      printaBool cs



cifra2a2 :: String -> String
cifra2a2 [] = []
cifra2a2(s:sc) = do
      let matriz = [['a','b','c','d','e'], ['f','g','h','i','j'], ['k','l','m','n','o'], ['p','q','r','s','t'], ['u','v','w','x','z']]
      (condicaoCifragem s (head sc) matriz) ++ cifra2a2 (tail sc)



condicaoCifragem :: Char -> Char -> [[Char]] -> String
condicaoCifragem a b matriz = 
    if acharLinha a 0 matriz == acharLinha b 0 matriz then condicaoLinha a b matriz
    else if acharColuna a 0 matriz == acharColuna b 0 matriz then condicaoColuna a b matriz
    else condicaoRetangulo a b matriz



acharLinha :: Char -> Int -> [[Char]] -> Int
acharLinha letra indice [[]] = indice
acharLinha letra indice (s:sc) =
    if (letraInArray letra 0 s < 5) then indice
    else acharLinha letra (indice+1) sc



letraInArray :: Char -> Int -> [Char] -> Int
letraInArray letra indice [] = indice
letraInArray letra indice (s:sc) =
     if letra == s then indice
     else letraInArray letra (indice+1) sc


acharColuna :: Char -> Int -> [[Char]] -> Int
acharColuna letra indice [[]] = indice
acharColuna letra indice (s:sc) =
    if (letraInArray letra 0 s < 5) then letraInArray letra 0 s
    else acharColuna letra (indice+1) sc



condicaoLinha :: Char -> Char -> [[Char]] -> String
condicaoLinha a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    if(colunaA == 4) then 
        if(colunaB == 4) then (pegaElemento 0 linhaA 0 (s:sc)) ++ (pegaElemento 0 linhaB 0 (s:sc))
        else (pegaElemento 0 linhaA 0 (s:sc)) ++ (pegaElemento 0 linhaB (colunaB+1) (s:sc))
    else 
         if(colunaB == 4) then (pegaElemento 0 linhaA (colunaA+1) (s:sc)) ++ (pegaElemento 0 linhaB 0 (s:sc))
         else (pegaElemento 0 linhaA (colunaA+1) (s:sc)) ++ (pegaElemento 0 linhaB (colunaB+1) (s:sc))




condicaoColuna :: Char -> Char -> [[Char]] -> String
condicaoColuna a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    if(linhaA == 4) then 
        if(linhaB == 4) then (pegaElemento 0 0 colunaA (s:sc)) ++ (pegaElemento 0 0 colunaB (s:sc))
        else (pegaElemento 0 0 colunaA (s:sc)) ++ (pegaElemento 0 (linhaB+1) colunaB (s:sc))
    else 
         if(linhaB == 4) then (pegaElemento 0 (linhaA+1) colunaA (s:sc)) ++ (pegaElemento 0 0 colunaB (s:sc))
         else (pegaElemento 0 (linhaA+1) colunaA (s:sc)) ++ (pegaElemento 0 (linhaB+1) colunaB (s:sc))
         
condicaoRetangulo :: Char -> Char -> [[Char]] -> String
condicaoRetangulo a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    (pegaElemento 0 linhaB colunaA (s:sc)) ++ (pegaElemento 0 linhaA colunaB (s:sc))





pegaElemento :: Int -> Int -> Int-> [[Char]] -> String
pegaElemento indice linha coluna [[]] = ""
pegaElemento indice linha coluna (s:sc) =
    if (indice == linha) then pegaElementoAux 0 linha coluna s
    else pegaElemento (indice+1) linha coluna sc
    
    
pegaElementoAux :: Int -> Int -> Int-> [Char] -> String
pegaElementoAux indice linha coluna [] = ""
pegaElementoAux indice linha coluna (s:sc) =
    if (indice == coluna) then [s]
    else pegaElementoAux (indice+1) linha coluna sc
