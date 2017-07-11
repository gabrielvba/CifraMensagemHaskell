main = do putStrLn("Menu:" ++ "\n")
          putStrLn("1 - Cifra Mensagem.")
          putStrLn("2 - Mostra alfabeto.")
          putStrLn("3 - Decifrar Mensagem.")
          putStrLn("4 - Terminar." ++ "\n")
          
          number <- readLn
          menu number

menu :: Int -> IO()
menu 1 = do putStrLn("\n" ++ "Digite a mensagem a ser cifrada:" ++ "\n")
            string <- getLine
            putStrLn("\n" ++ cifra2a2 (colocaFlag(colocaX (retiraEspacos string))) ++ "\n")
            main
            
menu 2 = do putStrLn("\n" ++ "Alfabeto usado:" ++ "\n")
            mostraAlfabeto
            main
            
menu 3 = do putStrLn("\n" ++ "Digite a mensagem a ser decifrada:" ++ "\n")
            string <- getLine
            putStrLn("\n" ++  retirarFalsoX(encontraFalsoX(retiraFlag(colocaEspaco(decifra2a2 string)))) ++ "\n")
            main
            
menu 4 = do putStrLn("\n" ++ "Programa finalizado.")

    


retiraEspacos :: String -> String
retiraEspacos [] = ""
retiraEspacos(s:cs) = 
    if s == ' ' then "w" ++ retiraEspacos(cs)
    else s : retiraEspacos(cs)



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


condicaoDecifraLinha :: Char -> Char -> [[Char]] -> String
condicaoDecifraLinha a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    if(colunaA == 0) then 
        if(colunaB == 0) then (pegaElemento 0 linhaA 4 (s:sc)) ++ (pegaElemento 0 linhaB 4 (s:sc))
        else (pegaElemento 0 linhaA 4 (s:sc)) ++ (pegaElemento 0 linhaB (colunaB-1) (s:sc))
    else 
         if(colunaB == 0) then (pegaElemento 0 linhaA (colunaA-1) (s:sc)) ++ (pegaElemento 0 linhaB 4 (s:sc))
         else (pegaElemento 0 linhaA (colunaA-1) (s:sc)) ++ (pegaElemento 0 linhaB (colunaB-1) (s:sc))




condicaoDecifraColuna :: Char -> Char -> [[Char]] -> String
condicaoDecifraColuna a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    if(linhaA == 0) then 
        if(linhaB == 0) then (pegaElemento 0 4 colunaA (s:sc)) ++ (pegaElemento 0 4 colunaB (s:sc))
        else (pegaElemento 0 4 colunaA (s:sc)) ++ (pegaElemento 0 (linhaB-1) colunaB (s:sc))
    else 
         if(linhaB == 0) then (pegaElemento 0 (linhaA-1) colunaA (s:sc)) ++ (pegaElemento 0 4 colunaB (s:sc))
         else (pegaElemento 0 (linhaA-1) colunaA (s:sc)) ++ (pegaElemento 0 (linhaB-1) colunaB (s:sc))
         
condicaoDecifraRetangulo :: Char -> Char -> [[Char]] -> String
condicaoDecifraRetangulo a b (s:sc) = do
    let linhaA = acharLinha a 0 (s:sc)
    let linhaB = acharLinha b 0 (s:sc)
    let colunaA = acharColuna a 0 (s:sc)
    let colunaB = acharColuna b 0 (s:sc)
    (pegaElemento 0 linhaB colunaA (s:sc)) ++ (pegaElemento 0 linhaA colunaB (s:sc))
      

condicaoDecifragem :: Char -> Char -> [[Char]] -> String
condicaoDecifragem a b matriz = 
    if acharLinha a 0 matriz == acharLinha b 0 matriz then condicaoDecifraLinha a b matriz
    else if acharColuna a 0 matriz == acharColuna b 0 matriz then condicaoDecifraColuna a b matriz
    else condicaoDecifraRetangulo a b matriz


decifra2a2 :: String -> String
decifra2a2 [] = []
decifra2a2(s:sc) = do
      let matriz = [['a','b','c','d','e'], ['f','g','h','i','j'], ['k','l','m','n','o'], ['p','q','r','s','t'], ['u','v','w','x','z']]
      (condicaoDecifragem s (head sc) matriz) ++ decifra2a2 (tail sc)
      
      
mostraAlfabeto :: IO()
mostraAlfabeto = do
                   let matriz = [['a','b','c','d','e'], ['f','g','h','i','j'], ['k','l','m','n','o'], ['p','q','r','s','t'], ['u','v','w','x','z']]
                   imprimeLinha matriz



imprimeLinha :: [[Char]] -> IO()
imprimeLinha [] = putStrLn("\n")
imprimeLinha (s:sc) = do putStrLn(s)
                         imprimeLinha sc


colocaEspaco :: String -> String
colocaEspaco [] = ""
colocaEspaco(s:cs) = 
    if s == 'w' then " " ++ colocaEspaco(cs)
    else s : colocaEspaco(cs)
    
    
retiraFlag :: String -> String
retiraFlag [] = ""
retiraFlag(s:cs) = 
    if s == 'k' then retiraFlag(cs)
    else s : retiraFlag(cs)


encontraFalsoX :: String -> String
encontraFalsoX (s:sc) = 
    if sc == [] then [s]
    else if length sc == 1 then [s] ++ sc
    else 
        if s == (head (tail sc)) && (head sc) == 'x' then s : "y" ++ encontraFalsoX(tail sc)
        else s : encontraFalsoX(sc)

retirarFalsoX :: String -> String
retirarFalsoX [] = ""
retirarFalsoX(s:cs) = 
    if s == 'y' then retirarFalsoX(cs)
    else s : retirarFalsoX(cs)


