main = do string <- getLine
          putStrLn(tiraEspacos string)
          printaBool string
          putStrLn(colocaFlag(colocaX (tiraEspacos string)))


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
