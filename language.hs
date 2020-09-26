Data LData = lpair LData LData | lnil | lsym String | lstr String | ltrue | lfalse

readExpr :: String -> (LData,String)
readExpr ('#':rest) = readHash ('#':rest)
readExpr ('(':rest) = readList ('(':rest)
readExpr (x:rest) = if isSpace x then readExpr rest else readSymNum (x:rest)
readExpr [] = []

readHash ('#':'t':rest) = (ltrue,rest)
