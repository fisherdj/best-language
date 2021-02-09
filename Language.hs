{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import System.Environment

digitInt :: Char -> Maybe Integer
digitInt '0' = Just 0
digitInt '1' = Just 1
digitInt '2' = Just 2
digitInt '3' = Just 3
digitInt '4' = Just 4
digitInt '5' = Just 5
digitInt '6' = Just 6
digitInt '7' = Just 7
digitInt '8' = Just 8
digitInt '9' = Just 9
digitInt _ = Nothing

digitString :: String -> Maybe Integer
digitStringLE [] = Just 0
digitStringLE (x:s) =
  case (digitInt x,digitStringLE s) of
    (Just d,Just n) -> Just (d + n * 10)
    (_,_) -> Nothing

digitString s = digitStringLE (reverse s)

numString "-" = Nothing
numString ('-':s) =
  case digitString s of
    Nothing -> Nothing
    Just n -> Just (-n)
numString s = digitString s

---- ******************* ----
---- *THE MAIN DATATYPE* ----
---- ******************* ----
data LData =
  LNil
  | LTrue
  | LFalse
  | LSym String
  | LStr String
  | LInt Integer
  | LCons LData LData
  | LVau LData LData LData LData LData
  | LPrim (LData -> LData -> LRet)
  | LCont (LData -> LRet)

instance Eq LData where
  LNil == LNil = True
  LTrue == LTrue = True
  LFalse == LFalse = True
  LSym s1 == LSym s2 = s1 == s2
  LStr s1 == LStr s2 = s1 == s2
  LInt n1 == LInt n2 = n1 == n2
  LCons a1 b1 == LCons a2 b2 = a1 == a2 && b1 == b2
  LVau a1 b1 c1 d1 e1 == LVau a2 b2 c2 d2 e2 =
    [a1,b1,c1,d1,e1] == [a2,b2,c2,d2,e2]
  _ == _ = False

instance Show LData where
  show LNil = "()"
  show LTrue = "#t"
  show LFalse = "#f"
  show (LSym s) = s
  show (LStr s) = show s
  show (LInt n) = show n
  show (LCons a b) = "(" ++ show a ++ showcdr b
  show (LVau _ _ _ _ _) = "<combiner>"
  show (LPrim _) = "<primitive>"
  show (LCont _) = "<continuation>"

data LRet = LRRet LData | LREff LData (LData -> LRet)

instance Show LRet where
  show (LRRet d) = "RETURN " ++ show d
  show (LREff eff cont) = "EFFECT " ++ show eff

showcdr (LNil) = ")"
showcdr (LCons a b) = " " ++ show a ++ showcdr b
showcdr x = " . " ++ show x ++ ")"

---- ************ ----
---- *THE PARSER* ----
---- ************ ----
filterComments :: String -> String
filterComments [] = []
filterComments (';':rest) = filterComments (nextLine rest)
filterComments (x:rest) = x:filterComments rest

nextLine :: String -> String
nextLine [] = []
nextLine ('\n':rest) = rest
nextLine (_:rest) = nextLine rest

readForestC :: String -> [LData]
readForestC [] = []
readForestC (x:rest) =
  if isSpace x
  then readForest rest
  else let (exp,rmd) = readExpr (x:rest) in (exp:readForest rmd)

readForest s = readForestC (filterComments s)

readExpr :: String -> (LData,String)
readExpr ('#':rest) = readHash rest
readExpr ('(':rest) = readLList rest
readExpr (')':rest) =
  error "readExpr got spurious closing delimiter"
readExpr ('"':rest) =
  let (s,rmd) = readString rest in (LStr s,rmd)
readExpr ('\'':rest) =
  let (s,rmd) = readExpr rest in (LCons (LSym "quote") (LCons s LNil),rmd)
readExpr (x:rest) =
  if isSpace x
  then readExpr rest
  else readSymNum (x:rest)
readExpr [] = error "readExpr got nothing"

readLList :: String -> (LData,String)
readLList (')':rest) = (LNil,rest)
readLList (x:rest) =
  if isSpace x
  then readLList rest
  else let (exp,rmd) = readExpr (x:rest)
           (rrest,rrmd) = readLList rmd in (LCons exp rrest, rrmd)
readLList [] = error "readLList ended early"

readSymNumContents :: String -> (String,String)
readSymNumContents [] = ([],[])
readSymNumContents ('(':rest) = ([],'(':rest)
readSymNumContents (')':rest) = ([],')':rest)
readSymNumContents ('#':rest) = ([],'#':rest)
readSymNumContents ('"':rest) = ([],'"':rest)
readSymNumContents (x:rest) =
  if isSpace x
  then ([],x:rest)
  else let (str,rmd) = readSymNumContents rest in (x:str,rmd)

readSymNum s =
  let (rd,rmd) = readSymNumContents s in
    case numString rd of
      Nothing -> (LSym rd,rmd)
      Just x -> (LInt x,rmd)

readHash ('t':rest) = (LTrue,rest)
readHash ('f':rest) = (LFalse,rest)

readString ('\\':escaped:rest) =
  let (restStr,rmd) = readString rest in
    case escaped of 'n' -> ('\n':restStr,rmd)
                    '"' -> ('"':restStr,rmd)
                    _ -> error "readString unrecognized escape"
readString ('"':rest) = ([],rest)
readString (x:rest) =
  let (s,rmd) = readString rest in (x:s,rmd)

---- ************************ ----
---- *EVALUATION STARTS HERE* ----
---- ************************ ----
isTrue :: LData -> Bool
isTrue LFalse = False
isTrue _ = True

acons :: LData -> LData -> LData -> LData
acons k v e = LCons (LCons k v) e

aref k (LCons (LCons ik iv) e) =
  if k == ik
  then Just iv
  else aref k e
aref _ _ = Nothing

patMatch :: LData -> LData -> LData -> Maybe LData
patMatch (LSym s) arg env =
  Just (acons (LSym s) arg env)
patMatch (LCons pa pb) (LCons aa ab) env =
  case patMatch pa aa env of
    Just ea -> patMatch pb ab ea
    Nothing -> Nothing
patMatch x y env =
  if x == y then Just env else Nothing

continue :: LRet -> (LData -> LRet) -> LRet
continue (LREff eff bCont) cont =
  LREff eff (\x -> continue (bCont x) cont)
continue (LRRet d) cont = cont d

combine :: LData -> LData -> LData -> LRet
combine comb argl env =
  case comb of
    LPrim f -> f argl env
    LCont f ->
      case argl of
        LCons arg LNil -> continue (eval arg env) f
        _ -> error ("Invalid argument list to continuation: " ++ show argl)
    LVau rarg argpat earg body venv ->
      case patMatch argpat argl (acons earg env (acons rarg comb venv)) of
        Nothing ->
          error ("Invalid argument list:" ++ show argl ++ "\n"
                 ++ "Argument pattern is:" ++ show argpat)
        Just e -> eval body e
    _ -> error ("Invalid combiner " ++ show comb)

eval :: LData -> LData -> LRet
eval (LCons funExp args) env =
  continue (eval funExp env) (\f -> combine f args env)
eval (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> LRRet x
eval x _ = LRRet x

evalIf c t e env =
  continue (eval c env) (\x -> if isTrue x then (eval t env) else (eval e env))
asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Nothing -> Nothing
    Just t -> Just (a:t)
asList x = Nothing

primCombRecordProc :: String -> Int -> ([LData] -> LData -> LRet) -> LData -> LData -> LRet
primCombRecordProc name nArgs f argl env =
  case asList argl of
    Just l ->
      if length l == nArgs
      then f l env
      else error ("Primitive " ++ name
                  ++ " expected " ++ show nArgs
                  ++ " arguments but got " ++ show (length l))
    Nothing -> error ("Primitive " ++ name
                      ++ " did not get a proper argument list: " ++ show argl ++ "\n")

primCombRecord name nArgs f =
  (name, primCombRecordProc name nArgs f)

callArgs :: ([LData] -> LRet) -> [LData] -> LData -> LRet
callArgs f (a:b) env =
  continue (eval a env) (\x -> callArgs (\xs -> f (x:xs)) b env)
callArgs f [] _ = f []

primArgsRecord :: String -> Int -> ([LData] -> LRet) -> (String,LData -> LData -> LRet)
primArgsRecord name nArgs f =
  primCombRecord name nArgs (callArgs f)

primRecord :: String -> Int -> ([LData] -> LData) -> (String,LData -> LData -> LRet)
primRecord name nArgs f =
  primArgsRecord name nArgs (\l -> LRRet (f l))

makePrimEnv :: [(String,LData -> LData -> LRet)] -> LData
makePrimEnv ((name,fun):xs) = acons (LSym name) (LPrim fun) (makePrimEnv xs)
makePrimEnv [] = LNil

initEnv :: LData
initEnv =
  makePrimEnv
    [primRecord "cons" 2 (\[x,y] -> LCons x y)
    ,primRecord "=" 2 (\[x,y] -> if (x == y) then LTrue else LFalse)
    ,primArgsRecord "eval" 2 (\[exp,env] -> eval exp env)
    ,primCombRecord "vau" 3
     (\[argPat,envArg,body] env -> LRRet (LVau LFalse argPat envArg body env))
    ,primCombRecord "vau-rec" 4
     (\[name,argPat,envArg,body] env -> LRRet (LVau name argPat envArg body env))
    ,primCombRecord "if" 3 (\[c,t,e] env -> evalIf c t e env)
    ,primArgsRecord "effect" 1 (\[eff] -> LREff eff LRRet)
    ,primRecord "eval-capture" 2 (\[exp,env] -> capture (eval exp env))
    ,primRecord "pair?" 1 pairp
    ,primRecord "number?" 1 numberp
    ,primRecord "symbol?" 1 symbolp
    ,primRecord "string?" 1 stringp
    ,primRecord "null?" 1 nullp
    ,primRecord "car" 1 (\[LCons a b] -> a)
    ,primRecord "cdr" 1 (\[LCons a b] -> b)
    ,primRecord "+" 2 (\[LInt x,LInt y] -> LInt (x+y))
    ,primRecord "-" 2 (\[LInt x,LInt y] -> LInt (x - y))
    ,primRecord "/" 2 (\[LInt x,LInt y] -> LInt (div x y))
    ,primRecord "%" 2 (\[LInt x,LInt y] -> LInt (rem x y))
    ,primRecord "*" 2 (\[LInt x,LInt y] -> LInt (x*y))
    ]

capture (LRRet r) = LCons (LSym "value") r
capture (LREff eff cont) = LCons (LSym "effect") (LCons eff (LCont cont))

pairp [LCons _ _] = LTrue
pairp _ = LFalse

numberp [LInt _] = LTrue
numberp _ = LFalse

symbolp [LSym _] = LTrue
symbolp _ = LFalse

stringp [LStr _] = LTrue
stringp _ = LFalse

nullp [LNil] = LTrue
nullp _ = LFalse

evalTop :: [LData] -> LData -> LRet
evalTop (a:b) env = (continue (eval a env) (\x -> evalTop b x))
evalTop [] env = LRRet env

mainHandle :: LRet -> IO LData
mainHandle (LRRet r) = return r
mainHandle (LREff (LSym "read") cont) = do {
    ln <- getLine;
    mainHandle (cont (fst (readExpr ln)))
  }
mainHandle (LREff (LCons (LSym "write") (LCons x LNil)) cont) = do {
    print x;
    mainHandle (cont LNil)
  }

mainArgs :: [String] -> LData -> IO ()
mainArgs [] env = return ()
mainArgs ("-e":expString:rest) env = do {
    mainHandle (evalTop (readForest expString) env) >>= print;
    mainArgs rest env
  }
mainArgs ("-f":fileName:rest) env = do {
    file <- readFile fileName;
    result <- mainHandle (evalTop (readForest file) env);
    mainArgs rest result
  }

main :: IO ()
main = do {
    args <- getArgs;
    mainArgs args initEnv
  }
