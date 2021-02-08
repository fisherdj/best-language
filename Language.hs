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
  | LPrim (LData -> LData -> LData)

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

fromList :: [LData] -> LData
fromList (x:xs) = LCons x (fromList xs)
fromList [] = LNil

isTrue :: LData -> Bool
isTrue LFalse = False
isTrue _ = True

fromBool False = LFalse
fromBool True = LTrue

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

combine :: LData -> LData -> LData -> LData
combine f argl env =
  case f of
    LPrim f -> f argl env
    LVau rarg argpat earg body venv ->
      case patMatch argpat argl (acons earg env (acons rarg f venv)) of
        Nothing ->
          error ("Invalid argument list:" ++ show argl ++ "\n"
                 ++ "Argument pattern is:" ++ show argpat)
        Just e -> eval body e
    _ -> error ("Invalid combiner " ++ show f)

eval :: LData -> LData -> LData
eval (LCons funExp args) env =
  combine (eval funExp env) args env
eval (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> x
eval x _ = x

evalArgs :: [LData] -> LData -> [LData]
evalArgs (x:xs) env = eval x env : evalArgs xs env
evalArgs [] env = []

asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Nothing -> Nothing
    Just t -> Just (a:t)

primCombRecordProc :: String -> Int -> ([LData] -> LData -> LData) -> LData -> LData -> LData
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

primCombRecord :: String -> Int -> ([LData] -> LData -> LData) -> (String,LData -> LData -> LData)
primCombRecord name nArgs f =
  (name, primCombRecordProc name nArgs f)

primRecord name nArgs f =
  primCombRecord name nArgs (\a e -> f (evalArgs a e))

makePrimEnv :: [(String,LData -> LData -> LData)] -> LData
makePrimEnv ((name,fun):xs) = acons (LSym name) (LPrim fun) (makePrimEnv xs)
makePrimEnv [] = LNil

initEnv :: LData
initEnv =
  makePrimEnv
    [primRecord "cons" 2 (\[x,y] -> LCons x y)
    ,primRecord "=" 2 (\[x,y] -> fromBool (x == y))
    ,primRecord "eval" 2 (\[exp,env] -> eval exp env)
    ,primCombRecord "vau" 3
     (\[argPat,envArg,body] env -> LVau LFalse argPat envArg body env)
    ,primCombRecord "vau-rec" 4
     (\[name,argPat,envArg,body] env -> LVau name argPat envArg body env)
    ,primRecord "if" 3 (\[c,t,e] -> if (isTrue c) then t else e)
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

evalTop :: [LData] -> LData -> LData
evalTop l env = foldl (\ienv iexp -> eval iexp ienv) env l

mainArgs :: [String] -> LData -> IO ()
mainArgs [] env = return ()
mainArgs ("-e":expString:rest) env = do {
    print (evalTop (readForest expString) env);
    mainArgs rest env;
  }
mainArgs ("-f":fileName:rest) env = do {
    file <- readFile fileName;
    mainArgs rest (evalTop (readForest file) env);
  }

main :: IO ()
main = do {
    args <- getArgs;
    mainArgs args initEnv;
  }
