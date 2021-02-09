{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
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
  | LPrim ((LData -> LRet) -> LData -> LData -> LRet)
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

data LRet = LRRet LData | LREff LData LCont
type LCont = LData -> LRet

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
readHash [] = error "readHash got nothing"
readHash _ = error "readHash got invalid"

readString ('\\':escaped:rest) =
  let (restStr,rmd) = readString rest in
    case escaped of 'n' -> ('\n':restStr,rmd)
                    '"' -> ('"':restStr,rmd)
                    _ -> error "readString unrecognized escape"
readString ('"':rest) = ([],rest)
readString (x:rest) =
  let (s,rmd) = readString rest in (x:s,rmd)
readString [] =
  error "readString got nothing"

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

composeCont :: LCont -> LCont -> LCont
composeCont k1 k2 ret =
  case k2 ret of
    LRRet x -> k1 x
    LREff eff k3 -> LREff eff (composeCont k1 k3)

primError :: LCont -> String -> LRet
primError k s = LREff (LCons (LSym "primitive-error") (LStr s)) k

combine :: LCont -> LData -> LData -> LData -> LRet
combine k comb argl env =
  case comb of
    LPrim f -> f k argl env
    LCont f ->
      case argl of
        LCons arg LNil -> eval (composeCont k f) arg env
        _ -> primError k ("Invalid argument list to continuation: " ++ show argl)
    LVau rarg argpat earg body venv ->
      case patMatch argpat argl (acons earg env (acons rarg comb venv)) of
        Nothing ->
          primError k ("Invalid argument list:" ++ show argl ++ "\n"
                       ++ "Argument pattern is:" ++ show argpat)
        Just e -> eval k body e
    _ -> primError k ("Invalid combiner " ++ show comb)

eval :: LCont -> LData -> LData -> LRet
eval k (LCons funExp args) env =
  eval (\f -> combine k f args env) funExp env
eval k (LSym s) env =
  case aref (LSym s) env of
    Nothing -> primError k ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> k x
eval k x _ = k x

asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Nothing -> Nothing
    Just t -> Just (a:t)
asList x = Nothing

primCombRecordProc :: String -> Int -> (LCont -> [LData] -> LData -> LRet) -> LCont -> LData -> LData -> LRet
primCombRecordProc name nArgs f k argl env =
  case asList argl of
    Just l ->
      if length l == nArgs
      then f k l env
      else primError k ("Primitive " ++ name
                        ++ " expected " ++ show nArgs
                        ++ " arguments but got " ++ show (length l))
    Nothing -> primError k ("Primitive " ++ name
                            ++ " did not get a proper argument list: " ++ show argl ++ "\n")

primRecord :: String -> Int -> (LCont -> [LData] -> LData -> LRet) -> (String,LCont -> LData -> LData -> LRet)
primRecord name nArgs f =
  (name, primCombRecordProc name nArgs f)

makePrimEnv :: [(String,LCont -> LData -> LData -> LRet)] -> LData
makePrimEnv ((name,fun):xs) = acons (LSym name) (LPrim fun) (makePrimEnv xs)
makePrimEnv [] = LNil

initEnv :: LData
initEnv =
  makePrimEnv
    [primRecord "cons" 2 (primBinaryFun LCons)
    ,primRecord "=" 2 (primBinaryFun (\x y -> if x == y then LTrue else LFalse))
    ,primRecord "eval" 2 (primBinary eval)
    ,primRecord "vau" 3 vauprim
    ,primRecord "vau-rec" 4 vaurecprim
    ,primRecord "if" 3 ifprim
    ,primRecord "effect" 1 (primUnary (\k x -> LREff x k))
    ,primRecord "eval-capture" 2 (primBinaryFun evalCapture)
    ,primRecord "pair?" 1 (primUnaryFun pairp)
    ,primRecord "number?" 1 (primUnaryFun numberp)
    ,primRecord "symbol?" 1 (primUnaryFun symbolp)
    ,primRecord "string?" 1 (primUnaryFun stringp)
    ,primRecord "null?" 1 (primUnaryFun nullp)
    ,primRecord "car" 1 (primUnary carprim)
    ,primRecord "cdr" 1 (primUnary cdrprim)
    ,primRecord "+" 2 (primBinaryNum "+" (+))
    ,primRecord "-" 2 (primBinaryNum "-" (-))
    ,primRecord "/" 2 (primBinaryNum "/" div)
    ,primRecord "%" 2 (primBinaryNum "%" mod)
    ,primRecord "*" 2 (primBinaryNum "*" (*))
    ]

impError k name = primError k ("Impossible error: " ++ name)

primUnary f k [a] e = eval (f k) a e
primUnary _ k _ _ = impError k "unary"

primBinary f k [a,b] e = eval (\x -> eval (\y -> f k x y) b e) a e
primBinary _ k _ _ = impError k "binary"

primUnaryFun :: (LData -> LData) -> LCont -> [LData] -> LData -> LRet
primUnaryFun f = primUnary (\k x -> k (f x))

primBinaryFun :: (LData -> LData -> LData) -> LCont -> [LData] -> LData -> LRet
primBinaryFun f = primBinary (\k x y -> k (f x y))

primBinaryNumProc name f k (LInt x) (LInt y) = k (LInt (f x y))
primBinaryNumProc name f k x y =
  primError k ("Numeric primitive " ++ name
               ++ " got non-numeric arguments " ++ show [x,y])

primBinaryNum name f = primBinary (primBinaryNumProc name f)

vauprim k [argPat,envArg,body] env =
  k (LVau LFalse argPat envArg body env)
vauprim k _ _ = impError k "vau"

vaurecprim k [name,argPat,envArg,body] env =
  k (LVau name argPat envArg body env)
vaurecprim k _ _ = impError k "vau-rec"

ifprim k [c,t,e] env =
  eval (\x -> if isTrue x then eval k t env else eval k e env) c env
ifprim k _ _ = impError k "if"

evalCapture exp env =
  case eval LRRet exp env of
    LRRet r -> LCons (LSym "value") r
    LREff eff cont -> LCons (LSym "effect") (LCons eff (LCont cont))

pairp (LCons _ _) = LTrue
pairp _ = LFalse

numberp (LInt _) = LTrue
numberp _ = LFalse

symbolp (LSym _) = LTrue
symbolp _ = LFalse

stringp (LStr _) = LTrue
stringp _ = LFalse

nullp LNil = LTrue
nullp _ = LFalse

carprim k (LCons a _) = k a
carprim k x = primError k ("Non-pair argument to car: " ++ show x)

cdrprim k (LCons _ b) = k b
cdrprim k x = primError k ("Non-pair argument to cdr: " ++ show x)

evalTop :: [LData] -> LData -> LRet
evalTop (a:b) env = eval (\x -> evalTop b x) a env
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
mainHandle (LREff eff cont) =
  error ("Unrecognized effect: " ++ show eff)

mainArgs :: LData -> [String] -> IO ()
mainArgs env [] = return ()
mainArgs env ("-e":expString:rest) = do {
    mainHandle (evalTop (readForest expString) env) >>= print;
    mainArgs env rest
  }
mainArgs env ("-f":fileName:rest) = do {
    file <- readFile fileName;
    result <- mainHandle (evalTop (readForest file) env);
    mainArgs result rest
  }
mainArgs x _ = error "Invalid arguments"

main :: IO ()
main = getArgs >>= mainArgs initEnv
