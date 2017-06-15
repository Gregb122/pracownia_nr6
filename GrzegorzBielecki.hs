-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module GrzegorzBielecki (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck fs vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.


--typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
--typecheck = undefined

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck f vars expr = case (checkfun f f) of
    TError q str -> Error q str
    _            -> case (typecheck' (toEnv f vars) expr) of
        T TInt         -> Ok
        TError q str   -> Error q str
        _              -> Error (getData expr) "Zwracana wartosc musi byc typu int"
    where
      toEnv :: [FunctionDef p] -> [Var] -> [(Var, TypeNew p)]
      toEnv f vars =
          ([(s, T TInt) | s <- vars]
          ++
          [(funcName s, T (TArrow (funcArgType s) (funcResType s))) | s <- f])

----------------------------FUNKCJEPOMOCNICZE-----------------------------------

checkfun ::[FunctionDef p] -> [FunctionDef p] -> TypeNew p
checkfun _ [] = T TInt
checkfun f (h:rest) =
  case typecheck' [(funcArg h,T (funcArgType h))] (funcBody h) of
      TError q str               -> TError q str
      T x | x==(funcResType h)   -> checkfun f rest
      _                          -> TError (getData (funcBody h)) "Nieodpowiedni typ zwracanej wartosci"

----------------------------------------------------------------------

findVarType :: Expr p -> [(Var, TypeNew p)] -> TypeNew p
findVarType (EVar p _) [] = TError p ""
findVarType (EVar p var) ((var1,t):env) | var==var1 = t
findVarType (EVar p var) (h:env) = findVarType (EVar p var) env

----------------------------------------------------------------------

comparetype :: TypeNew p -> TypeNew p -> Bool
comparetype typeL typeR = case (typeL, typeR) of
  (T TBool, T TBool)                                  -> True
  (T TInt, T TInt)                                    -> True
  (T TUnit, T TUnit)                                  -> True
  (T (TPair a a1), T (TPair b b1)) | a==b && a1==b1   -> True
  (T (TList a), T (TList b)) | a==b                   -> True
  _                                                   -> False

----------------------------------------------------------------------


typecheck' :: [(Var, TypeNew p)] -> Expr p -> TypeNew p
typecheck' env (EVar p var) = case (findVarType (EVar p var) env) of
  T tp -> T tp
  _       -> TError p "Zmienna nie zostala zdefiniowana"

typecheck' env (ENum p _)  = T TInt

typecheck' env (EBool p _) = T TBool

typecheck' env (EUnary p op expr) = case op of
  UNot -> case type1 of
    T TBool        -> T TBool
    TError q str   -> TError q str
    _              -> TError p "Typ bool jest oczekiwany"
  UNeg -> case type1 of
    T TInt         -> T TInt
    TError q str   -> TError q str
    _              -> TError p "Typ int jest oczekiwany"
  where
    type1 = (typecheck' env expr)

typecheck' env (EBinary p op exprL exprR)
  | comparetype typeL typeR = case op of
    BAnd ->  case typeL of
      T TBool        -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ bool jest oczekiwany"
    BOr  ->  case typeL of
      T TBool        -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ bool jest oczekiwany"
    BEq  ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    BNeq ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    BLt  ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    BGt  ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    BLe  ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    BGe  ->  case typeL of
      T TInt         -> T TBool
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
    _    -> case typeL of
      T TInt         -> T TInt
      TError q str   -> TError q str
      _              -> TError  p "Typ int jest oczekiwany"
  | otherwise = case (typeL, typeR) of
    (TError q str, _) -> TError q str
    (_, TError q str) -> TError q str
    _                 -> TError p "typy sa niezgodne"
  where
    typeL = typecheck' env exprL
    typeR = typecheck' env exprR

typecheck' env (ELet p var expr1 expr2) =
  case type0 of
      T tp         -> case typecheck' ((var,T tp):env) expr2 of
          T tp1          -> T tp1
          TError q str   -> TError q str
      TError q str -> TError q str
  where
    type0 = typecheck' env expr1


typecheck' env (EIf p expr0 expr1 expr2)
  | (comparetype type1 type2) && (comparetype type0 (T TBool)) = case type1 of
      T tp               -> T tp
      TError q str       -> TError q str
  | otherwise = case (type0 , type1, type2) of
    (TError q str, _, _) -> TError q str
    (_, TError q str, _) -> TError q str
    (_, _, TError q str) -> TError q str
    (T TBool, _, _)      -> TError p "typy sa niezgodne"
    _                    -> TError p "Typ bool jest oczekiwany"
  where
    type0 = typecheck' env expr0
    type1 = typecheck' env expr1
    type2 = typecheck' env expr2

-------------------------------------------------------------------------------

typecheck' env (EFn p var type0 expr1) = case typecheck' ((var,T type0):env) expr1 of
  T tp1        -> T (TArrow type0 tp1)
  TError q str -> TError q str

-------------------------------------------------------------------------------

typecheck' env (EApp p expr0 expr1) = case type0 of
  T (TArrow tp0 tp1) -> case type1 of
      T tpN0 | tp0==tpN0 -> T tp1
      TError q str       -> TError q str
      _                  -> TError p "Typ argumentu jest inny od typu przyjmowanego przez funkcje"
  TError q str       -> TError q str
  _                  -> TError p "Oczekiwany typ funkcji"
  where
    type0 = typecheck' env expr0
    type1 = typecheck' env expr1

-------------------------------------------------------------------------------

typecheck' env (EUnit p) = T TUnit

-------------------------------------------------------------------------------

typecheck' env (EPair p expr0 expr1) = case (type0,type1) of
  (TError q str,_)            -> TError q str
  (_,TError q str)            -> TError q str
  _                           -> let T x = type0 in
                                   let T y = type1 in
                                     T (TPair x y)
  where
    type0 = typecheck' env expr0
    type1 = typecheck' env expr1

typecheck' env (EFst p expr0) =  case type0 of
  T (TPair tp0 tp1)           -> T tp0
  TError q str                -> TError q str
  _                           -> TError p "Oczekiwana para"
  where
    type0 = typecheck' env expr0

typecheck' env (ESnd p expr0) =  case type0 of
  T (TPair tp0 tp1)           -> T tp1
  TError q str                -> TError q str
  _                           -> TError p "Oczekiwana para"
  where
    type0 = typecheck' env expr0

typecheck' env (ENil p (TList type0)) = T (TList type0)
typecheck' env (ENil p _)             = TError p "niepoprawny typ listy"


typecheck' env (ECons p expr0 expr1) = case (type0,type1) of
  (T a, T (TList b)) | a==b   -> T (TList a)
  (TError q str,_)            -> TError q str
  (_,TError q str)            -> TError q str
  _                           -> TError p "typ glowy jest rozny od typu listy"
  where
    type0 = typecheck' env expr0
    type1 = typecheck' env expr1

typecheck' env (EMatchL p expr0 nil (var0, var1, expr1)) = case type0 of
  T (TList a)           -> case typeNil of
      TError q str      -> TError q str
      T tp1             -> case typecheck'  ((var1,T (TList a)):(var0,T a):env) expr1 of
          TError q str            -> TError q str
          T tp2 | tp1==tp2        -> T tp2
          _                       -> TError p "Match sie nietypuje"
  TError q str          -> TError q str
  _                     -> TError p "typ list oczekiwany"
  where
    type0    = typecheck' env expr0
    typeNil  = typecheck' env nil

------------------------------------------------------------------------










------------------------------------------------------------------------
-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval fs input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że definicje funckcji fs oraz wyrażenie e są dobrze
-- typowane, tzn. typecheck fs (map fst input) e = Ok
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval fun env expr = case (myeval (conv env fun) expr) of
  Val (NInt value) -> Value value
  _                -> RuntimeError

--FUNKCJE POMOCNICZE---------------------------------------------------

findVal :: Var -> [(Var, TypeEval p)] -> Eval p
findVal _ [] = NError
findVal var ((var1,val):env) | var==var1 = Val val
findVal var (_:env) = findVal var env

----------------------------------------------------------------------

conv :: [(Var, Integer)] -> [FunctionDef p] -> [(Var, TypeEval p)]
conv vars defs = ([(fst v, (NInt (snd v))) | v <- vars]
                 ++
                 [(funcName f, NClosG defs f) | f <- defs])
----------------------------------------------------------------------

----------------------------------------------------------------------

myeval :: [(Var,TypeEval p)] -> Expr p -> Eval p
myeval env (EVar p val)       = case findVal val env of
   Val tpv          -> Val tpv
   _                -> NError
myeval env (ENum p val)       = Val (NInt val)
myeval env (EBool p val)      = Val (NBool val)
myeval env (EUnary p op val)  = case op of
  UNot -> case myeval env val of
    Val (NBool bool)            -> Val (NBool (not bool))
    _                           -> NError
  UNeg -> case myeval env val of
    Val (NInt int)              -> Val (NInt (- int))
    _                           -> NError

myeval env (EBinary p op val val1) = case op of
  BAnd -> case (type0, type1) of
    (Val (NBool bool), Val (NBool bool1)) -> Val (NBool (bool && bool1))
    _                                     -> NError
  BOr  -> case (type0, type1) of
    (Val (NBool bool), Val (NBool bool1)) -> Val (NBool (bool || bool1))
    _                                     -> NError
  BEq  -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int == int1))
    _                                     -> NError
  BNeq -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int /= int1))
    _                                     -> NError
  BLt  -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int < int1))
    _                                     -> NError
  BGt  -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int > int1))
    _                                     -> NError
  BLe  -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int <= int1))
    _                                     -> NError
  BGe  -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NBool (int >= int1))
    _                                     -> NError
  BAdd -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NInt (int + int1))
    _                                     -> NError
  BSub -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NInt (int - int1))
    _                                     -> NError
  BMul -> case (type0, type1) of
    (Val (NInt  int), Val (NInt  int1))   -> Val (NInt (int * int1))
    _                                     -> NError
  BDiv -> case (type0, type1) of
    (_, Val (NInt  0))                    -> NError
    (Val (NInt  int), Val (NInt  int1))   -> Val (NInt (div int int1))
    _                                     -> NError
  BMod -> case (type0, type1) of
    (_, Val (NInt  0))                    -> NError
    (Val (NInt  int), Val (NInt  int1))   -> Val (NInt (mod int int1))
    _                                     -> NError
  where
    type0 = myeval env val
    type1 = myeval env val1

myeval env (ELet p var val val1) = case type0 of
  Val tp         -> case myeval ((var,tp):env) val1 of
    Val tpv        -> Val tpv
    _              -> NError
  _              -> NError
  where
    type0 = myeval env val

myeval env (EIf p con val val1) = case myeval env con of
  Val (NBool True)  ->  case myeval env val of
    Val tp           -> Val tp
    _                -> NError
  Val (NBool False) ->  case myeval env val1 of
    Val tp           -> Val tp
    _                -> NError
  _                 -> NError

------------------------------------------------------------------------------
myeval env (EFn p var _ expr) = Val (NClos env var expr)

myeval env (EApp p expr0 expr1) = case clos of
  Val (NClos envN var exprN)   -> case arg of
    Val v                        -> myeval ((var,v):envN) exprN
    NError                       -> NError
  Val (NClosG  defs def)       -> case arg of
    Val v                        -> myeval ((funcArg def,v):(toEnv defs defs)) (funcBody def)
    NError                       -> NError
  where
    clos = myeval env expr0
    arg  = myeval env expr1
    toEnv [] _ = []
    toEnv (def:defs) allDefs = ((funcName def, NClosG allDefs def):(toEnv defs allDefs))

myeval env (EUnit p) = Val NUnit

myeval env (EPair p expr0 expr1) = case (res0, res1) of
  (Val a, Val b) -> Val (NPair a b)
  _              -> NError
  where
    res0 = myeval env expr0
    res1 = myeval env expr1

myeval env (EFst p expr) = case  myeval env expr of
  Val (NPair a b) -> Val a
  _               -> NError

myeval env (ESnd p expr) = case  myeval env expr of
  Val (NPair a b) -> Val b
  _               -> NError

myeval env (ENil p _) = Val (NList [])

myeval env (ECons p expr0 expr1) = case (res0, res1) of
  (Val a, Val (NList b)) -> Val (NList (a:b))
  _                      -> NError
  where
    res0 = myeval env expr0
    res1 = myeval env expr1

myeval env (EMatchL p expr0 nil (var0, var1, expr1)) = case res0 of
  Val (NList [])      -> case resNil of
    Val a               -> Val a
    _                   -> NError
  Val (NList (h:res)) -> case myeval ((var1,NList res):(var0, h):env) expr1 of
    Val a               -> Val a
    _                   -> NError
  _                   -> NError
  where
    res0   = myeval env expr0
    resNil = myeval env nil


--TYPY------------------------------------------------------------------------

data TypeNew p =
    T Type
  | TError p String
  deriving (Eq)

------------------------------------------------------------------------------

data TypeEval p =
    NInt Integer
  | NBool Bool
  | NUnit
  | NPair (TypeEval p) (TypeEval p)
  | NList [TypeEval p]
  | NClos [(Var,TypeEval p)] Var (Expr p)
  | NClosG [FunctionDef p] (FunctionDef p)
  deriving (Eq)

data Eval p =
    Val (TypeEval p)
  | NError
  deriving (Eq)
