-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module GrzegorzBieleckiTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [Test "dzialania1" (SrcString "1 + 2") (Eval [] (Value 3))
  , Test "dzialania2" (SrcString "4 - 1") (Eval [] (Value 3))
  , Test "dzialania3" (SrcString "2 * 5") (Eval [] (Value 10))
  , Test "dzialania4" (SrcString "3 div 2") (Eval [] (Value 1))
  , Test "dzialania5" (SrcString "2 mod 1") (Eval [] (Value 0))
  , Test "dzialania6" (SrcString "-2") (Eval [] (Value (-2)))

  , Test "zmienne1" (SrcString "input x y in x + y + 0") (Eval [1, 2] (Value 3))
  , Test "zmienne2" (SrcString "input x y in 2 + x + y") (Eval [1, 2] (Value 5))
  , Test "zmienne3" (SrcString "input x y in x - y") (Eval [2, 1] (Value 1))
  , Test "zmienne4" (SrcString "input x y in x * y") (Eval [1, 2] (Value 2))
  , Test "zmienne5" (SrcString "input x y in x div y") (Eval [1, 2] (Value 0))
  , Test "zmienne6" (SrcString "input x y in x mod y") (Eval [1, 2] (Value 1))
  , Test "zmienne7" (SrcString "input x y z in x + y * z") (Eval [1, 2, 3] (Value 7))
  , Test "zmienne8" (SrcString "input x y z in x + y div z") (Eval [1, 2, 3] (Value 1))
  , Test "zmienne9" (SrcString "input x y z in x + y mod z") (Eval [1, 2, 3] (Value 3))

  , Test "let1" (SrcString "let x = 42 in x") (Eval [] (Value 42))
  , Test "let2" (SrcString "input x in let x = x + 2 in x + x") (Eval [2] (Value 8))
  , Test "let3" (SrcString "input x in let x = x + 1 in let x = x + 1 in x + x") (Eval [2] (Value 8))

  , Test "ify1" (SrcString "input x y in if true then x else y") (Eval [1, 2] (Value 1))
  , Test "ify2" (SrcString "input x y in if not true then x else y") (Eval [1, 2] (Value 2))
  , Test "ify3" (SrcString "input x y in if false then x else y") (Eval [1, 2] (Value 2))
  , Test "ify4" (SrcString "input x y in if not false then x else y") (Eval [1, 2] (Value 1))

  , Test "prostewarunki1" (SrcString "if 1 > 0 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki4" (SrcString "if 1 = 0 then 1 else 0") (Eval [] (Value 0))
  , Test "prostewarunki5" (SrcString "if 1 <> -1 then 1 else 0") (Eval [] (Value 1))

  , Test "dzialaniawarunki1" (SrcString "if 1 = 0 + 1 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki2" (SrcString "if 2 = 1 * 2 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki3" (SrcString "if 4 div 2 = 1 then 1 else 0") (Eval [] (Value 0))
  , Test "dzialaniawarunki4" (SrcString "if 2 * 3 mod 4 = 6 mod 4 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki5" (SrcString "if 2 > 1 or 3 < 2 then 1 else 0") (Eval [] (Value 1))

  , Test "boolwarunki1" (SrcString "if true or false then 1 else 0") (Eval [] (Value 1))
  , Test "boolwarunki2" (SrcString "if false or false then 1 else 0") (Eval [] (Value 0))

  , Test "prostezagniezdzone1" (SrcString "let x = if true then 1 else 0 in x") (Eval [] (Value 1))
  , Test "prostezagniezdzone2" (SrcString "if 1 = 0 + 1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostezagniezdzone3" (SrcString "input x in let y = true in if y then x else 0") (Eval [1] (Value 1))
  , Test "warunkizagniezdzone1" (SrcString "if (if 1 = 1 then false else true) then 1 else 0") (Eval [] (Value 0))
  , Test "warunkizagniezdzone2" (SrcString "if (if 1 = 1 then false else true) and (if 1 = 1 then false else true) then 1 else 0") (Eval [] (Value 0))
  , Test "warunkizagniezdzone3" (SrcString "if (if 1 = 1 then 1 else 0) = (if false then 1 else 0) then 1 else 0") (Eval [] (Value 0))
  , Test "nietypowy1" (SrcString "input x in if true then x + 42 else x div 0") (Eval [1] (Value 43))
  , Test "RuntimeError1" (SrcString "input x in if false then x + 42 else x div 0")  (Eval [1] RuntimeError)
  , Test "RuntimeError2" (SrcString "1 mod 0")  (Eval [] RuntimeError)

  , Test "TypeError1" (SrcString "let x = true in x") TypeError
  , Test "TypeError2" (SrcString "x ") TypeError
  , Test "TypeError3" (SrcString "if false or 0 then 1 else 0") TypeError
  , Test "TypeError4" (SrcString "1 + true") TypeError
  , Test "TypeError5" (SrcString "4 or 1") TypeError
  , Test "TypeError6" (SrcString "2 and 5") TypeError
  , Test "TypeError7" (SrcString "if 1 = 1 then false else 0") TypeError
  , Test "TypeError8" (SrcString "if 1 = 1 then false else true") TypeError
  , Test "TypeError9" (SrcString "if 1 then 1 else 0") TypeError
  , Test "TypeError10" (SrcString "if 1 = false then 1 else 0") TypeError
  , Test "TypeError11" (SrcString "if if 1 = 1 then 1 else 2 then 1 else 0") TypeError
  , Test "TypeError12" (SrcString "if true = true then 1 else 0") TypeError
  , Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "prostafunkcja1" (SrcString "fun janek(a:int*int):int*int = a in fst(janek(0,1))") (Eval [] (Value 0))
  , Test "prostafunkcja2" (SrcString "fun janek(a:int*int):int*int = a in snd(janek(0,1))") (Eval [] (Value 1))
  , Test "prostafunkcja3" (SrcString "fun tomek(a:int):int = a in tomek(2)") (Eval [] (Value 2))
  , Test "prostafunkcja4" (SrcString "fun bolek(u:unit):int = 2 in bolek()") (Eval [] (Value 2))
  , Test "prostafunkcja5" (SrcString "fun suma(a:int*int):int = fst a + snd a in suma(1,4)") (Eval [] (Value 5))
  , Test "prostafunkcja6" (SrcString "fun mietek(a:bool):int = if a then 1 else 0 in mietek(true)") (Eval [] (Value 1))
  , Test "prostafunkcja7" (SrcString "fun mietek(a:bool):int = if a then 1 else 0 in mietek(false)") (Eval [] (Value 0))
  , Test "prostafunkcjarek1" (SrcString "fun rek(a:int):int = if a=0 then 1 else rek(a-1) in rek(5)") (Eval [] (Value 1))
  , Test "prostafunkcjarek2" (SrcString "fun rek(a:int*int):int = if fst(a)=0 then snd(a) else rek(fst(a)-1,snd(a))*2 in rek(5,2) ") (Eval [] (Value 64))
  , Test "funkcjaargzloz1" (SrcString "fun fib(u:unit):int = 2 in fib()") (Eval [] (Value 2))
  , Test "funkcjaargzloz2" (SrcString "fun fib(a:int):int = a in fib(2)") (Eval [] (Value 2))
  , Test "funkcjaargzloz3" (SrcString "fun fib(a:int):int = a in fib(3-1)") (Eval [] (Value 2))
  , Test "funkcjaargzloz4" (SrcString "fun fib(a:int):int = a in fib(if true then 1 else 0)") (Eval [] (Value 1))
  , Test "funkcjaargzloz5" (SrcString "fun fib(a:int):int = a in fib(if false then 1 else 0)") (Eval [] (Value 0))
  , Test "funkcjaargzloz6" (SrcString "fun fib(a:int*int):int = fst(a) in fib(2,3)") (Eval [] (Value 2))
  , Test "funkcjaargzloz7" (SrcString "fun fib(a:int list):int = 1 in fib(1::2::3::[]: int list)") (Eval [] (Value 1))
  , Test "funkcjawfunkcii1" (SrcString "fun fib(u:unit):int = fib1() fun fib1(a:unit):int = 1 in fib()") (Eval [] (Value 1))
  , Test "funkcjawfunkcii2" (SrcString "fun fib1(a:unit):int = 1 fun fib(u:unit):int = fib1() in fib()") (Eval [] (Value 1))
  , Test "funkcjawfunkcii3" (SrcString "fun fib(u:unit):int = fib1(2) fun fib1(a:int):int = fib2(a) fun fib2(a:int):int = a in fib()") (Eval [] (Value 2))
  , Test "funkcjawfunkcii4" (SrcString "fun fib(a:int):int = fib1(a) fun fib1(a:int):int = a in fib(5)") (Eval [] (Value 5))
  , Test "typecheckerror2" (SrcString "fun tomek(a:int):bool = a in tomek(2)") TypeError
  , Test "typecheckerror3" (SrcString "fun suma(a:int*int):int*int = a in suma(1,4)") TypeError
  , Test "typecheckerror4" (SrcString "fun mietek(a:bool):int = if a then true else 0 in mietek(true)") TypeError
  , Test "typecheckerror5" (SrcString "let x = [0,1]:int list in fst x") TypeError
  , Test "typecheckerror6" (SrcString "fun fib(a:int):int = fib1(a) fun fib1(a:unit):int = a in fib(5)") TypeError
  , Test "typecheckerror7" (SrcString "let x = [false,1,false,true]:bool list in if (match x with [] -> true | x::xs -> x) then 1 else 0 ") TypeError
  , Test "typecheckerror8" (SrcString "let x = [if true then true else 0,2,3]:int list in match x with [] -> 0 | x::xs -> x") TypeError
  , Test "typecheckerror9" (SrcString "let x = (((2,3),4),5) in fst(snd(fst x))") TypeError
  , Test "typecheckerror10" (SrcString "let x = (if true then 1 else 0, if false then 1 else 0) in if (fst x) then 10 else 20") TypeError
  , Test "typecheckerror11" (SrcString "let x = [3,2]: int list in match x with [] -> true | x::xs -> match xs with [] -> 0 | x::xs -> x") TypeError
  , Test "typecheckerror12" (SrcString "let x = [3,2]: int list in match x with [] -> [1]:int list | x::xs -> match xs with [] -> 0 | x::xs -> x") TypeError

  , Test "zwracanafunkcja1" (SrcString "fun janek(a:int):int->int = fn(x:int) -> a+x in janek 5 2") (Eval [] (Value 7))
  , Test "zwracanafunkcja2" (SrcString "fun janek(a:int->int):int = a 5 fun tomek(a:int):int->int = fn(x:int) -> x+a in janek (tomek 1) ") (Eval [] (Value 6))
  , Test "zwracanafunkcja3" (SrcString "fun janek(a:int->int):int = a 5 + 5 in let x = fn(a:int) -> a*2 in janek x") (Eval [] (Value 15))
  , Test "zwracanafunkcja4" (SrcString "fun janek(u:unit):int->int = fn(x:int) -> x*2 fun tomek(a:int->int):int = a 2 in  tomek (janek())") (Eval [] (Value 4))
  , Test "zwracanafunkcja5" (SrcString "fun janek(a:int->int):int = a 5 in janek(fn(x:int) -> x*2)") (Eval [] (Value 10))
  , Test "zwracanafunkcja6" (SrcString "let x = fn(x:int)->x*x in x 2") (Eval [] (Value 4))
  , Test "zwracanafunkcja7" (SrcString "let x = (fn(x:int)->x*x,fn(x:int)->x-x) in snd x 2") (Eval [] (Value 0))

  , Test "runtime1" (SrcString "let x = fn(u:unit) -> 0 in x() div x()") (Eval [] RuntimeError)
  , Test "runtime2" (SrcString "let x = fn(u:unit) -> 0 in x() mod x()") (Eval [] RuntimeError)
  , Test "runtime3" (SrcString "let x = fn(y:int) -> 0 div y in x 0") (Eval [] RuntimeError)
  , Test "runtime4" (SrcString "let x = fn(y:int) -> 0 mod y in x 0") (Eval [] RuntimeError)

  , Test "typecheckfn1" (SrcString "let x = fn(x:int)->x*x in x true") TypeError
  , Test "typecheckfn2" (SrcString "x true") TypeError
  , Test "typecheckfn3" (SrcString "let x = fn(x:int->int)->x*x in x 2") TypeError
  , Test "typecheckfn4" (SrcString "fun janek(a:int):int = fn(x:int) -> a+x in janek 5 2") TypeError
  , Test "typecheckfn5" (SrcString "fun janek(a:int->int):int = a 5 + 5 in let x = fn(a:int) -> a*2 in janek x 2") TypeError
  ]
