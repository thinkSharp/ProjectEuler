module Problem41
open System

// Problem 41 -- What is the largest n digit pandigital prime that exists?
let isPrimeSqrt n =
    let rec check i =
        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))
    if n = 1L then false else check 2L   

let isPanDigital (n:int64) =
    let strN = n.ToString()
    let ln = strN.Length
    let indDigit = strN.ToCharArray()
                   |> Array.map(fun elm -> int32(elm.ToString()))
    let minDigit = indDigit
                   |> Array.min
    let shouldBeDigits = [| for i = minDigit to (minDigit + ln - 1) do yield  if i > 10 then "A" else i.ToString() |]

    let sbDigits = shouldBeDigits |> Array.fold(fun st elm -> st + elm) ""

    let isPan = shouldBeDigits
                  |> Array.map(fun elm -> strN.Contains(elm.ToString()))
                  |> Array.filter(fun elm -> elm = false)
   
    isPan.Length = 0

let getAllPossibleNum2Dig pre x y  =
    let strVal1 = pre + x + y
    let strVal2 = pre + y + x
    let s1, value1 = System.Int64.TryParse(strVal1)
    let s2, value2 = System.Int64.TryParse(strVal2)
     
    //printf "%s %s %b %b %d %d\n " strVal1 strVal2 s1 s2 value1 value2
    let val1 = if value1 % 2L <> 0L && value1 % 3L <> 0L && value1 % 5L <> 0L && value1 % 7L <> 0L && isPrimeSqrt value1 then  Set.empty.Add(value1) else Set.empty
    let val2 = if value2 % 2L <> 0L && value2 % 3L <> 0L && value2 % 5L <> 0L && value2 % 7L <> 0L && isPrimeSqrt value2 then  Set.empty.Add(value2) else Set.empty
    let y = Set.union val1 val2
    //printf "%d \n" y.Count
    y

let getAllPossibleNum3Dig pre x y z =
    let fst = getAllPossibleNum2Dig (pre + x) y z 
    let snd = getAllPossibleNum2Dig (pre + y) x z
    let trd = getAllPossibleNum2Dig (pre + z) x y
    let x = [fst;snd;trd]
    let y = Set.unionMany x
    //printf " 3 %d \n " y.Count
    y

let getAllPossibleNum4Dig pre w x y z =
    let fst = getAllPossibleNum3Dig (pre + w) x y z
    let snd = getAllPossibleNum3Dig (pre + x) w y z 
    let trd = getAllPossibleNum3Dig (pre + y) x w z
    let fot = getAllPossibleNum3Dig (pre + z) y x w 
    let x = [fst;snd;trd;fot]
    let y = Set.unionMany x
    //printf " 4 %d \n " y.Count
    y

let getAllPossibleNum5Dig pre v w x y z =
    let fst = getAllPossibleNum4Dig (pre + v) w x y z
    let snd = getAllPossibleNum4Dig (pre + w) v x y z 
    let trd = getAllPossibleNum4Dig (pre + x) w v y z
    let fot = getAllPossibleNum4Dig (pre + y) x w v z 
    let fif = getAllPossibleNum4Dig (pre + z) y x w v 
    let x = [fst;snd;trd;fot;fif]
    let y = Set.unionMany x
    //printf " 5 %d \n " y.Count
    y

let getAllPossibleNum6Dig pre u v w x y z =
    let fst = getAllPossibleNum5Dig (pre + u) v w x y z
    let snd = getAllPossibleNum5Dig (pre + v) u w x y z 
    let trd = getAllPossibleNum5Dig (pre + w) v u x y z
    let fot = getAllPossibleNum5Dig (pre + x) w v u y z 
    let fif = getAllPossibleNum5Dig (pre + y) x w v u z
    let six = getAllPossibleNum5Dig (pre + z) y x w v u 
    let x = [fst;snd;trd;fot;fif;six]
    let y = Set.unionMany x
    //printf " 6 %d \n " y.Count
    y

let getAllPossibleNum7Dig pre t u v w x y z =
    let fst = getAllPossibleNum6Dig (pre + t) u v w x y z
    let snd = getAllPossibleNum6Dig (pre + u) t v w x y z 
    let trd = getAllPossibleNum6Dig (pre + v) u t w x y z
    let fot = getAllPossibleNum6Dig (pre + w) v u t x y z 
    let fif = getAllPossibleNum6Dig (pre + x) w v u t y z
    let six = getAllPossibleNum6Dig (pre + y) x w v u t z 
    let sev = getAllPossibleNum6Dig (pre + z) y x w v u t 
    let x = [fst;snd;trd;fot;fif;six;sev]
    let y = Set.unionMany x
    //printf " 7 %d \n " y.Count
    y

let getAllPossibleNum8Dig pre s t u v w x y z =
    let fst = getAllPossibleNum7Dig (pre + s) t u v w x y z
    let snd = getAllPossibleNum7Dig (pre + t) s u v w x y z 
    let trd = getAllPossibleNum7Dig (pre + u) t s v w x y z
    let fot = getAllPossibleNum7Dig (pre + v) u t s w x y z 
    let fif = getAllPossibleNum7Dig (pre + w) v u t s x y z
    let six = getAllPossibleNum7Dig (pre + x) w v u t s y z 
    let sev = getAllPossibleNum7Dig (pre + y) x w v u t s z 
    let eig = getAllPossibleNum7Dig (pre + z) y x w v u t s 
    let x = [fst;snd;trd;fot;fif;six;sev;eig]
    let y = Set.unionMany x
    //printf " 8 %d \n " y.Count  
    y

let getAllPossibleNum9Dig pre r s t u v w x y z =
    let fst = getAllPossibleNum8Dig (pre + r) s t u v w x y z
    let snd = getAllPossibleNum8Dig (pre + s) r t u v w x y z 
    let trd = getAllPossibleNum8Dig (pre + t) s r u v w x y z
    let fot = getAllPossibleNum8Dig (pre + u) t s r v w x y z 
    let fif = getAllPossibleNum8Dig (pre + v) u t s r w x y z
    let six = getAllPossibleNum8Dig (pre + w) v u t s r x y z 
    let sev = getAllPossibleNum8Dig (pre + x) w v u t s r y z 
    let eig = getAllPossibleNum8Dig (pre + y) x w v u t s r z 
    let nin = getAllPossibleNum8Dig (pre + z) y x w v u t s r
    let x = [fst;snd;trd;fot;fif;six;sev;eig;nin]
    let y = Set.unionMany x
    //printf " nine %d \n " y.Count
    y
    
let getAllPossibleNum10Dig pre q r s t u v w x y z =
    printf " Testing 10 Dig %s %s %s %s %s %s %s %s %s %s %s\n" pre q r s t u v w x y z 
    let fst = getAllPossibleNum9Dig (pre + q) r s t u v w x y z
    let snd = getAllPossibleNum9Dig (pre + r) q s t u v w x y z 
    let trd = getAllPossibleNum9Dig (pre + s) r q t u v w x y z
    let fot = getAllPossibleNum9Dig (pre + t) s r q u v w x y z
    let fif = getAllPossibleNum9Dig (pre + u) t s r q v w x y z
    let six = getAllPossibleNum9Dig (pre + v) u t s r q w x y z
    let sev = getAllPossibleNum9Dig (pre + w) v u t s r q x y z 
    let eig = getAllPossibleNum9Dig (pre + x) w v u t s r q y z 
    let nin = getAllPossibleNum9Dig (pre + y) x w v u t s r q z
    let ten = getAllPossibleNum9Dig (pre + z) y x w v u t s r q
    let x = [fst;snd;trd;fot;fif;six;sev;eig;nin;ten]
    let y = Set.unionMany x
    //printf " Result is %d \n" y.Count
    y

let getAllPossibleNum11Dig pre p q r s t u v w x y z =
    //printf " Testing 10 Dig %s %s %s %s %s %s %s %s %s %s %s\n" pre q r s t u v w x y z 
    let fst = getAllPossibleNum10Dig (pre + p) q r s t u v w x y z
    let snd = getAllPossibleNum10Dig (pre + q) p r s t u v w x y z 
    let trd = getAllPossibleNum10Dig (pre + r) q p s t u v w x y z
    let fot = getAllPossibleNum10Dig (pre + s) r q p t u v w x y z
    let fif = getAllPossibleNum10Dig (pre + t) s r q p u v w x y z
    let six = getAllPossibleNum10Dig (pre + u) t s r q p v w x y z
    let sev = getAllPossibleNum10Dig (pre + v) u t s r q p w x y z 
    let eig = getAllPossibleNum10Dig (pre + w) v u t s r q p x y z 
    let nin = getAllPossibleNum10Dig (pre + x) w v u t s r q p y z
    let ten = getAllPossibleNum10Dig (pre + y) x w v u t s r q p z
    let elv = getAllPossibleNum10Dig (pre + z) y x w v u t s r q p
    let x = [fst;snd;trd;fot;fif;six;sev;eig;nin;ten;elv]
    let y = Set.unionMany x
    //printf " Result is %d \n" y.Count
    y

let getAllPossiblePanDigs (dig:string) =
    printf " %d \n " dig.Length
    let digVal = match dig.Length with
                 | x when x = 11 -> getAllPossibleNum11Dig ""  "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) (dig.Substring(6,1)) (dig.Substring(7,1)) (dig.Substring(8,1)) (dig.Substring(9,1))
                 | x when x = 10 ->printf "10 -- %s \n" dig 
                                   getAllPossibleNum10Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) (dig.Substring(6,1)) (dig.Substring(7,1)) (dig.Substring(8,1)) (dig.Substring(9,1))
                 | x when x = 9 -> getAllPossibleNum9Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) (dig.Substring(6,1)) (dig.Substring(7,1)) (dig.Substring(8,1))
                 | x when x = 8 -> getAllPossibleNum8Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) (dig.Substring(6,1)) (dig.Substring(7,1)) 
                 | x when x = 7 -> getAllPossibleNum7Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) (dig.Substring(6,1)) 
                 | x when x = 6 -> getAllPossibleNum6Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) (dig.Substring(5,1)) 
                 | x when x = 5 -> getAllPossibleNum5Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) (dig.Substring(4,1)) 
                 | x when x = 4 -> getAllPossibleNum4Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) 
                 | x when x = 3 -> getAllPossibleNum3Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) 
                 | _ -> getAllPossibleNum2Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) 
    printf "%d \n " digVal.Count
    if digVal.Count > 0 then digVal.MaximumElement else 0L

// Problem 41 -- What is the largest n digit pandigital prime that exists?


    
