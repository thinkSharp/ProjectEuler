module Problem49
open System

// Problem 49 -- What 12 - digit number can be form by concatenating the three terms in this sequence
let isPrimeSqrt n =
    let rec check i =
        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))
    if n = 1L then false else check 2L   

let getAllPossibleNum2Dig pre x y  =
    let strVal1 = pre + x + y
    let strVal2 = pre + y + x
    let s1, value1 = System.Int64.TryParse(strVal1)
    let s2, value2 = System.Int64.TryParse(strVal2)
     
    let y = Set.empty.Add(value1).Add(value2)
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

let getAllPossiblePanDigs (dig:string) =
    //printf " %d \n " dig.Length
    let digVal = match dig.Length with
                 | x when x = 4 -> getAllPossibleNum4Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) 
                 | x when x = 3 -> getAllPossibleNum3Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) 
                 | _ -> getAllPossibleNum2Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) 
    printf "%s -- " dig

    digVal |> Set.filter(fun elm -> isPrimeSqrt(elm))
           |> Set.iter(fun elm -> printf " %d " elm)

    printf "\n"

let getequallydistributedPermutation (nums:int64 []) =
    let rec innerLoop start skip next diff result =
          match next < (nums.Length - 1) with
           | true -> result
           | _ -> let newResult = nums.[next] - nums.[start] = diff
                  
                  

let getPermutations limit =
    [|1001 .. limit|] |> Array.iter(fun elm -> getAllPossiblePanDigs (elm.ToString()))

// Problem 49 -- What 12 - digit number can be form by concatenating the three terms in this sequence

