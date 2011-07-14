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

let getAllPossiblePrimePermutationDigs (dig:string) =
    //printf " %d \n " dig.Length
    let digVal = match dig.Length with
                 | x when x = 4 -> getAllPossibleNum4Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) (dig.Substring(3,1)) 
                 | x when x = 3 -> getAllPossibleNum3Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) (dig.Substring(2,1)) 
                 | _ -> getAllPossibleNum2Dig "" (dig.Substring(0,1)) (dig.Substring(1,1)) 
    //printf "%s -- " dig

    digVal |> Set.filter(fun elm -> isPrimeSqrt(elm))
           

let getEquallyDistributedPermutation (nums:int64 []) =
    let rec innerLoop fstnum sndnum tirdnum isDone =
          match fstnum,sndnum,tirdnum, isDone with
           | _,_,_, true -> None
           | a,b,c, _ when nums.[b] - nums.[a] = nums.[c] - nums.[b] -> Some((nums.[a],nums.[b],nums.[c]))
           | a,b,c, _ when nums.[c] - nums.[b] > nums.[b] - nums.[a] -> // now we need to shift b and 
                                                                        //printf " In Second -- %d %d %d \n " a b c 
                                                                        let isBEnd = (nums.Length - 1) - (b + 1) < 1
                                                                        let isAEnd = (nums.Length - 1) - (a + 1) < 2
                                                                        let newB = if not isBEnd then b + 1 elif not isAEnd then a + 2 else 0
                                                                        let newA = if not isAEnd then a + 1 else a
                                                                        let newC = if not isBEnd then b + 2 elif not isAEnd then a + 3 else 0
                                                                        innerLoop newA newB newC isAEnd
           | a,b, c,_ -> // new we need to shift c
                        //printf " In Last -- %d %d %d \n " a b c 
                        let isCEnd = (nums.Length - 1) - (c + 1) < 0
                        let isBEnd = (nums.Length - 1) - (b + 1) < 1
                        let isAEnd = (nums.Length - 1) - (a + 1) < 2
                        let newC = if not isCEnd then c + 1 elif not isBEnd then b + 2 elif not isAEnd then a + 3 else 0
                        let newB = if not isCEnd then b elif not isBEnd then b + 1 elif not isAEnd then a + 2 else 0
                        let newA = if not isCEnd then a elif not isBEnd then a elif not isAEnd then a + 1 else 0
                        innerLoop newA newB newC isAEnd
    innerLoop 0 1 2 false
                        

let getPermutations limit =
    [|1001 .. limit|] |> Array.map(fun elm -> getAllPossiblePrimePermutationDigs (elm.ToString()))
                      |> Array.filter(fun elm -> elm.Count >= 3)
                      |> Array.map(fun elm -> getEquallyDistributedPermutation (elm |> Set.toArray))
                      |> Array.filter(fun elm -> elm <> None)
                      |> Array.iter(fun elm -> printf "%A \n" elm)


// Problem 49 -- What 12 - digit number can be form by concatenating the three terms in this sequence

