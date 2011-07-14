module Problem36
open System

// Problem 36 -- Find the sum of all numbers, less then one million, which are palindromic in base 10 and base 2

let pow num = 
  let rec powInternal count result =
    match count = 0 with
     | true -> result
     | _ -> powInternal (count - 1) (result * 2)
  if num = 0 then 1 else powInternal num 1

let toBinary num =
    let rec toBinarryInternal rem result =
        match rem = 0 with
         | true -> result
         | _ -> let newRem = rem / 2
                let newRes = (rem % 2).ToString() + result
                toBinarryInternal newRem newRes
    toBinarryInternal num ""

let toDecimal (biNum:string) =
    let limit = biNum.Length
    let rec toDecimalInternal pointer result =
        //printf "%d %d \n" pointer result
        match pointer = -1 with
         | true -> result
         | _ -> //printf " %d %s %d %d \n" (limit - pointer - 1) (biNum.Substring(0,1)) pointer result
                let newResult = if  biNum.Substring((limit - pointer - 1),1) = "1" then (result + pow (pointer)) else result
                toDecimalInternal (pointer - 1) newResult
    toDecimalInternal  (limit - 1) 0

let isPalindrom (str:string) =
    let strLen = str.Length - 1
    let limit = str.Length / 2
    let rec isPalindromInternal count result =
        //printf "%d %d \n" limit count
        match limit, result with
         | x, y when x = count || y = false -> result
         | _ -> isPalindromInternal (count + 1) (str.Substring(count, 1) = str.Substring((strLen - count), 1))
    isPalindromInternal 0 true

let getSumOfPalindromOfBaseTenAndTwoUnder limit =
  let x = {1 .. limit}
           |> Seq.filter(fun elm -> isPalindrom(elm.ToString()))
           |> Seq.map(fun elm -> toBinary elm)
           |> Seq.filter(fun elm -> isPalindrom(elm))
           |> Seq.map(fun elm -> toDecimal(elm))
  
//  let y = {10 .. limit}
//           |> Seq.filter(fun elm -> isPalindrom(elm.ToString()))
//           |> Seq.iter(fun elm -> printf "%d \n" elm)
//
//  x |> Seq.iter(fun elm -> printf "%d \n" elm)

  x |> Seq.sum 
    

// Problem 36 -- Find the sum of all numbers, less then one million, which are palindromic in base 10 and base 2

