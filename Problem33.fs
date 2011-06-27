module Problem33
open System

// Problem 33 Find the denominator of the product of non-trivial fraction that are less then one in value and containing two digits in the numerator and denominator
let rec GCD x y =
  match y = 0 with
  | true -> x
  | _ -> GCD y (x % y)

let isNonTrivialFraction (num:int) (denom:int) =
    let x = num.ToString().Substring(1)
    let y = denom.ToString().Substring(0,1)
    //printf "%s %s \n" x y
    let vals = if x = y && x <> "0" then int(num.ToString().Substring(0,1)), int(denom.ToString().Substring(1)) else 0 , 0
    if fst vals <> 0 && snd vals <> 0 then
        let gcdOfOrig = GCD num denom
        let gcdOfNonTri = GCD (fst vals) (snd vals)
        let newOrg = num / gcdOfOrig, denom / gcdOfOrig
        let newNonTri = fst vals / gcdOfNonTri , snd vals / gcdOfNonTri
        //printf "%d %d %d %d \n" (fst newOrg) (snd newOrg) (fst newNonTri) (fst newNonTri)
        if fst newOrg  = fst newNonTri && snd newOrg = snd newNonTri then newOrg else 0,0
    else
        0,0

let getAllNonTrivialFractionValues =
    let results = seq {for i in 10 .. 99 do
                        for j in (i + 1) .. 99 do
                            yield i , j
                    }
                    |> Seq.filter(fun elm -> let vals = isNonTrivialFraction (fst elm) (snd elm)
                                             fst vals <> 0 && snd vals <> 0)
                    |> Seq.fold (fun st elm -> fst st * fst elm , snd st * snd elm) (1,1)

    let gcdRes = GCD (fst results) (snd results)
    let finalRes = snd results / gcdRes
    finalRes
                          
                                     
// Problem 33 Find the denominator of the product of non-trivial fraction that are less then one in value and containing two digits in the numerator and denominator

