module Problem39
open System


// Problem 39 -- Find p, parameter of a right triangle with max solutions, for which value of p <= 1000, is the number of solutions maximised?

let getRightTriangleWithParm parmLimits countLimits =
    let rec interLoop x y z (result:Map<int,Set<string>>)  =
        match x, y, z with
         | a , b , c when a > countLimits -> result
         | a , b , c when b > countLimits -> interLoop (a + 1) 1 1 result
         | a , b , c when c > countLimits -> interLoop a (b + 1) 1 result
         | _ -> 
                let isRightTriangle = x * x + y * y = z * z
                let parm = if isRightTriangle then x + y + z else 0
                let newResult  = if parm > 0 && parm <= parmLimits then 
                                    let sides = if x < y then (x.ToString() + y.ToString() + z.ToString()) else (y.ToString() + x.ToString() + z.ToString())
                                    let parmVal = match result.TryFind(parm) with
                                                     | Some(value) -> value.Add(sides)
                                                     | None -> Set.empty.Add(sides)
                                    
                                    printf "%d %d %d %d \n " x y z parm
                                    result.Add (parm , parmVal)
                                 else
                                    result
                interLoop x y (z + 1) newResult
    interLoop 1 1 1 Map.empty


let getMaxParmFromList parmLimits countLimits =
    let res = getRightTriangleWithParm parmLimits countLimits
    
    let result = res |> Map.fold(fun state key value -> let count = value.Count
                                                        if count > snd state then key,count else state
                                ) (0,0)

    printf "%d %d \n" (fst result) (snd result)
    
        
    


// Problem 39 -- Find p, parameter of a right triangle with max solutions, for which value of p <= 1000, is the number of solutions maximised?




