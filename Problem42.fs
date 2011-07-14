module Problem42
open System

// Problem 42 -- Find all triangle words from the given text file

let triangleFun n =
    (n * ( n + 1)) / 2

let ToIntFromChar (c:Char) =
    (int c) - (int 'A') + 1

let IsTriangleWord (strWord:string) (triangleNums:Set<int>) =
 
    let strSum = strWord.ToString().ToCharArray()
                   |> Array.map(fun elm -> ToIntFromChar elm)
                   |> Array.sum
    printf "%s %d \n" strWord strSum
    triangleNums.Contains(strSum)

let generateTriangleNum limit =
    let rec generateInternal count (result:Set<int>) =
        match count = limit with
         | true -> result
         | _ -> generateInternal (count + 1) (result.Add(triangleFun count))
    generateInternal 0 Set.empty

let readDataFileP42 (filePath:string) = 
     use sr = new System.IO.StreamReader(filePath)    
                       
     let x = sr.ReadToEnd ()
     x.Split(',')

let countTriangleWord fileName  limit =
    let triangleNums = generateTriangleNum limit
    let data = readDataFileP42 fileName
                     
    let count = data |> Array.filter(fun elm -> IsTriangleWord elm triangleNums) 
    count.Length

// Problem 42 -- Find all triangle words from the given text file



