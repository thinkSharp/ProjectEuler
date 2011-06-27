module Tutorial2

open System

 // Problem 18 -- Find the Maximum total from top to bottom of the triangle below

// Problem 67 -- Find the Maximum total from top to bottom of the triangle below ( 100) rows

let triangleData = 

                    [|
                    [|75;|];
                    [|95; 64;|];
                    [|17; 47; 82;|];
                    [|18; 35; 87; 10;|];
                    [|20; 04; 82; 47; 65;|];
                    [|19; 01; 23; 75; 03; 34;|];
                    [|88; 02; 77; 73; 07; 63; 67;|];
                    [|99; 65; 04; 28; 06; 16; 70; 92;|];
                    [|41; 41; 26; 56; 83; 40; 80; 70; 33;|];
                    [|41; 48; 72; 33; 47; 32; 37; 16; 94; 29;|];
                    [|53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14;|];
                    [|70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57;|];
                    [|91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48;|]
                    [|63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31;|];
                    [|04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23;|];|]
 
let testData = [|[|3;|];[|7;4;|];[|2;4;6;|];[|8;5;9;3;|];|]


let readDataFile (filePath:string) = 
    let triangleData =
                    [|   
                     use sr = new System.IO.StreamReader(filePath)    
                     while not sr.EndOfStream do        
                        let x = sr.ReadLine ()
                        let vals = x.Split(';')
                                    |> Array.map (fun elm -> Int32.Parse(elm))
                            
                        yield vals
                    |]
    triangleData
     
let problem67Data = readDataFile "C:\Source\F#\ctoCorner\Problem67.txt"

let getMaxTotalBottomUpBySumming (tData: int [][]) =
  let rec ReplaceCurrentWithMaxOfTwo row (result:int []) =
        match row with 
         | 0 -> let left = tData.[0].[0] + result.[0]
                let right = tData.[0].[0] + result.[1]
                if left > right then left else right
         | _ -> //printfn "%d %d \n" row result.Length
                let newResult = Array.mapi (fun i elm -> (
                                                           let left = elm + result.[i] 
                                                           let right = elm + result.[i + 1]
                                                           //printfn "%d %d %d %d \n" left right i elm
                                                           if left > right then
                                                            left
                                                           else 
                                                            right
                                                         )) tData.[row]
                ReplaceCurrentWithMaxOfTwo (row - 1) newResult
  let len = tData.Length
  ReplaceCurrentWithMaxOfTwo (len - 2) tData.[len - 1]
 
let getMaxTotalBottomUp (tData:int [][]) =
  let rec getMaxEachRow row left right result =
     match row with
      | xRow when xRow = -1 -> result
      | _ ->  let leftData = tData.[row].[left]
              let rightData = if tData.[row].Length > right then tData.[row].[right]  else 0
              let maxData = if leftData > rightData then leftData else rightData
              let newLeft = if leftData > rightData then 
                               if left = 0 then left else left - 1 
                            else right - 1
              let newRight = newLeft + 1
              printf "%d %d %d \n" newLeft newRight maxData
              getMaxEachRow (row - 1) newLeft newRight (result + maxData)

  let maxData = tData.[tData.Length - 1] |> Array.max
  let index =  tData.[tData.Length - 1] |> Array.findIndex(fun x -> x = maxData) 
  let leftIndex = index - 1
  printf "%d %d %d \n" index leftIndex maxData

  getMaxEachRow (tData.Length - 2) leftIndex (leftIndex + 1) maxData

let getMaxTotalTopDown (tData:int [][]) =
  let rec getMaxEachRow row left right result =
     match row with
      | xRow when xRow = tData.Length -> result
      | _ ->  let leftData = tData.[row].[left]
              let rightData = tData.[row].[right] 
              let maxData = if leftData > rightData then leftData else rightData
              let newLeft = if leftData > rightData then left else right
              let newRight = newLeft + 1
              printf "%d %d %d \n" newLeft newRight maxData
              getMaxEachRow (row + 1) newLeft newRight (result + maxData)

  getMaxEachRow 3 0 1 17  
// Problem 67 -- Find the Maximum total from top to bottom of the triangle below ( 100) rows     

// Problem 18 -- Find the Maximum total from top to bottom of the triangle below
// Problem 19 -- How many Sundays fell on the first of the month during the twentieth century
let getNextSunDay current month year =
    let getDaysByMonth month year =
        let isLeapYear year =
          match year % 100 = 0 with
           | true -> year % 400 = 0
           | false -> year % 4 = 0
        match month with
         | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
         | 4 | 6 | 9 | 11 -> 30
         | 2 -> match isLeapYear year with
                  | true -> 29
                  | false -> 28
         | _ -> 0
    let getNextMonthYear month year =
        if month = 12 then
            1, (year + 1)
        else
            (month + 1), year 
    let currentMonthMaxDay = getDaysByMonth month year 
    let rec addDay count (day, month, year) =
        match count with
           | x when x = 7 -> day, month, year
           | _ ->let newCount = count + 1 
                 if currentMonthMaxDay = day then
                    let nextMonth, newYear = getNextMonthYear month year
                    let newDay = 1
                    addDay newCount (newDay,nextMonth,newYear)       
                 else
                    let newDay = day + 1
                    addDay newCount (newDay,month, year) 
    addDay 0 (current,month,year) 
let getCountsundaysAsFirstOfMonth (sDay, sMonth, sYear) (eDay,eMonth,eYear) =
    let rec getCountInternal current result =
        match current with
         | cDay,cMonth,cYear when cYear > eYear -> result
         | cDay,cMonth,cYear -> //printf "%d %d %d \n" cDay cMonth cYear 
                                let nDay,nMonth,nYear = getNextSunDay cDay cMonth cYear
                                let newResult = if nDay = 1 then 
                                                    //printf "%d %d %d \n" nDay nMonth nYear 
                                                    result + 1 
                                                else 
                                                    result
                                getCountInternal (nDay,nMonth,nYear) newResult
    getCountInternal (sDay,sMonth,sYear) 0

// Problem 19 -- How many Sundays fell on the first of the month during the twentieth century
// Problem 20 -- Som of the digits of the result of n! of 100
let getFactorial num =
    let rec factorial n result =
        match n with 
         |x when x = 1I -> result
         | _ -> factorial (n - 1I) (result * (n - 1I))
    factorial num num

let getSumOfDigit num =
    let fact = getFactorial num
    printf "%A \n" fact
    let sum = fact.ToString().ToCharArray()
                 |> Array.sumBy(fun x -> System.Convert.ToInt32(x.ToString()))
    sum
// Problem 20 -- Som of the digits of the result of n! of 100
// Problem 21 -- Evaluate the sum of all amicable numbers under 10000

let getSumOfAllDivisers num =
    let rec getSumOfDivisers div result =
        match div with
         | 1 -> result
         | _ -> let newDiv = div - 1
                let newResult = if num % newDiv = 0 then result + newDiv else result
                getSumOfDivisers newDiv newResult
    getSumOfDivisers ((num / 2) + 1) 0

let getAmicablePair num =
  let x = getSumOfAllDivisers num 
  let y = getSumOfAllDivisers x
  if num = y  && num <> x then
    //printf "%d %d \n" num x
    (num , x)
  else
    (0 , 0)

let SumOfAmicableNumbersUnder num =
    let rec findAmicablePair count (result: (int * int) list) =
        let isAmicableValueInPair (pair:int * int) =
            count = fst pair || count = snd pair

        match count with
         | x when x = num -> result
         | _ -> match List.tryFind isAmicableValueInPair result with
                  | Some _ -> findAmicablePair(count + 1) result
                  | None  ->  let pair = getAmicablePair count 
                              let newResult = if fst pair <> 0 then result @ [pair] else result
                              findAmicablePair (count + 1) newResult
    let result = findAmicablePair 2 [] 
                    |> List.fold (fun sum (x,y) ->  sum + x + y) 0
    result
// Problem 21 -- Evaluate the sum of all amicable numbers under 10000
// Problem 22 -- What is the total of all the name scores in the file

let readDataFileP22 (filePath:string) = 
     use sr = new System.IO.StreamReader(filePath)    
     let x = sr.ReadToEnd ()
     x.Split(',')

let getNameScore (name:string) =
    let sumOfName = name.ToCharArray()
                            |> Array.map(fun elm -> System.Convert.ToInt32(elm))
                            |> Array.sum
    let score = sumOfName - (64 * name.ToCharArray().Length)
    score

let getScoreForAllNames filePath =
    let scores = readDataFileP22 filePath
                    |> Array.sort
                    |> Array.mapi(fun i elm -> (int64(i) + 1L) * int64(getNameScore elm))
                    |> Array.sum
    scores
// Problem 22 -- what is the total of all the name scores in the file

// Problem 23 -- what is the sum of all the positive integers which cannot be written as the sum of two abundant numbers

let getSumOfAllDivisors num =
    let rec getSumOfDivisors count result =
        match count with
         | x when x > (num + 1) /2 -> result
         | _ -> //printf "%d %d \n" count result
                let newResult = if num % count = 0 then result + count else result
                getSumOfDivisors (count + 1) newResult
    getSumOfDivisors 1 0

let getNextAbundantNum num =
    let rec getAbundantNum count result =
        match result > count with
         | true -> count
         | false -> let newCount = count + 1
                    let newResult = getSumOfAllDivisors newCount
                    //printf "%d %d \n" newCount newResult
                    getAbundantNum newCount  newResult
    getAbundantNum num  0
//
//let getMultiplyOfAbundantNumLessThen abnd num currentResult =
//    let rec getMultiplyOfAbundantNum count last result =
//        match last with
//         | x when x > num -> //List.iter (fun elm -> printf"%d \n" elm) result
//                             result           
//         | _ -> let mult =  abnd * count 
//                match List.tryFind ( fun elm -> count = elm ) result with
//                 | None -> getMultiplyOfAbundantNum (count + 1) mult ( mult :: result)
//                 | Some _ -> getMultiplyOfAbundantNum (count + 1) last result
//    getMultiplyOfAbundantNum 2 0 currentResult

let getAllAbundantNumberLessThen num =
    let rec getAbundantNumber count result =
        //printf "%d \n" count
        match count with
         | x when x > num -> result
         | _ -> match List.tryFind ( fun elm -> count = elm ) result with
                 | Some _ -> getAbundantNumber (count + 1) result
                 | None -> let abdNum = getNextAbundantNum count 
                           //let abdNums = getMultiplyOfAbundantNumLessThen abdNum num result
                           let newResult = if abdNum < num then  result @ [abdNum]  else result
                           if abdNum % 2 <> 0 then printf"%d \n" abdNum else ()
                           
                           getAbundantNumber abdNum newResult
    let result = getAbundantNumber 11 []
    //List.iter( fun x -> printf "%d \n" x) result
    List.sort result

//let isSumOfTwoAbundantNum num (abdList1:int list) (abdList2:int list) =
//   let rec isSumOfTwoAbundantInternal fst snd (fstLst:int list) (sndLst:int list) =
//        match fst,snd with
//         | x,y when x + y = num -> true
//         | x,y when x > num/2  && y > num/2 ->  printf "%d \n" num
//                                                false
//         | x,y when y > num/2 -> //printf "%d %d \n" fst snd 
//                                isSumOfTwoAbundantInternal fstLst.Head abdList2.Head fstLst.Tail abdList2.Tail
//         | _ -> //printf "%d %d \n" fst snd
//                isSumOfTwoAbundantInternal fst sndLst.Head fstLst sndLst.Tail
//   isSumOfTwoAbundantInternal abdList1.Head abdList2.Head abdList1.Tail abdList2.Tail

let abNums = getAllAbundantNumberLessThen 5987;;

let lengthOf = abNums.Length

//let sumOfTwoAbd = 
//    let lst = [||]
//    for i in abNums do
//    for j in abNums do
//        printf "abd nums : %d %d %d \n" i j (i + j)
//        let sum = i + j
//        if sum <= 28123 then
//          lst <- sum

                   
          

let sortedUniqueSum  xs = xs |> Set.ofList |> Set.toList

let isSumOfTwoAbundNums num (sumTwoAbd:int list) =
    match List.tryFind (fun x -> x = num) sumTwoAbd with
    | Some x -> //printf "%d \n" x 
                true
    | None -> false


let sumOfNPN lst= 
     let lstUniqueSum = sortedUniqueSum sumTwoAbd |> List.sort
     List.fold (fun sum elm -> if isSumOfTwoAbundNums elm lstUniqueSum  then 
                                    //printf "Hello world %A \n" sum
                                    sum 
                               else 
                                    printf "Hello world Else  %A %d \n"  sum elm
                                    sum + System.Numerics.BigInteger(elm) 
               ) 0I lst
// problem 23 -- what is the sum of all the positive integers which cannot be written as the sum of two abundant numbers
//4179871 4968650

