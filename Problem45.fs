module Problem45
open System

// Problem 45 -- Find the next triangle number that is also pentagonal and hexagonal

let getTriangle (n:int32) =
    lazy ( let m = int64(n)
           (m * (m + 1L) ) / 2L)

let getPentagonal (n:int32) =
    lazy(let m = int64(n)
         (m * ( 3L * m - 1L) ) / 2L)

let getHexagonal (n:int32) =
    lazy( let m = int64(n)
          m * ((2L * m) - 1L))

let x = 10001
let hexaVal = (getHexagonal x).Value

let getTriangleNumber start till =
    let triangles = [|for i in 1 .. till do yield getTriangle i|]
    let pentagonals = [|for i in 1 .. till do yield getPentagonal i|]
    let hexagonals = [| for i in 1 .. till do yield getHexagonal i|]

    printf " %d %d %d \n" triangles.Length pentagonals.Length hexagonals.Length
    let rec getMachingPenta count matchVal currentVal =
        match count >= till || currentVal > matchVal , matchVal = currentVal with
         | _ , true -> count
         | true , _ -> 0
         |  _ , _ -> getMachingPenta (count + 1) matchVal pentagonals.[count].Value

    let rec getMatchingHexa count matchVal currentVal =
        match count >= till || currentVal > matchVal, matchVal = currentVal with
          | _ , true -> count
          | true , _ -> 0
          | _ , _ -> getMatchingHexa (count + 1) matchVal hexagonals.[count].Value

    let rec getMachingTriangleNum count result =
        match count >= till, result <> 0L with
         | _ , true -> count
         | true , _ -> 0
         | _ , _ -> let triangleVal = triangles.[count].Value
                    let pentaVal = getMachingPenta 0 triangleVal 0L 
                    let hexaVal = if pentaVal = 0 then 0 else getMatchingHexa 0 triangleVal 0L
                    if hexaVal <> 0 then printf "%d %d %d = %d \n " count pentaVal hexaVal triangleVal
                    getMachingTriangleNum (count + 1) (if hexaVal <> 0 then triangleVal else 0L)
    getMachingTriangleNum (start - 1) 0L

// Problem 45 -- Find the next triangle number that is also pentagonal and hexagonal

