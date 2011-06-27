
// F# Tutorial File
module Tutorial
open System
    
let isPrime x =
    let mutable answer = [false;]
    let sqNum = int64(sqrt(double(x)))
    
    for i in 2L .. sqNum do
        let value = x % i = 0L 
        //printfn "%b" value
        let valueL = value :: []
        answer <- answer @ valueL
    
    let prime = List.exists (fun elm -> elm) answer
    not prime 

let getPrimeNumbers x =
   let primNums = {2L .. x}
                   |> Seq.filter (fun elm -> elm % 2L <> 0L)
                   |> Seq.filter (fun elm -> elm % 3L <> 0L)
                   |> Seq.filter (fun elm -> elm % 5L <> 0L)
                   |> Seq.filter (fun elm ->isPrime elm)
   Seq.append {2L .. 3L}  primNums

let largestPrimeFactor x elem acc =
    if x% elem = 0L then
        //printf "%d" elem
        elem
    else
        acc

let getLargestPrimeFactor x =
    let getPrimNums = getPrimeNumbers x
                           
    Seq.fold (fun acc elem -> largestPrimeFactor x elem acc) 2L getPrimNums



let isNumberPalindrome x =
   let mutable isPal = true
   let sX = string(x)
   let lensX = sX.Length
   let halfLx = int(lensX / 2)
   for i= 0 to halfLx do
     let tailLen = lensX - i - 1
     if( sX.[i] = sX.[tailLen]) && isPal then
        isPal <- true
     else
        isPal <- false
   isPal


let getLargestPal =
    let mutable i = 1000
    let mutable j = 999
    let mutable largestPal = 0
    while (i > 99) do
        i <- i - 1
        j <- 999
        while (j > 99) do
            let mult = j * i 
            printf "%d * %d = %d" i j mult
            if (isNumberPalindrome mult) then
                i <- 99
                j <- 99
                largestPal <- mult
            else
                j <- j - 1
    largestPal

let isDivisibleBy1To20 elm =
    //elm % 2 = 0 && elm % 3 = 0 && elm % 4 = 0 && elm % 5 = 0 && elm % 6 = 0 && elm % 7 = 0 && elm % 8 = 0 && elm % 9 = 0 && elm % 10 = 0 
    elm % 11 = 0 && elm % 12 = 0 && elm % 13 = 0 && elm % 14 = 0 && elm % 15 = 0 && elm % 16 = 0 && elm % 17 = 0 && elm % 18 = 0 && elm % 19 = 0 && elm % 20 = 0 

let getSmallestIntDivisibleBy1to20 lastVal =
   let num = seq { 1 .. lastVal} 
                |> Seq.find(isDivisibleBy1To20)
   num


let getDiff x =
    let first = {1 .. x}
                 |> Seq.sum

    let sqfirst = first * first 

    let second = {1 .. x}
                   |> Seq.map(fun x -> x * x )
                   |> Seq.sum

    let ans = sqfirst - second
    ans


let isprime n =

    let rec check i =

        i > n/2L || (n % i <> 0L && check (i + 1L))

    check 2L

 

let rec getNextPrime (i:int64) (isPrm:bool) =

    if  isPrm then

        i

    else

        let x =  isprime (i + 1L)

        getNextPrime (i + 1L) x

 

let getNthPrime n =

   

    let rec getNthPrimeInternal orig count prime =

        if count = orig then

            prime

        else

            let nPrime = getNextPrime prime false

            getNthPrimeInternal orig (count + 1L) nPrime

 

    getNthPrimeInternal n 1L 2L

 

 

// Problem 7 10001st Prime finder

 

// Problem 8 -- grestest product of five consecutive digits 

 

let digits1000 = [|

    7;3;1;6;7;1;7;6;5;3;1;3;3;0;6;2;4;9;1;9;2;2;5;1;1;9;6;7;4;4;2;6;5;7;4;7;4;2;3;5;5;3;4;9;1;9;4;9;3;4;9;6;9;8;3;5;2;0;3;1;2;7;7;4;5;0;6;3;2;6;2;3;9;5;7;8;3;1;8;0;1;6;9;8;4;8;0;1;8;6;9;4;7;8;8;5;1;8;4;3;

    8;5;8;6;1;5;6;0;7;8;9;1;1;2;9;4;9;4;9;5;4;5;9;5;0;1;7;3;7;9;5;8;3;3;1;9;5;2;8;5;3;2;0;8;8;0;5;5;1;1;1;2;5;4;0;6;9;8;7;4;7;1;5;8;5;2;3;8;6;3;0;5;0;7;1;5;6;9;3;2;9;0;9;6;3;2;9;5;2;2;7;4;4;3;0;4;3;5;5;7;

    6;6;8;9;6;6;4;8;9;5;0;4;4;5;2;4;4;5;2;3;1;6;1;7;3;1;8;5;6;4;0;3;0;9;8;7;1;1;1;2;1;7;2;2;3;8;3;1;1;3;6;2;2;2;9;8;9;3;4;2;3;3;8;0;3;0;8;1;3;5;3;3;6;2;7;6;6;1;4;2;8;2;8;0;6;4;4;4;4;8;6;6;4;5;2;3;8;7;4;9;

    3;0;3;5;8;9;0;7;2;9;6;2;9;0;4;9;1;5;6;0;4;4;0;7;7;2;3;9;0;7;1;3;8;1;0;5;1;5;8;5;9;3;0;7;9;6;0;8;6;6;7;0;1;7;2;4;2;7;1;2;1;8;8;3;9;9;8;7;9;7;9;0;8;7;9;2;2;7;4;9;2;1;9;0;1;6;9;9;7;2;0;8;8;8;0;9;3;7;7;6;

    6;5;7;2;7;3;3;3;0;0;1;0;5;3;3;6;7;8;8;1;2;2;0;2;3;5;4;2;1;8;0;9;7;5;1;2;5;4;5;4;0;5;9;4;7;5;2;2;4;3;5;2;5;8;4;9;0;7;7;1;1;6;7;0;5;5;6;0;1;3;6;0;4;8;3;9;5;8;6;4;4;6;7;0;6;3;2;4;4;1;5;7;2;2;1;5;5;3;9;7;

    5;3;6;9;7;8;1;7;9;7;7;8;4;6;1;7;4;0;6;4;9;5;5;1;4;9;2;9;0;8;6;2;5;6;9;3;2;1;9;7;8;4;6;8;6;2;2;4;8;2;8;3;9;7;2;2;4;1;3;7;5;6;5;7;0;5;6;0;5;7;4;9;0;2;6;1;4;0;7;9;7;2;9;6;8;6;5;2;4;1;4;5;3;5;1;0;0;4;7;4;

    8;2;1;6;6;3;7;0;4;8;4;4;0;3;1;9;9;8;9;0;0;0;8;8;9;5;2;4;3;4;5;0;6;5;8;5;4;1;2;2;7;5;8;8;6;6;6;8;8;1;1;6;4;2;7;1;7;1;4;7;9;9;2;4;4;4;2;9;2;8;2;3;0;8;6;3;4;6;5;6;7;4;8;1;3;9;1;9;1;2;3;1;6;2;8;2;4;5;8;6;

    1;7;8;6;6;4;5;8;3;5;9;1;2;4;5;6;6;5;2;9;4;7;6;5;4;5;6;8;2;8;4;8;9;1;2;8;8;3;1;4;2;6;0;7;6;9;0;0;4;2;2;4;2;1;9;0;2;2;6;7;1;0;5;5;6;2;6;3;2;1;1;1;1;1;0;9;3;7;0;5;4;4;2;1;7;5;0;6;9;4;1;6;5;8;9;6;0;4;0;8;

    0;7;1;9;8;4;0;3;8;5;0;9;6;2;4;5;5;4;4;4;3;6;2;9;8;1;2;3;0;9;8;7;8;7;9;9;2;7;2;4;4;2;8;4;9;0;9;1;8;8;8;4;5;8;0;1;5;6;1;6;6;0;9;7;9;1;9;1;3;3;8;7;5;4;9;9;2;0;0;5;2;4;0;6;3;6;8;9;9;1;2;5;6;0;7;1;7;6;0;6;

    0;5;8;8;6;1;1;6;4;6;7;1;0;9;4;0;5;0;7;7;5;4;1;0;0;2;2;5;6;9;8;3;1;5;5;2;0;0;0;5;5;9;3;5;7;2;9;7;2;5;7;1;6;3;6;2;6;9;5;6;1;8;8;2;6;7;0;4;2;8;2;5;2;4;8;3;6;0;0;8;2;3;2;5;7;5;3;0;4;2;0;7;5;2;9;6;3;4;5;0 

    |]

 

//let count = 0

//let prd = digits1000.[count] * digits1000.[count + 1] * digits1000.[count + 2] * digits1000.[count + 3] * digits1000.[count + 4]

 

let largestProductOf5Digit (digts: int []) =

    let rec calcProd count ans =

        if count > 995 then

            ans

        else

            if count > 994 then

                printf "%d %d %d %d %d" digts.[count]  digts.[count + 1]  digts.[count + 2]  digts.[count + 3]  digts.[count + 4]

 

            let prd = digts.[count] * digts.[count + 1] * digts.[count + 2] * digts.[count + 3] * digts.[count + 4]

            let newAns = if prd > ans then prd else ans

            calcProd (count + 1) newAns

 

    calcProd 0 0

 

// Problem 8 -- greatest product of five consutive digits

 

 

// Problem 9 -- find the product of abc where abc are side of a Pythagorean where a + b + c = 1000

// eHow Help to find Pythagorean Triples get the lowest set and multiply with common factor 

// 3 4 5 * 2 = 6,8,10

let getproductofPythagoreanWhosumIs1000 =

    let rec calcProduct a b c =

        if (a*a + b*b = c*c) && (a + b + c = 1000) then

            a,b,c

        elif a+b+c > 1000 then

           -a,-b,-c

        else

            calcProduct (a + 2) (b + 2) (c + 2)

    calcProduct 3 4 5

 

 

// WIKIpedia formula --http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples

let generatePythagorenTriplesWithI n =

     (2 * n + 1 ,2 * n * (n + 1), 2 * n * ( n + 1 ) + 1)

 

let generatePythagorenTriplesWithII m =

    (2 * m , (m * m) - 1 , (m * m) + 1) 

 

let getProductOfPythagorenThriplesWithSumIs1000 =

    let rec calcProduct  n =

        let m = n + 1

        let tup = generatePythagorenTriplesWithI m

        

        let a,b,c = tup

        printf "Start: %d %d %d %d" n a b c

 

        let isPythagoren = a*a + b*b = c*c

        let sum = a + b + c

        match isPythagoren  with

            | t when t = true && sum = 1000 -> a * b * c

            | t when t = true && sum > 1000 -> -(a * b * c)

            | _ -> calcProduct  m

                    

    calcProduct 2

 

// formula approach didn't work - let try recursive to the rescue

let getProductOfPythagorenThriplesWithSum1000  initial product =

    let rec incrementC a b c product =

        let shouldContinue = b < c && c < 1000

        match shouldContinue with

            | false -> 0

            | true -> 

                let isSum1000 = a + b + c = product && a * a + b * b = c * c

                //printf " From C %d %d %d \n" a b c

                match isSum1000 with

                   | true -> printf " Result C %d %d %d \n" a b c 

                             a * b * c

                   | false -> incrementC a b (c + 1) product

 

    let rec incrementB a b  product=

        let shouldContinue = a < b && b < 1000

        //printf "In side B %d %d %b \n" a b shouldContinue

        match shouldContinue with

            | false -> 0

            | true -> let result = incrementC a b (b + 1) product

                      //printf "From B %d %d \n" a b

                      match result with

                       | x when x > 0  ->printf "Result B %d %d \n" a b 

                                         result

                       | _ -> incrementB a (b + 1) product

 

    let rec incrementA a stop product =

        let shouldContinue = a < 1000 && a <= stop

        match shouldContinue with

            | false -> 0

            | true -> let result = incrementB a (a + 1) product

                      printf "From A %d %d \n" a result

                      match result with

                      | x when x > 0 -> result

                      | _ -> incrementA (a + 1) stop product

 

    let rec runRecCode start stop result product =

        let shouldContinue = start <= stop

        match shouldContinue with

         | false -> result

         | true -> incrementA (start + 1) stop product

 

    runRecCode initial (initial + 100)  0 product

 

// Problem 9 -- find the product of abc where abc are side of a Pythagorean where a + b + c = 1000

 

// Problem 10 -- Sum all prime below 2000000

 

let isprimeSqrt n =

    let rec check i =

        i > int64(sqrt(double(n))) || (n % i <> 0L && check (i + 1L))

    check 2L    

 

let rec getNextPrimeSqrt (i:int64) (isPrm:bool) =

    if  isPrm then

        i

    else

        let x =  isprimeSqrt (i + 1L)

        getNextPrimeSqrt (i + 1L) x

 

let getSumOfPrimeBelow value =

  let rec SumOfPrimes lastPrime sum =

       match lastPrime with

        | x when x > value -> sum

        | _ -> let newSum = sum + lastPrime

               let nextPrime = getNextPrimeSqrt lastPrime false

               //printf "%d %d \n" newSum nextPrime

               SumOfPrimes nextPrime newSum

  SumOfPrimes 2L 0L

 

// Problem 10 -- Sum all prime below 2000000

 

// Problem 11 -- greatest product of adjacent number in any direction from 20 X 20 grid


let temp = [|[|1;2;3;4;5;|];[|2;3;4;5;|]|]

printf "%d %d %d %d " temp.[1].[0] temp.[1].[1] temp.[1].[2] temp.[1].[3]

 

let grid = [|

            [|08;02;22;97;38;15;00;40;00;75;04;05;07;78;52;12;50;77;91;08|];

            [|49;49;99;40;17;81;18;57;60;87;17;40;98;43;69;48;04;56;62;00|];

            [|81;49;31;73;55;79;14;29;93;71;40;67;53;88;30;03;49;13;36;65|];

            [|52;70;95;23;04;60;11;42;69;24;68;56;01;32;56;71;37;02;36;91|];

            [|22;31;16;71;51;67;63;89;41;92;36;54;22;40;40;28;66;33;13;80|];

            [|24;47;32;60;99;03;45;02;44;75;33;53;78;36;84;20;35;17;12;50|];

            [|32;98;81;28;64;23;67;10;26;38;40;67;59;54;70;66;18;38;64;70|];

            [|67;26;20;68;02;62;12;20;95;63;94;39;63;08;40;91;66;49;94;21|];

            [|24;55;58;05;66;73;99;26;97;17;78;78;96;83;14;88;34;89;63;72|];

            [|21;36;23;09;75;00;76;44;20;45;35;14;00;61;33;97;34;31;33;95|];

            [|78;17;53;28;22;75;31;67;15;94;03;80;04;62;16;14;09;53;56;92|];

            [|16;39;05;42;96;35;31;47;55;58;88;24;00;17;54;24;36;29;85;57|];

            [|86;56;00;48;35;71;89;07;05;44;44;37;44;60;21;58;51;54;17;58|];

            [|19;80;81;68;05;94;47;69;28;73;92;13;86;52;17;77;04;89;55;40|];

            [|04;52;08;83;97;35;99;16;07;97;57;32;16;26;26;79;33;27;98;66|];

            [|88;36;68;87;57;62;20;72;03;46;33;67;46;55;12;32;63;93;53;69|];

            [|04;42;16;73;38;25;39;11;24;94;72;18;08;46;29;32;40;62;76;36|];

            [|20;69;36;41;72;30;23;88;34;62;99;69;82;67;59;85;74;04;36;16|];

            [|20;73;35;29;78;31;90;01;74;31;49;71;48;86;81;16;23;57;05;54|];

            [|01;70;54;71;83;51;54;69;16;92;33;48;61;43;52;01;89;19;67;48|];|]

 

let greatestProductOfAdjacentNums (x:int[][]) =

    let rec getProducts maxPrd row column =

                 

          match row, column with

          | a , b when a = 17 && b = 17 -> maxPrd

          | _ -> 

                

                // first row left to right 

                let p1 = if column = 17 || row = 17 then 0 else x.[row].[column] * x.[row].[column + 1] * x.[row].[column + 2] * x.[row]..[column + 3]

        

                // first column top to bottom 

                let p2 = if column = 17 || row = 17 then 0 else x.[row].[column] * x.[row + 1].[column] * x.[row + 2].[column] * x.[row + 3].[column]

 

                // left to right diagonally from top

                let p3 = if column = 17 || row = 17 then 0 else x.[row].[column] * x.[row + 1].[column + 1] * x.[row + 2].[column + 2] * x.[row + 3].[column + 3]

 

                // left to right diagonally from bottom

                let p4 = if column = 17 || row = 17 then 0 else x.[row + 3].[column] * x.[row + 2].[column + 1] * x.[row + 1].[column + 2] * x.[row].[column + 3]

  

                // last row top to bottom      

                let p5 = if column > 13 then 

                             if column = 17 || row = 17 then 0 

                             else x.[row ].[column + 3] * x.[row + 1].[column + 3] * x.[row + 2].[column + 3] * x.[row + 3].[column + 3] 

                         else 0

        

                // bottom row left to right

                let p6 = if row > 13 then 

                                if column = 17 || row = 17 then 0 

                                else x.[row + 3].[column] * x.[row + 3].[column + 1] * x.[row + 3].[column + 2] * x..[row + 3].[column + 3] 

                         else 0

                

                let p1p2 = if p1 > p2 then p1 else p2

                let p3p4 = if p3 > p4 then p3 else p4

                let p5p6 = if p5 > p6 then p5 else p6

                let p7 = if p1p2 > p3p4 then p1p2 else p3p4

                let p8 = if p7 > p5p6 then p7 else p5p6

                let newMaxPrd = if p8 > maxPrd then p8 else maxPrd

                  

                let newColumn = if column = 17 then 0 else column + 1

                let newRow = if column = 17 then  row + 1 else row

                //printf "%d %d  %d \n" newRow newColumn newMaxPrd

                getProducts newMaxPrd newRow newColumn

 

    getProducts 0 0 0

 

// Problem 11 -- greatest product of adjacent number in any direction from 20 X 20 grid

 

// Problem 12 -- first triangle number to have over five hundred divisors

 

let getNextTriangleNumber tnum = 

    (tnum * (tnum + 1L) ) / 2L

 

let getTriangleNumberDivisorCount tNum =

    let rec getDivisorsCount count  num =

        match num with 

         | x when x = tNum -> count

         | _ -> let newCount = if tNum % num = 0L then count + 1L else count

                getDivisorsCount newCount (num + 1L)

    getDivisorsCount 1L 1L

    

let getTriangleNumberDivisorCountOver tCount =

    let rec getDivisorsCount num pNum result =

        match result with

         | x when x >= tCount -> pNum

         | _ -> let newNum = num + 1L

                let tNum = getNextTriangleNumber newNum

                let result = getTriangleNumberDivisorCount tNum

                if result > 100L then printf "%d %d \n" tNum result else ()

                getDivisorsCount newNum tNum result

    getDivisorsCount 10000L 1L 1L

 

 

// Problem 12 -- first triangle number to have over five hundred divisors

 

// Problem 13 -- Get First 10 digits of sum of the following one-hundred 50 digit numbers

 

let data = [|

            37107287533902102798797998220837590246510135740250I;

            46376937677490009712648124896970078050417018260538I;

            74324986199524741059474233309513058123726617309629I;

            91942213363574161572522430563301811072406154908250I;

            23067588207539346171171980310421047513778063246676I;

            89261670696623633820136378418383684178734361726757I;

            28112879812849979408065481931592621691275889832738I;

            44274228917432520321923589422876796487670272189318I;

            47451445736001306439091167216856844588711603153276I;

            70386486105843025439939619828917593665686757934951I;

            62176457141856560629502157223196586755079324193331I;

            64906352462741904929101432445813822663347944758178I;

            92575867718337217661963751590579239728245598838407I;

            58203565325359399008402633568948830189458628227828I;

            80181199384826282014278194139940567587151170094390I;

            35398664372827112653829987240784473053190104293586I;

            86515506006295864861532075273371959191420517255829I;

            71693888707715466499115593487603532921714970056938I;

            54370070576826684624621495650076471787294438377604I;

            53282654108756828443191190634694037855217779295145I;

            36123272525000296071075082563815656710885258350721I;

            45876576172410976447339110607218265236877223636045I;

            17423706905851860660448207621209813287860733969412I;

            81142660418086830619328460811191061556940512689692I;

            51934325451728388641918047049293215058642563049483I;

            62467221648435076201727918039944693004732956340691I;

            15732444386908125794514089057706229429197107928209I;

            55037687525678773091862540744969844508330393682126I;

            18336384825330154686196124348767681297534375946515I;

            80386287592878490201521685554828717201219257766954I;

            78182833757993103614740356856449095527097864797581I;

            16726320100436897842553539920931837441497806860984I;

            48403098129077791799088218795327364475675590848030I;

            87086987551392711854517078544161852424320693150332I;

            59959406895756536782107074926966537676326235447210I;

            69793950679652694742597709739166693763042633987085I;

            41052684708299085211399427365734116182760315001271I;

            65378607361501080857009149939512557028198746004375I;

            35829035317434717326932123578154982629742552737307I;

            94953759765105305946966067683156574377167401875275I;

            88902802571733229619176668713819931811048770190271I;

            25267680276078003013678680992525463401061632866526I;

            36270218540497705585629946580636237993140746255962I;

            24074486908231174977792365466257246923322810917141I;

            91430288197103288597806669760892938638285025333403I;

            34413065578016127815921815005561868836468420090470I;

            23053081172816430487623791969842487255036638784583I;

            11487696932154902810424020138335124462181441773470I;

            63783299490636259666498587618221225225512486764533I;

            67720186971698544312419572409913959008952310058822I;

            95548255300263520781532296796249481641953868218774I;

            76085327132285723110424803456124867697064507995236I;

            37774242535411291684276865538926205024910326572967I;

            23701913275725675285653248258265463092207058596522I;

            29798860272258331913126375147341994889534765745501I;

            18495701454879288984856827726077713721403798879715I;

            38298203783031473527721580348144513491373226651381I;

            34829543829199918180278916522431027392251122869539I;

            40957953066405232632538044100059654939159879593635I;

            29746152185502371307642255121183693803580388584903I;

            41698116222072977186158236678424689157993532961922I;

            62467957194401269043877107275048102390895523597457I;

            23189706772547915061505504953922979530901129967519I;

            86188088225875314529584099251203829009407770775672I;

            11306739708304724483816533873502340845647058077308I;

            82959174767140363198008187129011875491310547126581I;

            97623331044818386269515456334926366572897563400500I;

            42846280183517070527831839425882145521227251250327I;

            55121603546981200581762165212827652751691296897789I;

            32238195734329339946437501907836945765883352399886I;

            75506164965184775180738168837861091527357929701337I;

            62177842752192623401942399639168044983993173312731I;

            32924185707147349566916674687634660915035914677504I;

            99518671430235219628894890102423325116913619626622I;

            73267460800591547471830798392868535206946944540724I;

            76841822524674417161514036427982273348055556214818I;

            97142617910342598647204516893989422179826088076852I;

            87783646182799346313767754307809363333018982642090I;

            10848802521674670883215120185883543223812876952786I;

            71329612474782464538636993009049310363619763878039I;

            62184073572399794223406235393808339651327408011116I;

            66627891981488087797941876876144230030984490851411I;

            60661826293682836764744779239180335110989069790714I;

            85786944089552990653640447425576083659976645795096I;

            66024396409905389607120198219976047599490197230297I;

            64913982680032973156037120041377903785566085089252I;

            16730939319872750275468906903707539413042652315011I;

            94809377245048795150954100921645863754710598436791I;

            78639167021187492431995700641917969777599028300699I;

            15368713711936614952811305876380278410754449733078I;

            40789923115535562561142322423255033685442488917353I;

            44889911501440648020369068063960672322193204149535I;

            41503128880339536053299340368006977710650566631954I;

            81234880673210146739058568557934581403627822703280I;

            82616570773948327592232845941706525094512325230608I;

            22918802058777319719839450180888072429661980811197I;

            77158542502016545090413245809786882778948721859617I;

            72107838435069186155435662884062257473692284509516I;

            20849603980134001723930671666823555245252804609722I;

            53503534226472524250874054075591789781264330331690I|]

    

let result = data |> Array.sum

printf "the result is %A " result

 

 

// Problem 13 -- Get first 10 digits of sum of the above one-hundred 50 digits numbers

 

// Problem 14 -- Produce Longest chain from starting number less then one million using following fomula n -> n/2 for positive n -> 3n + 1 for negative

 

 

 

let getLargestSeqCountUnder value =

   let rec getSeq num count =

    match num with

     | x when x = 1L -> count

     | _ -> let newCount = count + 1L

            let nextNum = if num % 2L = 0L then num /2L else (3L * num) + 1L

            //printf " From GetSeq %d %d \n" newCount nextNum

            getSeq nextNum newCount

 

   let rec getSeqcountInter num maxCount maxTNum =

    match num with

     | x when x >= value -> maxTNum 

     | _ -> let newNum = num + 1L

            let count = getSeq newNum 1L

            

            let newCount = if count > maxCount then count else maxCount

            let newTNum = if newCount <> maxCount then newNum else maxTNum

 

            //printf " From GetMaxCount %d %d \n" newCount newNum

            getSeqcountInter newNum newCount newTNum

 

   getSeqcountInter 500000L 1L 1L

 

// Problem 14 -- Produce Longest chain from starting number less then one million

 

// Problem 15 -- How many routes are there in the 20 X 20 grid

let getFactorial num =
    let rec getFacto n result =
        match n with
         | x when x = 1I -> result
         | _ -> let newN = n - 1I
                let newResult = result * newN
                getFacto  newN newResult
    getFacto (num + 1I) 1I

let getPossibleRouteOfSqGrid num =
   getFactorial (2I * num) / ( getFactorial num * getFactorial num)

let printAllPossibleRoute num =
   let endX = num
   let endY = num
   let rec printAPoint (currentP: int * int) (result:string) =
      match currentP with
       | x,y when x = endX && y = endY ->  printf "%s \n" result
       | x,y when x = endX && y < endY ->  printAPoint (x, y + 1) (result + "D")                                           
       | x,y when x < endX && y = endY ->  printAPoint (x + 1, y) (result + "R")
       | x,y -> printAPoint (x + 1, y) (result + "R")
                printAPoint (x, y + 1) (result + "D")
   printAPoint (0 , 0)  ""

// Problem 15 -- How many routes are there in the 20 X 20 grid

// Problem 16 -- the sum of the digit of result of 2^1000

let pow baseX times =
  let rec powInt count result =
     match count with 
      | x when x = times -> result
      | _ -> powInt (count + 1I) (result * baseX)
  powInt 0I 1I

let getSumOfDigitOfPow baseX times =
   let result = pow baseX times
   let sumPow = string(result)
                  
                  |> Seq.fold( fun sum elm -> 
                                  
                                  let intVal = System.Convert.ToInt32(elm.ToString())
                                  printf "%c %d %d \n" elm sum intVal
                                  intVal + sum ) 0
   sumPow

let x = System.Convert.ToInt32("32")
let y = System.Convert.ToInt32('3')
// Problem 16 -- the sum of the digit of result of 2^1000


// Problem 17 -- how many letters would be used if we write 1 to 1000 in word
let rec getLength = function
    | x when x = 1 || x = 2 || x = 6  || x = 10 -> 3
    | x when x = 3 || x = 7 || x = 8 -> 5
    | x when x = 4 || x = 5 || x = 9 -> 4
    | x when x = 11 || x = 12  -> 6
    | x when x = 13 || x = 14 || x = 18 || x = 19 -> 8
    | x when x = 15 || x = 16 -> 7
    | x when x = 17 -> 9
    | x when x = 20 || x = 30 || x = 80 || x = 90 -> 6
    | x when x = 40 || x = 50  || x = 60 -> 5
    | x when x = 70 -> 7
    | x when x = 1000 -> 11
    | x when x < 100 -> let rem = x % 10 
                        let co = (x / 10) * 10 
                        getLength co + getLength rem
    | x when x % 100 = 0 -> let co = x / 100
                            getLength co + 7
    | x -> let rem = x % 100
           let co = x / 100 
           getLength co + 10 + getLength rem

let sumLength = {1 .. 1000}
                  |> Seq.map (fun x -> getLength x)
                  |> Seq.sum

 // Problem 17 -- how many letters would be used if we write 1 to 1000 in word

// Problem 18 -- find maximum total from top to bottom of a triangle
let triangle a =[|[|3|];[|7; 4;|];[|2; 4; 6;|];[|8; 5; 9; 3;|];|]



// Problem 18 -- find maximum total from top to bottom of a triangle
