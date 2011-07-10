module Problem9
open System

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
