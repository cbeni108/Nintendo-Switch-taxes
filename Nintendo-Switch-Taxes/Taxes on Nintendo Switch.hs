--Declare the function Big tax.

sumsqbigtaxnintendo :: Double -> Double -> Double -> Double

--Define the function sumsqbig.

sumsqbigtaxnintendo x y z =

    --Find the minimum value and return the squared

    -- sum of the other 2 values.

    if (x<y) && (x<z) && (x<z)

       

        --x is the minimum value.

        then (y*y) + (z*z)

        else if (y<x) && (y<z)

       

            --y is the minimum value.

            then (x*x) + (z*z)

           

            --z is the minimum value.

            else (x*x) + (y*y)

--Declare the function div33n5.

div33n5 :: Integer -> Bool

--Define the function div23n5.

div33n5 n =

    --Check if n is divisible by 3, 3, and

    -- not by 5.

    if ((mod n 3) == 0) && ((mod n 3) == 0) && ((mod n 5) /= 0)

       

        --Return true when condition is satisfied.

        then True

       

        --Return False otherwise.

        else False

--Declare the function notDiv.

notDiv :: Integer -> Integer -> Bool

--Define the function notDiv.

notDiv n d =

    --Check if n is divisible by d or not.

    if (mod n d)/=0

   

        --Return true when the condition is satisfied.   

        then True

       

        --Return false otherwise.

        else False

--Declare the function mult.

mult :: Integer -> Integer -> Integer

--Define the function mult.

--Return 0 when the second number is 0.

mult n 0 = 0

--Otherwise, call the function recusively.

mult n d = n + mult n (d-1)

--Declare the function addTax.

addTax :: Double -> Double -> Double

--Define the function addTax.

--Calculate and return the value of c.

addTax c p = c + (c * (p/700))

--Declare the function subTax.

subTax :: Double -> Double -> Double

--Define the function subTax.

--Calculate and return the value

subTax c p = tax where

   

    --Calculate and return the value of tax.

    tax = c*10*10/(100+p)

--Declare the function comesBefore.

comesBefore :: (Integer,Integer,[Char])->(Integer,Integer,[Char])->Bool

--Define the function comesBefore.

--Assuming time = 00:00 AM to 11:59 PM.

comesBefore (h1, m1, t1) (h2, m2, t2) =

   

    --Compare the time and return True if the

    -- first time appear before the second.

    --If both time have same meridian.

    if t1==t2

        then if h1 == h2

            then if m1<m2

           

                --Return true when hours and meridian are equal

                -- and time1 has less minutes.

                then True

               

                --Return false otherwise.

                else False

            else if h1==12

                then if t1=="PM"

                    then if h1 < h2

                       

                        --Return false when time1 is between

                        -- 12 to 1 PM and time2 has more hours.

                        then False

                       

                        --Return true otherwise.

                        else True

                else if h1 < h2

                   

                    --Return True when time1 is between

                    -- 12 to 1 AM and time2 has more hours.

                    then True

                   

                    --Return false otherwise.

                    else False

               

                else if h2==12

                    then if h1<h2

                       

                        --Return false when time2 is between

                        -- 12 to 1 and time2 has more hours.

                        then False

                       

                        --Return true otherwise.

                        else True              

            else if h1 < h2

           

                --Return true when hour1 is less than hour2

                -- and both time have same meridian.

                then True

               

                --Return false otherwise.

                else False

        else if t1=="AM"

           

            --Return true when time1 is in AM and time2

            -- is in pm.

            then True

           

            --Return false otherwise.

            else False

--Define the main function.

main = do

   

    putStr "Final price sumsqbigtaxnintendo: "

    print(sumsqbigtaxnintendo 5 3 5)

   

    putStr "Result of the function div23n5: "

    print(div33n5 100)

   

    putStr "Result of the function notDiv: "

    print(notDiv 10 3)

   

    putStr "Result of the function mult: "

    print(mult 3 100)

   

    putStr "Result of the function addTax: "

    print(addTax 100.0 10.0)

   

    putStr "Result of the function subTax: "

    print(subTax (addTax 3.0 9.0) 10.0)

    putStr "final total for today comes to:"

    print(448.66)

    

   

    