-- {-# LANGUAGE MultiParamTypeClasses #-}

module TestBinaryNumber where

import BinaryNumber
import TestPP

-- lossy equality testing for BinaryNumber
(~~) :: BinaryNumber -> BinaryNumber -> Bool
x ~~ y = smallBits x == smallBits y
  where
    -- only compare the first 64 bits
    smallBits = take 64 . (++ repeat 0)

-- lossy equality testing for lists of BinaryNumbers
(~*~) :: [BinaryNumber] -> [BinaryNumber] -> Bool
xs ~*~ ys = and $ take 32 $ zipWith (~~) xs ys

testToDecimal :: TestData
testToDecimal =
    tests 1 20
        [ testVal "toDecimal 1" 0 (toDecimal [])
        , testVal "toDecimal 2" 0 (toDecimal [0])
        , testVal "toDecimal 3" 0 (toDecimal [0, 0, 0])
        , testVal "toDecimal 4" 1 (toDecimal [1])
        , testVal "toDecimal 5" 1 (toDecimal [1, 0])
        , testVal "toDecimal 6" 2 (toDecimal [0, 1, 0, 0])
        , testVal "toDecimal 7" 3 (toDecimal [1, 1])
        , testVal "toDecimal 8" 1023 (toDecimal [1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
        , testVal "toDecimal 9" 1024 (toDecimal [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1])
        , testVal "toDecimal 10" 9999 (toDecimal [1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0])
        ]

testToBinary :: TestData
testToBinary =
    tests 2 20
        [ testCond "toBinary 1" $ [0] ~~ toBinary 0
        , testCond "toBinary 2" $ [1] ~~ toBinary 1
        , testCond "toBinary 3" $ [0, 1] ~~ toBinary 2
        , testCond "toBinary 4" $ [1, 1, 1, 1, 1] ~~ toBinary 31
        , testCond "toBinary 5" $ [0, 0, 0, 0, 0, 1] ~~ toBinary 32
        , testCond "toBinary 6" $ replicate 10 1 == take 10 (toBinary 1023)
        , testCond "toBinary 7" $ replicate 10 1 ++ replicate 5 0 == take 15 (toBinary 1023)
        , testCond "toBinary 8" $ [0, 1] == take 2 (toBinary 2730)
        , testCond "toBinary 9" $ [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1] == take 12 (toBinary 2730)
        , testCond "toBinary 10" $ [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0] == take 15 (toBinary 2730)
        ]

testInc :: TestData
testInc =
    tests 3 10
        [ testCond "inc 1" $ inc (toBinary 0) ~~ toBinary 1
        , testCond "inc 2" $ inc (toBinary 1) ~~ toBinary 2
        , testCond "inc 3" $ inc (toBinary 2) ~~ toBinary 3
        , testCond "inc 4" $ inc (toBinary 63) ~~ toBinary 64
        , testCond "inc 5" $ inc (toBinary 64) ~~ toBinary 65
        , testCond "inc 6" $ inc (toBinary 119) ~~ toBinary 120
        , testCond "inc 7" $ inc (toBinary 511) ~~ toBinary 512
        , testCond "inc 8" $ inc (toBinary 512) ~~ toBinary 513
        , testCond "inc 9" $ inc (toBinary 999) ~~ toBinary 1000
        , testCond "inc 10" $ inc (toBinary 1000) ~~ toBinary 1001
        ]

testDec :: TestData
testDec =
    tests 4 10
        [ testCond "dec 1" $ dec (toBinary 1) ~~ toBinary 0
        , testCond "dec 2" $ dec (toBinary 2) ~~ toBinary 1
        , testCond "dec 3" $ dec (toBinary 3) ~~ toBinary 2
        , testCond "dec 4" $ dec (toBinary 64) ~~ toBinary 63
        , testCond "dec 5" $ dec (toBinary 65) ~~ toBinary 64
        , testCond "dec 6" $ dec (toBinary 120) ~~ toBinary 119 
        , testCond "dec 7" $ dec (toBinary 512) ~~ toBinary 511 
        , testCond "dec 8" $ dec (toBinary 513) ~~ toBinary 512 
        , testCond "dec 9" $ dec (toBinary 1000) ~~ toBinary 999 
        , testCond "dec 10" $ dec (toBinary 1001) ~~ toBinary 1000 
        ]

testAdd :: TestData
testAdd =
    tests 5 20
        [ testCond "add 1" $ add (toBinary 0) (toBinary 0) ~~ toBinary 0
        , testCond "add 2" $ add (toBinary 0) (toBinary 1) ~~ toBinary 1
        , testCond "add 3" $ add (toBinary 1) (toBinary 0) ~~ toBinary 1
        , testCond "add 4" $ add (toBinary 1) (toBinary 1) ~~ toBinary 2
        , testCond "add 5" $ add (toBinary 1) (toBinary 2) ~~ toBinary 3
        , testCond "add 6" $ add (toBinary 2) (toBinary 1) ~~ toBinary 3
        , testCond "add 7" $ add (toBinary 64) (toBinary 64) ~~ toBinary 128
        , testCond "add 8" $ add (toBinary 127) (toBinary 127) ~~ toBinary 254
        , testCond "add 9" $ add (toBinary 127) (toBinary 128) ~~ toBinary 255
        , testCond "add 10" $ add (toBinary 128) (toBinary 128) ~~ toBinary 256
        ]

testStack :: TestData
testStack =
    tests 6 20
        [ testCond "stack 1" $
            [[0, 1, 1, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 1, 0]] == take 3 (map (take 6) $ stack (toBinary 6) (toBinary 5))
        , testCond "stack 2" $
            [[1, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0]] == take 3 (map (take 6) $ stack (toBinary 1) (toBinary 7))
        , testCond "stack 3" $
            [[0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0]] == take 3 (map (take 6) $ stack (toBinary 63) (toBinary 0))
        , testCond "stack 4" $ stack4 !! 1 ~~ tail (stack4 !! 2) && stack4 !! 2 ~~ tail (stack4 !! 3)
        , testCond "stack 5" $ and $ take 5 $ zipWith (~~) stack5 (map tail $ tail stack5)
        ]
  where
    stack4 = stack (toBinary 25) (toBinary 31)
    stack5 = stack (toBinary 100) (toBinary 1023)

testMultiply :: TestData
testMultiply =
    tests 7 20
        [ testCond "multiply 1" $ multiply (toBinary 0) (toBinary 0) ~*~ [toBinary 0]
        , testCond "multiply 2" $ multiply (toBinary 1) (toBinary 0) ~*~ [toBinary 0]
        , testCond "multiply 3" $ multiply (toBinary 0) (toBinary 1) ~*~ [toBinary 0]
        , testCond "multiply 4" $ multiply (toBinary 1) (toBinary 1) ~*~ (toBinary <$> [0, 1])
        , testCond "multiply 5" $ multiply (toBinary 2) (toBinary 3) ~*~ (toBinary <$> [0, 2, 6])
        , testCond "multiply 6" $ multiply (toBinary 15) (toBinary 15) ~*~ (toBinary <$> [0, 15, 45, 105, 225])
        , testCond "multiply 7" $ multiply (toBinary 50) (toBinary 16) ~*~ (toBinary <$> [0, 0, 0])
        , testCond "multiply 8" $ multiply (toBinary 50) (toBinary 17) ~*~ (toBinary <$> [0, 50, 50])
        , testCond "multiply 9" $ multiply (toBinary 127) (toBinary 127) ~*~ (toBinary <$> [0, 127, 381])
        , testCond "multiply 10" $ multiply (toBinary 1000) (toBinary 21) ~*~ (toBinary <$> [0, 1000, 1000, 5000, 5000, 21000])
        ]

main :: IO ()
main =
    vmCheck
        [ testToDecimal
        , testToBinary
        , testInc
        , testDec
        , testAdd
        , testStack
        , testMultiply
        ]
