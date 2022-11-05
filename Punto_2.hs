--Realizar una versión modificada de la RPN con los siguientes operadores.
--Nota: tiene que ser compatible con los operadores aritméticos básicos

import Data.List  
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction (x:xs) "neg" = negate x:xs
            foldingFunction (x:xs) "raiz2" = sqrt x:xs
            foldingFunction (x:xs) "condnumero" = condnumero x:xs
            foldingFunction (x:y:ys) "sum" = sum (x:y:ys):ys
            foldingFunction (x:y:ys) "producto" = product (x:y:ys):ys
            foldingFunction xs "promedio" = (sum xs / fromIntegral(length xs)):xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
condnumero:: Float -> Float
condnumero num
    | num == 3 = 100
    | num == 5 = 25
    | otherwise = 0