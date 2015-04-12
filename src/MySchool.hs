module MySchool where

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
removeNonUppercase :: [Char] -> [Char]

main = do
    print (removeNonUppercase "uwaga uwaga JEDZIE")
