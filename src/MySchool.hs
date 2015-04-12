module MySchool where

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [c | c <- str, elem c ['A'..'Z'] ]

main = do
    print (removeNonUppercase "uwaga uwaga JEDZIE")
