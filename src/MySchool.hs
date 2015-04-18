module MySchool where

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [c | c <- str, elem c ['A'..'Z'] ]
lucky :: (Integral a) => a -> String
lucky 7 = "You are lucky"
lucky x = "Bad luck" 

main = do
    print (removeNonUppercase "uwaga uwaga JEDZIE")
