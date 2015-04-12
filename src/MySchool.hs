module MySchool where

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
removeNonUppercase :: [Char] -> [Char]
<<<<<<< HEAD
removeNonUppercase str = [c | c <- str, c elem [A..Z] ]
=======

>>>>>>> 2296f2434312c4f0fa53a7a089e5332bd18d512c
main = do
    print (removeNonUppercase "uwaga uwaga JEDZIE")
