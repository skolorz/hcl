module MySchool where

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

main = do
    print "doubleUs"
    print (doubleUs 2 1)
