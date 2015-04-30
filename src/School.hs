module School where
appltyTwice :: (a -> a) -> a -> a
appltyTwice f x = f (f x)

afilter :: (a -> Bool) -> [a] -> [a]
afilter _ [] = []
afilter f (x:xs)
    | f x = x : afilter f xs
    | otherwise = afilter f xs
takew :: (a -> Bool) -> [a] -> [a]
takew _ [] = []
takew p (x:xs) 
             | p x = x : takew p xs
             | otherwise = []

foold :: (a -> a -> a) -> [a] -> a -> a
foold _ [] ac = ac
foold f (x:xs) ac = foold f xs (f x ac)

