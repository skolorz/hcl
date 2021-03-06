module Roman where

digits = [('M',1000),('D',500),('C',100),('L',50),('X',10),('V',5),('I',1)]
matchDigit :: Int -> (Char,Int)
possibleDigits n = [(d,v) | (d,v) <- digits,  v <= n]
matchDigit n = head (possibleDigits n) 
--prevDigit n | n < 5 = ("I",1)
prevDigit n =  head (drop 1 (possibleDigits n ))
isReducer d = elem d "IXC"
rval x = head [v | (d,v) <- digits, x == d] 

romanNumber :: Int -> String
romanNumber 0 = ""
romanNumber n 
    | isReducer d && n >= (4 * v) && n < (5 * v)  =   d : romanNumber(n+v)
    where  (d,v) = matchDigit n
romanNumber n 
    | n >=9 && n >= (9 * v) && n < (10 * v) && isReducer d =   d : romanNumber(n+v)
    where  (d,v) = prevDigit n
romanNumber n = let (d,v) = matchDigit n in d:romanNumber (n-v) 

readRoman :: String -> Int
readRoman [] = 0
readRoman (r:d:xs)
        | isReducer r && reducer < digit = digit - reducer + readRoman xs
        where (reducer,digit) = (rval r, rval d)
readRoman (x:xs) = rval x + readRoman xs

good :: [(Int, String)]
good = [
    (1, "I"),
    (2, "II"),
    (3, "III"),
    (4, "IV"),
    (5, "V"),
    (6, "VI"),
    (7, "VII"),
    (8, "VIII"),
    (9, "IX"),
    (10, "X"),
    (11, "XI"),
    (12, "XII"),
    (13, "XIII"),
    (14, "XIV"),
    (15, "XV"),
    (16, "XVI"),
    (17, "XVII"),
    (18, "XVIII"),
    (19, "XIX"),
    (20, "XX"),
    (30, "XXX"),
    (40, "XL"),
    (42, "XLII"),
    (50, "L"),
    (69, "LXIX"),
    (90, "XC"),
    (301, "CCCI"),
    (423, "CDXXIII"),
    (500, "D"),
    (900, "CM"),
    (1000, "M"),
    (1900, "MCM"),
    (1990, "MCMXC"),
    (2008, "MMVIII"),
    (3297, "MMMCCXCVII"),
    (3999, "MMMCMXCIX")
    ]
    
testRoman :: [(Int, String, String)]
testRoman = take 100 [(d,r,result) | (d,r) <- good, let result = romanNumber d, r /= result]

testReadRoman :: [(Int, String, Int)]
testReadRoman = take 100 [(d,r,result) | d <- [1..3900], let r = romanNumber d, let result = readRoman r, d /= result]
testDigitValue = [(d,v,r) | (d,v) <- digits, let r = romanNumber v, [d] /= r]
test = (testReadRoman,testRoman,testDigitValue)
main = do
    print (romanNumber 1)
