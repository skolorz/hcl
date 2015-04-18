module Roman where

romanNumber :: Int -> String
romanNumber n
    | n == 0 = ""
    | n >= 1000 = 'M' : romanNumber (n-1000)
    | n >= 900 = 'C' : romanNumber (n+100)
    | n >= 500 = 'D' : romanNumber (n-500)
    | n >= 400 = 'C' : romanNumber (n+100)
    | n >= 100 = 'C' : romanNumber (n-100)
    | n >= 90 = 'X' : romanNumber (n+10)
    | n >= 50 = 'L' : romanNumber (n-50)
    | n >= 40 = 'X' : romanNumber (n+10)
    | n >= 10 = 'X' : romanNumber (n-10)
    | n >= 9  = 'I' : romanNumber (n+1)
    | n >= 5 = 'V' : romanNumber (n-5)
    | n >= 4  = 'I' : romanNumber (n+1)
    | otherwise = 'I' : romanNumber (n-1) 

digits = [('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)]
isReducer d = elem d "IXC"
rval x = head [v | (d,v) <- digits, x == d] 

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
testRoman = [(d,r,result) | (d,r) <- good, let result = romanNumber d, r /= result]

testReadRoman :: [(Int, String, Int)]
testReadRoman = [(d,r,result) | d <- [1..10000], let r = romanNumber d, let result = readRoman r, d /= result]
testDigitVaule = [(d,v,r) | (d,v) <- digits, let r = romanNumber v, [d] /= r]
main = do
    print (romanNumber 1)
