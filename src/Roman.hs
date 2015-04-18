module Roman where
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
    (900, "CM"),
    (1900, "MCM"),
    (1990, "MCMXC"),
    (2008, "MMVIII"),
    (3297, "MMMCCXCVII"),
    (3999, "MMMCMXCIX")
    ]
test = [(x,romanNumber (fst x))| x <- good, snd x /= romanNumber (fst x)]

romanNumber :: (Integral a) => a -> String
main = do
    print (test)
