import qualified Data.Map.Strict as Map

data Result a = Success a | String
    deriving (Show)

convert :: String -> Result String
convert "" = Success ""
convert x = Success (convertFrom (determineNumberType x) x)

convertFrom :: NumberType -> String -> String
convertFrom Roman = show . sum . applySubtractionRule . mapRomanToValues

mapRomanToValues :: String -> [Int]
mapRomanToValues r = [ v | x@(Just v) <- map romanValue r]

romanValue :: Char -> Maybe Int
romanValue c = Map.lookup c romanDigitValues

applySubtractionRule :: [Int] -> [Int]
applySubtractionRule (x:y:xs) = (if x < y then -x else x) : applySubtractionRule (y:xs)
applySubtractionRule x = x

data NumberType = Roman | Arabic

determineNumberType :: String -> NumberType
determineNumberType number = Roman

isType :: NumberType -> String -> Bool
isType _ [] = True
isType Roman (x:xs) = (Map.lookup x romanDigitValues) /= Nothing && (isType Roman xs)

romanDigitValues = Map.fromList [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 100)]
