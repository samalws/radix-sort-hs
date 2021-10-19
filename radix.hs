import Data.Ord

countingSort :: [(a,Bool)] -> [a]
countingSort = helper [] [] where
  helper lf lt [] = reverse lf <> reverse lt
  helper lf lt ((a, False) : b) = helper (a:lf) lt b
  helper lf lt ((a, True)  : b) = helper lf (a:lt) b

radixSort :: [(a,[Bool])] -> [a]
radixSort [] = []
radixSort l@((_,[]):_) = fst <$> l
radixSort l = radixSort $ countingSort $ f <$> l where
  f (a,b) = ((a, tail b), head b)

numToBits :: Integer -> [Bool]
numToBits 0 = []
numToBits 1 = [True]
numToBits n = (n `mod` 2 == 1):(numToBits $ n `div` 2)

numsToBits :: [Integer] -> [[Bool]]
numsToBits = toBools . (numToBits <$>) . addMin where
  addMin l = subtract (minimum (0:l)) <$> l
  toBools l = take (maxLength l) . (<> repeat False) <$> l
  maxLength = maximum . (0:) . (length <$>)

numberedsToBits :: [(a,Integer)] -> [(a,[Bool])]
numberedsToBits l = zip (fst <$> l) $ numsToBits $ snd <$> l

radixSortWithMetric :: (a -> Integer) -> [a] -> [a]
radixSortWithMetric f l = radixSort $ numberedsToBits $ g <$> l where
  g a = (a, f a)

testList = [0..20]
testMetric x = x*(20-x)
main = print $ radixSortWithMetric testMetric testList
