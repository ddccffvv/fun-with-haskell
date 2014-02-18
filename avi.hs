import Data.Monoid
import Data.List.Split


-- summing

sum_data =  [AdditionTest c | c <- [1..]]

data AdditionTest = AdditionTest Int deriving (Show)

instance Monoid AdditionTest where
    mempty = AdditionTest 0
    mappend = addTest

addTest :: AdditionTest -> AdditionTest -> AdditionTest
addTest (AdditionTest x) (AdditionTest y) = AdditionTest (x+y)

aggregate x = foldl mappend mempty x

sum_result = foldl mappend mempty $ take 100 sum_data
-- doesn't matter if left or right...
sum_result' = foldr mappend mempty $ take 100 sum_data
-- or even different chunks and then combine the chunks...
chunks = chunksOf 10 $ take 100 sum_data
sum_result'' = foldr mappend mempty $ map (foldr mappend mempty) chunks

-- generalising te result
sum_result''' = aggregate $ take 100 sum_data

-- Now, how can we use this to "aggregate" strings?

string_data = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"]
data StringAddition = StringAddition String deriving (Show)

instance Monoid StringAddition where
    mempty = StringAddition ""
    mappend = stringAppend

stringAppend (StringAddition x) (StringAddition y) = StringAddition (x ++ y)

sum_string = aggregate string_data


-- well, that was easy...
-- How about finding a maximum?
-- Apropos, does anybody know how to define a Monoid on all classes that derive Ordering? There is probably a problem with chosing a sane mempty, because ord doesn't seem to have that?
-- Apropos2, is it possible to define 2 monoids for the same data type? For example: an Int monoid for sum and product

data MaxTest = MaxTest Int deriving (Show)
max_data =  [MaxTest c | c <- [1..]]

instance Monoid MaxTest where
    mempty = MaxTest 0
    mappend = maxTest

maxTest (MaxTest x) (MaxTest y) = MaxTest $ max x y

max_result = aggregate $take 100 max_data

-- averages?

-- This is a sum and a number of occurences
data AverageData = AverageData { sum:: Int, occurences :: Int }  deriving (Show)

average_data = [(AverageData 2 3), (AverageData 2 5)]

instance Monoid AverageData where
    mempty = AverageData 0 0
    mappend = averageAggregate

averageAggregate :: AverageData -> AverageData -> AverageData
averageAggregate (AverageData a b) (AverageData x y) = AverageData (a+x) (b+y)

average_result = aggregate average_data
