-- Hello world, doing io
helloWorld :: IO ()
helloWorld = print "Hello"

-- Hello name, taking string and doing io
helloName :: String -> IO ()
helloName x = print ("Hello " ++ x)

-- function that returns the last element of a list
lastElement :: [a] -> a
lastElement [x] = x
lastElement (x:xs) = lastElement xs

-- function that takes a list of lists and returns a list 
flatten :: [[a]] -> [a]
flatten [[]] = []
flatten [[x]] = [x]
flatten (x:xs) = x ++ (flatten [xs])
