printHello = print "hello world"

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
