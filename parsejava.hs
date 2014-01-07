import Language.Java.Lexer
import Language.Java.Parser
import Language.Java.Pretty

main = do
    source <- readFile "Main.java"
    print $ lexer source
    print "---------------------"
    print $ parser compilationUnit source
    print "---------------------"
    
    let result = parser compilationUnit source
    case result of
        Left error -> print error
        Right ast -> putStrLn $ prettyPrint ast
