import Prelude hiding (readFile, writeFile)
import System.IO.UTF8
import System.FilePath ((</>))
--import System.Environment (getArgs)

main =
  do
    let paths = "license.txt" : map ("src" </>) files
    contents' <- mapM readFile paths
    let contents = concat $ map (++ (nl ++ nl)) contents'
    writeFile "jsparsec-javascript.js" $ wrap contents

files = ["import.js"
        ,"Syntax.js"
        ,"Lexer.js"
        ,"Parser.js"
--        ,"PrettyPrint.js"
        ]

wrap = id
--wrap str = ";(function(){" ++ nl ++ str ++ nl ++ "})();"

nl = "\r\n"