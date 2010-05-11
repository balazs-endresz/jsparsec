import Prelude hiding (readFile, writeFile)
import System.IO.UTF8
import System.FilePath ((</>))
--import System.Environment (getArgs)

main =
  do
    let paths = "license.txt" : map ("src" </>) files
    contents' <- mapM readFile paths
    let contents = concat $ map (++ (nl ++ nl)) contents'
    writeFile "jsparsec.js" $ wrap contents

files = ["Main.js"
        ,"Haskell.js"
        ,"Prim.js"
        ,"Char.js"
        ,"Combinator.js"
        ,"Token.js"
        ,"Language.js"
        ,"Expr.js"
        ]

wrap str = ";(function(){" ++ nl ++ str ++ nl ++ "})();"

nl = "\r\n"