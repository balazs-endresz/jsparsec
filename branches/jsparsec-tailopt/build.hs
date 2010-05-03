import Prelude hiding (readFile, writeFile)
import System.IO.UTF8
import System.FilePath ((</>))
--import System.Environment (getArgs)

main =
  do
    let paths = "license.txt" : map ("src" </>) files
    contents <- mapM readFile paths
    writeFile "jsparsec.js" $ concat $ map (++ "\r\n\r\n") contents 

files = ["Main.js"
        ,"Haskell.js"
        ,"Prim.js"
        ,"Char.js"
        ,"Combinator.js"
        ,"Token.js"
--        ,"Language.js"
        ]