module DatabaseDesign.Ampersand_Prototype.Generate (generateAll) where

import DatabaseDesign.Ampersand_Prototype.CoreImporter  
import Prelude hiding (writeFile,readFile,getContents,putStr,putStrLn)
import Data.List
import Data.Maybe
import System.FilePath               
import DatabaseDesign.Ampersand_Prototype.Version 

import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL

generateAll :: Fspc -> Options -> IO ()
generateAll fSpec opts =
 do { verboseLn opts "Experimental Generation"
    ; writePrototypeFile "Interfaces.php" $ generateInterfaces fSpec opts
    }
  where
    writePrototypeFile fname content =
     do { verboseLn opts ("  Generating "++fname)
        --; verboseLn opts $ content
        ; writeFile (combine (dirPrototype opts) fname) content
        }
 
  
generateInterfaces fSpec opts = genPhp "Generate.hs" "Interfaces.php" $
  phpPreliminaries ++
  ["require \"php/DatabaseUtils.php\";"
  , ""
  , "echo '<link href=\"css/Experimental.css\" rel=\"stylesheet\" type=\"text/css\"/>';"
  , ""
  , "$allInterfaceObjects ="
  , "  array" ] ++
    (addToLastLine ";" $ indent 4 $ blockParenthesize $ map (generateInterface fSpec opts) allInterfaces) ++
  [ ""
--  , "printBinaryTable(DB_doquer('"++dbName opts++"', getQueryOverview_as()));"
--  , "print_r( getCoDomainAtoms( 'Hello', '2', getQueryId_notIdentifies() ));"
  , ""
  , "echo generateInterface('"++dbName opts++"', $allInterfaceObjects['Overview'], '1');"
  , "echo generateInterface('"++dbName opts++"', $allInterfaceObjects['Id'], '2');"
  , "echo generateInterface('"++dbName opts++"', $allInterfaceObjects['Th'], 'France');"
  , ""
  ]     
 where allInterfaces = interfaceS fSpec ++ interfaceG fSpec

generateInterface fSpec opts interface =
  [ "// Top-level interface "++name interface ++":"
  , "'"++name interface ++"' => " ] ++
  genInterfaceObjects fSpec opts 1 (ifcObj interface) 
  
-- two arrays: one for the object and one for the list of subinterfaces
genInterfaceObjects :: Fspc -> Options -> Int -> ObjectDef -> [String]
genInterfaceObjects fSpec opts depth object = indent (depth*2) $
  [ "array ( 'name' => '"++name object++"'"
  , "      , 'isUnivalent' => " ++ (phpBool $ isUni (objctx object))
  , "      , 'sqlQuery' => '" ++ (fromMaybe "" $ selectExpr fSpec 25 "src" "tgt" $ objctx object) ++ "'" -- todo give an error for Nothing                                                  
  , "      , 'subInterfaces' =>"
  , "          array"
  ] ++ (indent 10 $ blockParenthesize $ map (genInterfaceObjects fSpec opts $ depth + 1) $ objats object) ++
  [ "      )"
  ]
 
blockParenthesize :: [[String]] -> [String]
blockParenthesize [] = ["()"]
blockParenthesize liness = concat [ zipWith (++) (sep:repeat "  ") (lines::[String]) | (sep, lines) <- zip ("( ":repeat ", ") liness ] ++ [")"]
-- [["line"], ["line1", "line2", "line3"],["linea", "lineb"] ->
-- ( line
-- , line1
--   line2
--   line3
-- , linea
--   lineb
-- )

addToLastLine :: String -> [String] -> [String]
addToLastLine str [] = [str] 
addToLastLine str lines = init lines ++ [last lines ++ str] 
  
toPhp str = map replace str
 where replace ' ' = '_'
       replace c   = c
  
phpBool b = if b then "true" else "false"

-- GenUtil
phpPreliminaries = -- Maybe this will be put in an imported Php module
  [ "error_reporting(E_ALL); "
  , "ini_set(\"display_errors\", 1);"
  ]

-- generatorModule is the Haskell module responsible for generation, makes it easy to track the origin of the php code
genPhp generatorModule moduleName contentLines = unlines $
  [ "<?php"
  , "// module "++moduleName++" generated by "++generatorModule
  , "// "++prototypeVersionStr
  ] ++ replicate 2 "" ++ contentLines ++
  [ "?>"
  ]
  
indent n lines = [ replicate n ' ' ++ line | line <- lines ]
