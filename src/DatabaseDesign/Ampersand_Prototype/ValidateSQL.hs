module DatabaseDesign.Ampersand_Prototype.ValidateSQL where

import Data.List
import Data.Maybe
import Control.Monad
import System.Process
import System.IO hiding (hPutStr,hGetContents)
import System.Directory
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Fspec
import DatabaseDesign.Ampersand.ADL1.Rule
import DatabaseDesign.Ampersand_Prototype.CoreImporter hiding (putStr)
import DatabaseDesign.Ampersand_Prototype.RelBinGenBasics
import DatabaseDesign.Ampersand_Prototype.RelBinGenSQL
import DatabaseDesign.Ampersand_Prototype.Installer
import DatabaseDesign.Ampersand_Prototype.Version 
import Control.Exception
import Prelude hiding (catch)

{-
Validate the generated SQL for all rules in the fSpec, by comparing the evaluation results
with the results from Haskell-based Ampersand rule evaluator. The latter is much simpler and
therefore most likely to be correct in case of discrepancies.
-}

fatal :: Int -> String -> a
fatal = fatalMsg "ValidateSQL"

tempDbName :: String
tempDbName = "TemporaryValidationDatabase"

validateRuleSQL :: Fspc -> Options -> IO Bool
validateRuleSQL fSpec opts =
 do { removeTempDatabase opts -- in case it exists when we start, just drop it
    ; hSetBuffering stdout NoBuffering
    
    ; putStrLn "Initializing temporary database"
    ; createTempDatabase fSpec opts
     
    ; let allExps = getAllInterfaceExps fSpec ++ 
                    getAllRuleExps fSpec ++
                    getAllPairViewExps fSpec ++
                    getAllKeyExps fSpec
                    
    ; putStrLn $ "Number of expressions to be validated: "++show (length allExps)
    ; results <- mapM (validateExp fSpec opts) allExps 
                   
    ; putStrLn "\nRemoving temporary database"
    ; removeTempDatabase opts
    
    ; case [ ve | (ve, False) <- results] of
        [] -> do { putStrLn "\nValidation successful.\nWith the provided populations, all generated SQL code has passed validation."
                 ; return True
                 }
        ves -> do { putStrLn $ "\n\nValidation error. The following expressions failed validation:\n" ++
                                (unlines $ map showVExp ves)
                  ; return False
                  } 
    }

-- functions for extracting all expressions from the context

getAllInterfaceExps :: Fspc -> [ValidationExp]
getAllInterfaceExps fSpec = concat [ getObjExps (name ifc) $ ifcObj ifc 
                                   | ifc <- interfaceS fSpec ++ interfaceG fSpec ]
 where getObjExps iName objDef = (objctx objDef, "interface " ++ show iName) :
                                 concatMap (getObjExps iName) (objAts objDef)

-- we check the complement of the rule, since that is the expression evaluated in the prototype
getAllRuleExps :: Fspc -> [ValidationExp]
getAllRuleExps fSpec = map getRuleExp $ vrules fSpec ++ grules fSpec
 where getRuleExp rule = (ECpl . rrexp $ rule, "rule "++show (name rule))
 
getAllPairViewExps :: Fspc -> [ValidationExp]
getAllPairViewExps fSpec = concatMap getPairViewExps $ vrules fSpec ++ grules fSpec
 where getPairViewExps r@Ru{rrviol = Just (PairView pvsegs)} =
         [ (exp, "violation view for rule "++show (name r)) | PairViewExp _ exp <- pvsegs ]
       getPairViewExps _    = []              

getAllKeyExps :: Fspc -> [ValidationExp]
getAllKeyExps fSpec = concatMap getKeyExps $ vkeys fSpec
 where getKeyExps key = [ (objctx objDef, "key "++show (name key)) 
                        | KeyExp objDef <- kdats key ]


type ValidationExp = (Expression, String) 
-- a ValidationExp is an expression together with the place in the context where we 
-- obtained it from (e.g. rule/interface/..)
showVExp (exp, origin) = "Origin: "++origin++", expression: "++showADL exp

-- validate a single expression and report the results
validateExp :: Fspc -> Options -> ValidationExp -> IO (ValidationExp, Bool)
validateExp _     _    vExp@(ERel _, _)   = -- skip all simple relations 
 do { putStr $ "."
    ; return (vExp, True)
    }
validateExp fSpec opts vExp@(exp, origin) =
 do { --putStr $ "Checking "++origin ++": expression = "++showADL exp
    ; violationsSQL <- fmap sort . evaluateExpSQL fSpec opts $ exp
    ; let violationsAmp = sort $ contents exp
    
    ; if violationsSQL == violationsAmp 
      then 
       do { putStr $ "." -- ++show violationsSQL
          ; return (vExp, True)
          }    
      else
       do { putStrLn "\nMismatch between SQL and Ampersand"
          ; putStrLn $ showVExp vExp
          ; putStrLn $ "SQL violations:\n"++show violationsSQL
          ; putStrLn $ "Ampersand violations:\n" ++ show violationsAmp
          ; return (vExp, False)
          }
    }

-- evaluate normalized exp in SQL
evaluateExpSQL :: Fspc -> Options -> Expression -> IO [(String,String)]
evaluateExpSQL fSpec opts exp =
 do { violations <- fmap sort $ performQuery opts violationsQuery
    ; return violations
    }
 where violationsExpr = conjNF exp
       violationsQuery = fromMaybe (fatal 100 $ "No sql generated for "++showHS opts "" violationsExpr)
                                   (selectExpr fSpec 26 "src" "tgt" $ violationsExpr) 
  
performQuery :: Options -> String -> IO [(String,String)]
performQuery opts queryStr =
 do { queryResult <- executePHP . showPHP $ 
        connectToServer opts ++
        [ "mysql_select_db('"++tempDbName++"');"
        , "$result=mysql_query('"++queryStr++"');"
        , "if(!$result)"
        , "  die('Error '.($ernr=mysql_errno($DB_link)).': '.mysql_error());"
        , "$rows=Array();"
        , "  while (($row = @mysql_fetch_array($result))!==false) {"
        , "    $rows[]=$row;"
        , "    unset($row);"
        , "  }"
        , "echo '[';"
        , "for ($i = 0; $i < count($rows); $i++) {"
        , "  if ($i==0) echo ''; else echo ',';"
        , "  echo '(\"'.addslashes($rows[$i]['src']).'\", \"'.addslashes($rows[$i]['tgt']).'\")';"
        , "}"
        , "echo ']';"
        ]
    ; if "Error" `isPrefixOf` queryResult -- not the most elegant way, but safe since a correct result will always be a list
      then fatal 141 $ "PHP/SQL problem: "++queryResult
      else case reads queryResult of
             [(pairs,"")] -> return pairs
             _            -> fatal 143 $ "Parse error on php result: "++show queryResult
    }

createTempDatabase :: Fspc -> Options -> IO ()
createTempDatabase fSpec opts =
 do { executePHP php
    ; return ()
    }
 where php = showPHP $
               connectToServer opts ++
               createDatabasePHP tempDbName ++
               [ "mysql_select_db('"++tempDbName++"');"
               , "$existing=false;" ] ++ -- used by php code from Installer.php, denotes whether the table already existed
               createTablesPHP fSpec

removeTempDatabase :: Options -> IO ()
removeTempDatabase opts =
 do { executePHP . showPHP $ 
        connectToServer opts ++
        ["mysql_query('DROP DATABASE "++tempDbName++"');"]
    ; return ()
    }

connectToServer :: Options -> [String]
connectToServer opts =
  ["$DB_link = mysql_connect('"++addSlashes (fromMaybe "localhost" $ sqlHost opts)++"'"
                         ++",'"++addSlashes (fromMaybe "root" $ sqlLogin opts)++"'"
                         ++",'"++addSlashes (fromMaybe "" $ sqlPwd opts)++"');"] 
               
-- call the command-line php with phpStr as input
executePHP :: String -> IO String
executePHP phpStr =
 do { --putStrLn $ "Executing PHP:\n" ++ phpStr
    ; tempdir <- catch getTemporaryDirectory
                       (\e -> do let err = show (e :: IOException)
                                 return ".")

    ; (tempfile, temph) <- openTempFile tempdir "phpInput"
    ; hPutStr temph phpStr
    ; hClose temph
     
    ; let cp = CreateProcess
                { cmdspec      = RawCommand "php" [tempfile]
                , cwd          = Nothing -- path
                , env          = Nothing -- environment
                , std_in       = Inherit 
                , std_out      = CreatePipe
                , std_err      = CreatePipe
                , close_fds    = False -- no need to close all other file descriptors
                , create_group = False
                }
            
    ; (_, mStdOut, mStdErr, procHandle) <- createProcess cp 
    ; outputStr <-
        case (mStdOut, mStdErr) of
          (Nothing, _) -> fatal 105 "no output handle"
          (_, Nothing) -> fatal 106 "no error handle"
          (Just stdOutH, Just stdErrH) ->
           do { --putStrLn "done"
              ; errStr <- hGetContents stdErrH
              ; seq (length errStr) $ return ()
              ; hClose stdErrH
              ; unless (null errStr) $
                  putStrLn $ "Error during PHP execution:\n" ++ errStr 
              ; outputStr <- hGetContents stdOutH --and fetch the results from the output pipe
              ; seq (length outputStr) $ return ()
              ; hClose stdOutH
              --; putStrLn $ "Results:\n" ++ outputStr
              ; return outputStr
              }
    ; removeFile tempfile
    ; return outputStr
    }

showPHP phpLines = unlines $ ["<?php"]++phpLines++["?>"]
