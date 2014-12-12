{-# OPTIONS_GHC  -XScopedTypeVariables #-}
module Database.Design.Ampersand.Input.Parsing ( parseADL1pExpr
                                        , runParser)
where

-- TODO: This module is obsolete and should be consolidated with InputProcessing.hs (i.e. extend InputProcessing.hs and rename it to Parsing.hs)

import Control.Monad
import Data.Char
import Data.Maybe
import System.Directory
import System.FilePath
import Database.Design.Ampersand.Input.ADL1.Parser (pContext,pPopulations,pTerm,keywordstxt, keywordsops, specialchars, opchars)
import Database.Design.Ampersand.Misc
import Database.Design.Ampersand.Basics
import Database.Design.Ampersand.Input.ADL1.UU_Scanner -- (scan,initPos)
import UU.Parsing -- (getMsgs,parse,evalSteps,parseIO)
import Database.Design.Ampersand.ADL1
import Control.Exception

type ParseError = Message Token (Maybe Token)

fatal :: Int -> String -> a
fatal = fatalMsg "Input.Parsing"

-- | The parser currently needs to be monadic, because there are multiple versions of the Ampersand language supported. Each parser
--   currently throws errors on systemerror level. They can only be 'catch'ed in a monad.
--   This parser is for parsing of a Context
parseContext :: Options                          -- ^ options to be taken into account
             -> FilePath                         -- ^ the full path to the file to parse
             -> IO (Either ParseError P_Context) -- ^ The IO monad with the parse tree.
parseContext opts file
             = do { rapRes <- if includeRap opts
                              then do let rapFile = ampersandDataDir opts </> "RepoRap" </> "RAP.adl"
                                      exists <- doesFileExist rapFile
                                      when (not exists) (fatal 39 $ "RAP file isn't installed properly. RAP.adl expected at:"
                                                                  ++"\n  "++show rapFile
                                                                  ++"\n  (You might want to reinstall ampersand...)")
                                      fmap addRightJust $ parseADL opts rapFile
                              else return (Right Nothing)
                  ; (case rapRes of
                      Left err -> do verboseLn opts "Parsing of RAP failed"
                                     return $ Left err
                      Right mRapCtx
                               -> do eRes   <- parseADL opts file
                                     case eRes of
                                       Right ctx  -> let ctx' = case mRapCtx of
                                                                  Nothing -> ctx
                                                                  Just rapCtx ->mergeContexts ctx rapCtx
                                                     in  verboseLn opts "Parsing successful"
                                                         >> return (Right ctx')
                                       Left err -> verboseLn opts "Parsing failed"
                                                >> return eRes
                    )
                  }
 where addRightJust :: (Either a b) -> (Either a (Maybe b))
       addRightJust (Left a)  = Left a
       addRightJust (Right b) = Right (Just b)
 
-- | Parse isolated ADL1 expression strings
parseADL1pExpr :: String -> String -> Either String (Term TermPrim)
parseADL1pExpr pexprstr fn = parseExpr pexprstr fn

-- | Parse isolated ADL1 expression strings
parseExpr :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the file (used for error messages)
          -> Either String (Term TermPrim)  -- ^ The result: Either an error message,  or a good result
parseExpr str fn =
    case runParser pTerm fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors:\n"++show msg

parseADL :: Options
         -> FilePath      -- ^ The name of the .adl file
         -> IO (Either ParseError P_Context) -- ^ The result: Either some errors, or the parsetree.
parseADL opts file =
 do { verboseLn opts $ "Files read:"
    ; (result, parsedFiles) <- readAndParseFile opts 0 [] Nothing "" file
    ; verboseLn opts $ "\n"
    ; case result of
        (Left err)       -> return $ Left err
        (Right mContext) -> return $ Right $ fromMaybe (fatal 87 "top-level parse yielded empty result") mContext
    } -- top-level parse will always be Just context. We could factorize the code to ensure this at type level, but this module is probably obsolete anyway. 
    
-- parse the input file and read and parse the imported files
-- The alreadyParsed parameter keeps track of filenames that have been parsed already, which are ignored when included again.
-- Hence, include cycles do not cause an error.
-- We don't distinguish between "INCLUDE SomeADL" and "INCLUDE SoMeAdL" to prevent errors on case-insensitive file systems.
-- (on a case-sensitive file system you do need to keep your includes with correct capitalization though)

readAndParseFile :: Options -> Int -> [String] -> Maybe String -> String -> String ->
                    IO (Either ParseError (Maybe P_Context), [String])
readAndParseFile opts depth alreadyParsed mIncluderFilepath fileDir relativeFilepath =
       do { canonicFilepath <- fmap (map toUpper) $ canonicalizePath filepath
            -- Legacy parser has no includes, so no need to print here

          ; if canonicFilepath `elem` alreadyParsed
            then do { verboseLn opts $ replicate (3*depth) ' ' ++ "(" ++ filepath ++ ")"
                    ; return (Right Nothing, alreadyParsed) -- returning an empty context is easier than a maybe (leads to some plumbing in readAndParseIncludeFiles)
                    }
            else do { fileContents <- Database.Design.Ampersand.Basics.readFile filepath
                    ; verboseLn opts $ replicate (3*depth) ' ' ++ filepath
                    ; fmap addFstRightJust $ parseFileContents opts (depth+1) (canonicFilepath:alreadyParsed)
                                                            fileContents newFileDir newFilename
                    
                    }
          } `catch` \(exc :: IOException) -> 
        do { error $ case mIncluderFilepath of
                       Nothing -> "\n\nError: cannot read Ampersand file " ++ show filepath
                       Just includerFilepath -> "\n\nError: cannot read include file " ++ show filepath ++
                                                ", included by " ++ show includerFilepath}
 where filepath = combine fileDir relativeFilepath
       newFileDir = let dir = takeDirectory filepath in if dir == "." then "" else dir
       newFilename = takeFileName filepath
       addFstRightJust :: (Either a b, c) -> (Either a (Maybe b), c)
       addFstRightJust (Left a, c)  = (Left a,c)
       addFstRightJust (Right b, c) = (Right (Just b),c)

parseFileContents :: Options  -- ^ command-line options
                  -> Int      -- ^ The include depth
                  -> [String] -- ^ Already parsed files (canonicalized)
                  -> String   -- ^ The string to be parsed
                  -> String   -- ^ The path to the .adl file
                  -> String   -- ^ The name of the .adl file
                  -> IO (Either ParseError P_Context, [String]) -- ^ The result: The updated already-parsed contexts and Either some errors, or the parsetree.
parseFileContents opts depth alreadyParsed fileContents fileDir filename =
  do { let filepath = combine fileDir filename
     ; case parseSingleADL fileContents filepath of
           Left err -> return (Left err, alreadyParsed)
           Right (parsedContext, includeFilenames) ->
             do { (includeParseResults, alreadyParsed') <-
                     readAndParseIncludeFiles opts alreadyParsed depth (Just $ combine fileDir filename) fileDir includeFilenames
                ; return ( case includeParseResults of
                             Left err              -> Left err
                             Right includeContexts -> Right $ foldl mergeContexts parsedContext includeContexts
                         , alreadyParsed' )
                }
    }
readAndParseIncludeFiles :: Options -> [String] -> Int -> Maybe String -> String -> [String] ->
                            IO (Either ParseError [P_Context], [String])
readAndParseIncludeFiles opts alreadyParsed depth mIncluderFilepath fileDir [] = return (Right [], alreadyParsed)
readAndParseIncludeFiles opts alreadyParsed depth mIncluderFilepath fileDir (relativeFilepath:relativeFilepaths) =
 do { (result, alreadyParsed') <- readAndParseFile opts depth alreadyParsed mIncluderFilepath fileDir relativeFilepath
    ; case result of                               -- Include is only implemented in Current parser
        Left err -> return (Left err, alreadyParsed')
        Right mContext ->
         do { (results, alreadyParsed'') <- readAndParseIncludeFiles opts alreadyParsed' depth mIncluderFilepath fileDir relativeFilepaths
            ; case results of
                Left err -> return (Left err, alreadyParsed'')
                Right contexts -> let contexts' = case mContext of
                                                    Nothing -> contexts
                                                    Just c  -> c:contexts
                                  in  return (Right contexts', alreadyParsed'')
            }
    }


mergeContexts :: P_Context -> P_Context -> P_Context
mergeContexts (PCtx nm1 pos1 lang1 markup1 thms1 pats1 pprcs1 rs1 ds1 cs1 ks1 vs1 gs1 ifcs1 ps1 pops1 sql1 php1 metas1)
              (PCtx nm2 pos2 lang2 markup2 thms2 pats2 pprcs2 rs2 ds2 cs2 ks2 vs2 gs2 ifcs2 ps2 pops2 sql2 php2 metas2) =
  PCtx{ ctx_nm = nm1
      , ctx_pos = pos1 ++ pos2
      , ctx_lang = lang1
      , ctx_markup = markup1
      , ctx_thms = thms1 ++ thms2
      , ctx_pats = pats1 ++ pats2
      , ctx_PPrcs = pprcs1 ++ pprcs2
      , ctx_rs = rs1 ++ rs2
      , ctx_ds = ds1 ++ ds2
      , ctx_cs = cs1 ++ cs2
      , ctx_ks = ks1 ++ ks2
      , ctx_vs = vs1 ++ vs2
      , ctx_gs = gs1 ++ gs2
      , ctx_ifcs = ifcs1 ++ ifcs2
      , ctx_ps = ps1 ++ ps2
      , ctx_pops = pops1 ++ pops2
      , ctx_sql = sql1 ++ sql2
      , ctx_php = php1 ++ php2
      , ctx_metas = metas1 ++ metas2
      }

parseSingleADL :: String        -- ^ The string to be parsed
               -> String        -- ^ The name of the .adl file (used for error messages)
               -> Either ParseError (P_Context, [String]) -- ^ The result: Either some errors, or the parsetree.

parseSingleADL str fn = runParser pContext                              fn str

-- | Same as parseCtx_ , however this one is for a list of populations
parsePops :: String            -- ^ The string to be parsed
          -> String            -- ^ The name of the .pop file (used for error messages)
          -> Either String [P_Population] -- ^ The result: Either a list of populations, or some errors.
parsePops str fn =
    case  runParser pPopulations fn str of
      Right result -> Right result
      Left  msg    -> Left $ "Parse errors:\n"++show msg

runParser :: forall res . Parser Token res -> String -> String -> Either ParseError res
runParser parser filename input =
  let scanner = scan keywordstxt keywordsops specialchars opchars filename initPos
      steps = parse parser (scanner input)
  in  case  getMsgs steps of
         []    -> let Pair res _ = evalSteps steps
                  in  Right res
         msg:_ -> Left msg