{-# OPTIONS_GHC -fno-enable-rewrite-rules #-} -- Disable rewrite rules to drastically improve compilation speed
{-# LANGUAGE FlexibleContexts #-}
module Parser
  (AmpParser, pContext, pPopulations,pTerm, pRule, keywordstxt, keywordsops, specialchars, opchars) where

import ParsingLib
import LexerToken
import qualified Lexer as L
import Basics  (fatalMsg,Collection(..))
import Core.ParseTree
import Data.List
import Data.Maybe

fatal :: Int -> String -> a
fatal = fatalMsg "Input.ADL1.Parser"

keywordstxt :: [String]
keywordstxt       = L.keywords

keywordsops :: [String]
keywordsops       = L.operators

specialchars :: [Char]
specialchars      = L.special_chars

opchars :: [Char]
opchars           = nub (sort (concat keywordsops))

--to parse files containing only populations
--- Populations ::= Population+
pPopulations :: AmpParser [P_Population]
pPopulations = pList1 pPopulation

--- Context ::= 'CONTEXT' ConceptName LanguageRef TextMarkup? ContextElement* 'ENDCONTEXT'
pContext :: AmpParser (P_Context, [String]) -- the result is the parsed context and a list of include filenames
pContext  = rebuild <$> pKey_pos "CONTEXT" <*> pConceptName
                         <*> pLanguageRef
                         <*> pMaybe pTextMarkup
                         <*> pList pContextElement <* pKey "ENDCONTEXT"
  where
    rebuild :: Origin -> String -> Lang -> Maybe PandocFormat -> [ContextElement] -> (P_Context, [String])
    rebuild    pos'      nm        lang          fmt                   ces
     = (PCtx{ ctx_nm     = nm
            , ctx_pos    = [pos']
            , ctx_lang   = lang
            , ctx_markup = fmt
            , ctx_thms   = (nub.concat) [xs | CThm xs<-ces] -- Names of patterns/processes to be printed in the functional specification. (For partial documents.)
            , ctx_pats   = [p | CPat p<-ces]       -- The patterns defined in this context
            , ctx_PPrcs  = [p | CPrc p<-ces]       -- The processes as defined by the parser
            , ctx_rs     = [p | CRul p<-ces]       -- All user defined rules in this context, but outside patterns and outside processes
            , ctx_ds     = [p | CRel p<-ces]       -- The relations defined in this context, outside the scope of patterns
            , ctx_cs     = [c ("CONTEXT "++nm) | CCon c<-ces]    -- The concept definitions defined in this context, outside the scope of patterns
            , ctx_gs     = [g | CGen g<-ces]       -- The gen definitions defined in this context, outside the scope of patterns
            , ctx_ks     = [k | CIndx k<-ces]      -- The identity definitions defined in this context, outside the scope of patterns
            , ctx_vs     = [v | CView v<-ces]      -- The view definitions defined in this context, outside the scope of patterns
            , ctx_ifcs   = [s | Cifc s<-ces]       -- The interfaces defined in this context, outside the scope of patterns -- fatal 78 ("Diagnostic: "++concat ["\n\n   "++show ifc | Cifc ifc<-ces])
            , ctx_sql    = [p | CSqlPlug p<-ces]   -- user defined sqlplugs, taken from the Ampersand scriptplug<-ces]
            , ctx_php    = [p | CPhpPlug p<-ces]   -- user defined phpplugs, taken from the Ampersand script
            , ctx_ps     = [e | CPrp e<-ces]       -- The purposes defined in this context, outside the scope of patterns
            , ctx_pops   = [p | CPop p<-ces]       -- The populations defined in this contextplug<-ces]
            , ctx_metas  = [meta | CMeta meta <-ces]
            }
       , [s | CIncl s<-ces]) -- the INCLUDE filenames

    --- ContextElement ::= Meta | PatternDef | ProcessDef | RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Interface | Sqlplug | Phpplug | Purpose | Population | PrintThemes | IncludeStatement
    pContextElement :: AmpParser ContextElement
    pContextElement = CMeta    <$> pMeta         <|>
                      CPat     <$> pPatternDef   <|>
                      CPrc     <$> pProcessDef   <|>
                      CRul     <$> pRuleDef      <|>
                      CCfy     <$> pClassify     <|>
                      CRel     <$> pRelationDef  <|>
                      CCon     <$> pConceptDef   <|>
                      CGen     <$> pGenDef       <|>
                      CIndx    <$> pIndex        <|>
                      CView    <$> pViewDef      <|>
                      Cifc     <$> pInterface    <|>
                      CSqlPlug <$> pSqlplug      <|>
                      CPhpPlug <$> pPhpplug      <|>
                      CPrp     <$> pPurpose      <|>
                      CPop     <$> pPopulation   <|>
                      CThm     <$> pPrintThemes  <|>
                      CIncl    <$> pIncludeStatement

data ContextElement = CMeta Meta
                    | CPat P_Pattern
                    | CPrc P_Process
                    | CRul (P_Rule TermPrim)
                    | CCfy P_Gen
                    | CRel P_Declaration
                    | CCon (String->ConceptDef)
                    | CGen P_Gen
                    | CIndx P_IdentDef
                    | CView P_ViewDef
                    | Cifc P_Interface
                    | CSqlPlug P_ObjectDef
                    | CPhpPlug P_ObjectDef
                    | CPrp PPurpose
                    | CPop P_Population
                    | CThm [String]    -- a list of themes to be printed in the functional specification. These themes must be PATTERN or PROCESS names.
                    | CIncl String     -- an INCLUDE statement

--- IncludeStatement ::= 'INCLUDE' String
pIncludeStatement :: AmpParser String
pIncludeStatement = pKey "INCLUDE" *> pString

--- LanguageRef ::= 'IN' ('DUTCH' | 'ENGLISH')
pLanguageRef :: AmpParser Lang
pLanguageRef = pKey "IN" *>
               (( Dutch   <$ pKey "DUTCH"  ) <|>
                ( English <$ pKey "ENGLISH")
               )

--- TextMarkup ::= 'REST' | 'HTML' | 'LATEX' | 'MARKDOWN'
pTextMarkup :: AmpParser PandocFormat
pTextMarkup = ( ReST     <$ pKey "REST"     ) <|>
              ( HTML     <$ pKey "HTML"     ) <|>
              ( LaTeX    <$ pKey "LATEX"    ) <|>
              ( Markdown <$ pKey "MARKDOWN" )

--- Meta ::= 'META' String String
pMeta :: AmpParser Meta
pMeta = Meta <$> pKey_pos "META" <*> pMetaObj <*> pString <*> pString
 where pMetaObj = pSucceed ContextMeta -- for the context meta we don't need a keyword

--- PatternDef ::= 'PATTERN' ConceptName PatElem* 'ENDPATTERN'
pPatternDef :: AmpParser P_Pattern
pPatternDef = rebuild <$> pKey_pos "PATTERN" <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
                      <*> pList pPatElem
                      <*> pKey_pos "ENDPATTERN"
  where
    rebuild :: Origin -> String -> [PatElem] -> Origin -> P_Pattern
    rebuild pos' nm pes end
     = P_Pat { pt_nm  = nm
             , pt_pos = pos'
             , pt_end = end
             , pt_rls = [r | Pr r<-pes]
             , pt_gns = [g | Pg g<-pes]
             , pt_dcs = [d | Pd d<-pes]
             , pt_cds = [c nm | Pc c<-pes]
             , pt_ids = [k | Pk k<-pes]
             , pt_vds = [v | Pv v<-pes]
             , pt_xps = [e | Pe e<-pes]
             , pt_pop = [p | Pp p<-pes]
             }
    --- PatElem ::= RuleDef | Classify | RelationDef | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
    pPatElem :: AmpParser PatElem
    pPatElem = Pr <$> pRuleDef      <|>
               Py <$> pClassify     <|>
               Pd <$> pRelationDef  <|>
               Pc <$> pConceptDef   <|>
               Pg <$> pGenDef       <|>
               Pk <$> pIndex        <|>
               Pv <$> pViewDef      <|>
               Pe <$> pPurpose      <|>
               Pp <$> pPopulation

data PatElem = Pr (P_Rule TermPrim)
             | Py P_Gen
             | Pd P_Declaration
             | Pc (String->ConceptDef)
             | Pg P_Gen
             | Pk P_IdentDef
             | Pv P_ViewDef
             | Pe PPurpose
             | Pp P_Population

--- ProcessDef ::= 'PROCESS' ConceptName ProcElem* 'ENDPROCESS'
pProcessDef :: AmpParser P_Process
pProcessDef = rebuild <$> pKey_pos "PROCESS" <*> pConceptName   -- The name spaces of patterns, processes and concepts are shared.
                      <*> pList pProcElem
                      <*> pKey_pos "ENDPROCESS"
   where
    rebuild :: Origin -> String -> [ProcElem] -> Origin -> P_Process
    rebuild pos' nm pes end
      = P_Prc { procNm    = nm
              , procPos   = pos'
              , procEnd   = end
              , procRules = [rr | PrR rr<-pes]
              , procGens  = [g  | PrG g <-pes]
              , procDcls  = [d  | PrD d <-pes]
              , procRRuls = [rr | PrM rr<-pes]
              , procRRels = [rr | PrL rr<-pes]
              , procCds   = [cd nm | PrC cd<-pes]
              , procIds   = [ix | PrI ix<-pes]
              , procVds   = [vd | PrV vd<-pes]
              , procXps   = [e  | PrE e <-pes]
              , procPop   = [p  | PrP p <-pes]
              }
    --- ProcElem ::= RuleDef | Classify | RelationDef | RoleRule | RoleRelation | ConceptDef | GenDef | Index | ViewDef | Purpose | Population
    pProcElem :: AmpParser ProcElem
    pProcElem = PrR <$> pRuleDef      <|>
                PrY <$> pClassify     <|>
                PrD <$> pRelationDef  <|>
                PrM <$> pRoleRule     <|>
                PrL <$> pRoleRelation <|>
                PrC <$> pConceptDef   <|>
                PrG <$> pGenDef       <|>
                PrI <$> pIndex        <|>
                PrV <$> pViewDef      <|>
                PrE <$> pPurpose      <|>
                PrP <$> pPopulation

data ProcElem = PrR (P_Rule TermPrim)
              | PrY P_Gen
              | PrD P_Declaration
              | PrM RoleRule
              | PrL P_RoleRelation
              | PrC (String->ConceptDef)
              | PrG P_Gen
              | PrI P_IdentDef
              | PrV P_ViewDef
              | PrE PPurpose
              | PrP P_Population

--- Classify ::= 'CLASSIFY' ConceptRef 'IS' Cterm
pClassify :: AmpParser P_Gen   -- Example: CLASSIFY A IS B /\ C /\ D
pClassify = rebuild <$> pKey_pos "CLASSIFY"
                    <*> pConceptRef
                    <*  pKey "IS"
                    <*> pCterm
               where
                 rebuild po lhs rhs
                   = P_Cy { gen_spc  = lhs             --  Left hand side concept expression
                          , gen_rhs  = rhs             --  Right hand side concept expression
                          , gen_fp   = po
                          }
                 --- Cterm ::= Cterm1 ('/\' Cterm1)*
                 --- Cterm1 ::= ConceptRef | ('('? Cterm ')'?)
                 pCterm  = f <$> pList1Sep (pKey "/\\") pCterm1
                 pCterm1 = g <$> pConceptRef                        <|>
                           h <$> (pSpec '(' *> pCterm <* pSpec ')')  -- brackets are allowed for educational reasons.
                 f ccs = concat ccs
                 g c = [c]
                 h cs = cs

--- RuleDef ::= 'RULE' (ADLid ':')? Rule Meaning* Message* Violation?
pRuleDef :: AmpParser (P_Rule TermPrim)
pRuleDef =  rebuild <$> pKey_pos "RULE"
                    <*> pMaybe (pADLid <* pKey ":" )
                    <*> pRule
                    <*> pList pMeaning
                    <*> pList pMessage
                    <*> pMaybe pViolation
               where
                 rebuild po mn rexp mean msg mViolation
                   = P_Ru { rr_nm   = fromMaybe (rulid po) mn
                          , rr_exp  = rexp
                          , rr_fps  = po
                          , rr_mean = mean
                          , rr_msg  = msg
                          , rr_viol = mViolation
                          }
                 rulid (FileLoc(FilePos (_,src,_))) = "rule@line" ++ show (sourceLine src)
                 rulid _ = fatal 226 "pRuleDef is expecting a file location."
                 
                 --- Violation ::= 'VIOLATION' PairView
                 pViolation :: AmpParser (PairView (Term TermPrim))
                 pViolation = id <$ pKey "VIOLATION" <*> pPairView

                 --- PairView ::= '(' PairViewSegmentList ')'
                 pPairView :: AmpParser (PairView (Term TermPrim))
                 pPairView = PairView <$ pSpec '(' <*> pList1Sep (pSpec ',') pPairViewSegment <* pSpec ')'

                 --- PairViewSegmentList  ::= PairViewSegment (',' PairViewSegment)*
                 --- PairViewSegment ::= SrcOrTgt Term | 'TXT' String
                 pPairViewSegment :: AmpParser (PairViewSegment (Term TermPrim))
                 pPairViewSegment = PairViewExp <$> pSrcOrTgt <*>  pTerm
                                <|> PairViewText <$ pKey "TXT" <*> pString

--- SrcOrTgt ::= 'SRC' | 'TGT'
pSrcOrTgt :: AmpParser SrcOrTgt
pSrcOrTgt = Src <$ pKey "SRC" <|> Tgt <$ pKey "TGT"

--- RelationDef ::= (Varid '::' ConceptRef Fun ConceptRef | 'RELATION' Varid Sign) 'BYPLUG'? Props? 'BYPLUG'? Pragma? Meaning* ('=' Content)? '.'?
pRelationDef :: AmpParser P_Declaration
pRelationDef      = ( rebuild <$> pVarid  <*> pKey_pos "::"  <*> pConceptRef  <*> pFun  <*> pConceptRef
                      <|> rbd <$> pKey_pos "RELATION" <*> pVarid  <*> pSign
                    )
                      <*> ((True <$ pKey "BYPLUG") `opt` False)
                      <*> (pProps `opt` [])
                      <*> ((True <$ pKey "BYPLUG") `opt` False)
                      <*> (pPragma `opt` [])
                      <*> pList pMeaning
                      <*> ((pKey "=" *> pContent) `opt` [])
                      <* (pKey "." `opt` "")         -- in the syntax before 2011, a dot was required. This optional dot is there to save user irritation during the transition to a dotless era  :-) .
                    where rebuild nm pos' src fun' trg bp1 props --bp2 pragma meanings content
                            = rbd pos' nm (P_Sign src trg,pos') bp1 props' --bp2 pragma meanings content
                              where props'= nub (props `uni` fun')
                          rbd pos' nm (sgn,_) bp1 props bp2 pragma meanings content
                            = P_Sgn { dec_nm   = nm
                                    , dec_sign = sgn
                                    , dec_prps = props
                                    , dec_prL  = head pr
                                    , dec_prM  = pr!!1
                                    , dec_prR  = pr!!2
                                    , dec_Mean = meanings
                                    , dec_popu = content
                                    , dec_fpos = pos'
                                    , dec_plug = bp1 || bp2
                                    }
                              where pr = pragma++["","",""]

                          --- Props ::= '[' PropList? ']'
                          pProps :: AmpParser [Prop]
                          pProps  = (f.concat) <$> (pSpec '[' *> pListSep (pSpec ',') pProp <* pSpec ']')
                              where f ps = nub (ps ++ concat [[Uni, Inj] | null ([Sym, Asy]>-ps)])
                          
                          --- PropList ::= Prop (',' Prop)*
                          --- Prop ::= 'UNI' | 'INJ' | 'SUR' | 'TOT' | 'SYM' | 'ASY' | 'TRN' | 'RFX' | 'IRF' | 'AUT' | 'PROP'
                          pProp :: AmpParser [Prop]
                          pProp   = k [Uni] "UNI" <|> k [Inj] "INJ" <|> k [Sur] "SUR" <|> k [Tot] "TOT" <|>
                                    k [Sym] "SYM" <|> k [Asy] "ASY" <|> k [Trn] "TRN" <|>
                                    k [Rfx] "RFX" <|> k [Irf] "IRF" <|> k [Aut] "AUT" <|> k [Sym, Asy] "PROP"
                              where k obj str = f <$> pKey str where f _ = obj
                          
                          --- Pragma ::= 'PRAGMA' String+
                          pPragma :: AmpParser [String]
                          pPragma = pKey "PRAGMA" *> pList1 pString
                          
                          --- Fun ::= '*' | '->' | '<-' | '[' Mult '-' Mult ']'
                          pFun :: AmpParser [Prop]
                          pFun    = []        <$ pKey "*"  <|>
                                    [Uni,Tot] <$ pKey "->" <|>
                                    [Sur,Inj] <$ pKey "<-" <|>
                                    (rbld     <$  pSpec '['
                                              <*> (pMult (Sur,Inj) `opt` [])
                                              <*  pKey "-"
                                              <*> (pMult (Tot,Uni) `opt` [])
                                              <*  pSpec ']'
                                    )
                              where
                                --- Mult ::= ('0' | '1') '..' ('1' | '*') | '*' | '1'
                                pMult :: (Prop,Prop) -> AmpParser [Prop]
                                pMult (ts,ui) = rbld  <$> (( []   <$ pKey "0") <|> ([ts] <$ pKey "1") )
                                                      <*  pKey ".."
                                                      <*> (( [ui] <$ pKey "1") <|> ([]   <$ pKey "*" )) <|>
                                                [] <$ pKey "*"  <|>
                                                [ts,ui] <$ pKey "1"
                                rbld a b = a++b

--- ConceptDef ::= 'CONCEPT' ConceptName 'BYPLUG'? String ('TYPE' String)? String?
pConceptDef :: AmpParser (String->ConceptDef)
pConceptDef       = Cd <$> pKey_pos "CONCEPT"
                       <*> pConceptName           -- the concept name
                       <*> ((True <$ pKey "BYPLUG") `opt` False)
                       <*> pString                -- the definition text
                       <*> ((pKey "TYPE" *> pString) `opt` "")     -- the type of the concept.
                       <*> (pString `opt` "")     -- a reference to the source of this definition.

--- GenDef ::= 'SPEC' ConceptRef 'ISA' ConceptRef | 'CLASSIFY' ConceptRef 'ISA' ConceptRef | Classify
pGenDef :: AmpParser P_Gen
pGenDef           = rebuild <$> pKey_pos "SPEC"     <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>  -- SPEC is obsolete syntax. Should disappear!
                    rebuild <$> pKey_pos "CLASSIFY" <*> pConceptRef <* pKey "ISA" <*> pConceptRef <|>
                    pClassify
                    where rebuild p spc gen = PGen{gen_spc=spc, gen_gen=gen, gen_fp =p}

-- | A identity definition looks like:   IDENT onNameAdress : Person(name, address),
-- which means that name<>name~ /\ address<>addres~ |- I[Person].
-- The label 'onNameAddress' is used to refer to this identity.
-- You may also use an expression on each attribute place, for example: IDENT onpassport: Person(nationality, passport;documentnr),
-- which means that nationality<>nationality~ /\ passport;documentnr<>(passport;documentnr)~ |- I[Person].
--- Index ::= 'IDENT' Label ConceptRefPos '(' IndSegmentList ')'
pIndex :: AmpParser P_IdentDef
pIndex  = identity <$ pKey "IDENT" <*> pLabel <*> pConceptRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pIndSegment <* pSpec ')'
    where identity :: Label -> (P_Concept, Origin) -> [P_IdentSegment] -> P_IdentDef
          identity (Lbl nm _ _) (c, orig) ats
           = P_Id { ix_pos = orig
                  , ix_lbl = nm
                  , ix_cpt = c
                  , ix_ats = ats
                  }

          --- IndSegmentList ::= IndSegment (',' IndSegment)
          --- IndSegment ::= IndAtt
          pIndSegment :: AmpParser P_IdentSegment
          pIndSegment = P_IdentExp <$> pIndAtt

          --- IndAtt ::= LabelProps Term | Term
          pIndAtt :: AmpParser P_ObjectDef
          pIndAtt  = attL <$> pLabelProps <*> pTerm <|>
                     att <$> pTerm
              where attL (Lbl nm p strs) attexpr =
                       P_Obj { obj_nm   = nm
                             , obj_pos  = p
                             , obj_ctx  = attexpr
                             , obj_msub = Nothing
                             , obj_strs = strs
                             }
                    att attexpr =
                        P_Obj { obj_nm   = ""
                              , obj_pos  = Origin "pIndAtt CC664"
                              , obj_ctx  = attexpr
                              , obj_msub = Nothing
                              , obj_strs = []
                              }

-- | A view definition looks like:
--      VIEW onSSN: Person("social security number":ssn)
-- or
--      VIEW SaveAdlFile: SaveAdlFile(PRIMHTML "<a href='../../index.php?operation=2&file=", filepath , filename
--      ,PRIMHTML "&userrole=", savecontext~;sourcefile;uploaded~;userrole
--      ,PRIMHTML "'>", filename/\V[SaveAdlFile*FileName], PRIMHTML "</a>")
-- which can be used to define a proper user interface by assigning labels and markup to the attributes in a view.

--- ViewDef ::= ('VIEW' | 'KEY') LabelProps ConceptOneRefPos '(' ViewSegmentSepList ')'
pViewDef :: AmpParser P_ViewDef
pViewDef  = vd <$ (pKey "VIEW" <|> pKey "KEY") <*> pLabelProps <*> pConceptOneRefPos <* pSpec '(' <*> pList1Sep (pSpec ',') pViewSegment <* pSpec ')'
    where vd :: Label -> (P_Concept, Origin) -> [P_ViewSegment] -> P_ViewDef
          vd (Lbl nm _ _) (c, orig) ats
              = P_Vd { vd_pos = orig
                     , vd_lbl = nm
                     , vd_cpt = c
                     , vd_ats = [ case viewSeg of
                                     P_ViewExp x       -> if null (obj_nm x) then P_ViewExp $ x{obj_nm=show i} else P_ViewExp x
                                     P_ViewText _ -> viewSeg
                                     P_ViewHtml _ -> viewSeg
                                | (i,viewSeg)<-zip [(1::Integer)..] ats]
                     } -- nrs also count text segments but they're are not important anyway
          --- ViewSegmentSepList ::= ViewSegment (',' ViewSegment)*
          --- ViewSegment ::= ViewAtt | 'TXT' String | 'PRIMHTML' String
          pViewSegment :: AmpParser P_ViewSegment
          pViewSegment = P_ViewExp  <$> pViewAtt <|>
                         P_ViewText <$ pKey "TXT" <*> pString <|>
                         P_ViewHtml <$ pKey "PRIMHTML" <*> pString
          --- ViewAtt ::= LabelProps? Term
          pViewAtt :: AmpParser P_ObjectDef
          pViewAtt = rebuild <$> pMaybe pLabelProps <*> pTerm
              where
                rebuild mLbl attexpr =
                  case mLbl of
                    Just (Lbl nm p strs) ->
                            P_Obj { obj_nm   = nm
                                  , obj_pos  = p
                                  , obj_ctx  = attexpr
                                  , obj_msub = Nothing
                                  , obj_strs = strs
                                  }
                    Nothing ->
                            P_Obj { obj_nm   = ""
                                  , obj_pos  = origin attexpr
                                  , obj_ctx  = attexpr
                                  , obj_msub = Nothing
                                  , obj_strs = []
                                  }

--- Interface ::= 'INTERFACE' ADLid 'CLASS'? (Conid | String) Params? InterfaceArgs? Roles? ':' Term SubInterface
pInterface :: AmpParser P_Interface
pInterface = lbl <$> (pKey "INTERFACE" *> pADLid_val_pos) <*>
                     (pMaybe $ pKey "CLASS" *> (pConid <|> pString)) <*> -- the class is an upper-case identifier or a quoted string
                     (pParams `opt` [])                   <*>       -- a list of expressions, which say which relations are editable within this service.
                                                                    -- either  Prel _ nm
                                                                    --       or  PTrel _ nm sgn
                     (pArgs   `opt` [])                   <*>
                     (pRoles  `opt` [])                   <*>
                     (pKey ":" *> pTerm)                  <*>
                     pSubInterface
    where lbl :: (String, Origin) -> Maybe String -> [TermPrim] -> [[String]] -> [String] -> (Term TermPrim) -> P_SubInterface -> P_Interface
          lbl (nm,p) iclass params args roles expr sub
             = P_Ifc { ifc_Name   = nm
                     , ifc_Class  = iclass
                     , ifc_Params = params
                     , ifc_Args   = args
                     , ifc_Roles  = roles
                     , ifc_Obj    = P_Obj { obj_nm   = nm
                                          , obj_pos  = p
                                          , obj_ctx  = expr
                                          , obj_msub = Just sub
                                          , obj_strs = args
                                          }
                     , ifc_Pos    = p
                     , ifc_Prp    = ""   --TODO: Nothing in syntax defined for the purpose of the interface.
                     }
          --- Params ::= '(' RelSignList ')'
          pParams = pSpec '(' *> pList1Sep (pSpec ',') pRelSign          <* pSpec ')'
          --- InterfaceArgs ::= '{' ADLidListList '}'
          pArgs   = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid)   <* pSpec '}'
          --- Roles ::= 'FOR' ADLidList
          pRoles  = pKey "FOR" *> pList1Sep (pSpec ',') pADLid

--- SubInterface ::= ('BOX' | 'ROWS' | 'COLS') Box | 'INTERFACE' ADLid
pSubInterface :: AmpParser P_SubInterface
pSubInterface = (\(o,cl) objs -> P_Box o cl objs) <$> pBoxKey <*> pBox
            <|> (\(n,p) -> P_InterfaceRef p n) <$ pKey "INTERFACE" <*> pADLid_val_pos
  where pBoxKey :: AmpParser (Origin, Maybe String)
        pBoxKey = (\o -> (o,Nothing))     <$> pKey_pos "BOX"
              <|> (\o -> (o,Just "ROWS")) <$> pKey_pos "ROWS"
              <|> (\o -> (o,Just "COLS")) <$> pKey_pos "COLS"
              <|> (\o -> (o,Just "TABS")) <$> pKey_pos "TABS"

--- ObjDef ::= LabelProps Term SubInterface?
--- ObjDefList ::= ObjDef (',' ObjDef)*
pObjDef :: AmpParser P_ObjectDef
pObjDef            = obj <$> pLabelProps
                         <*> pTerm            -- the context expression (for example: I[c])
                         <*> pMaybe pSubInterface  -- the optional subinterface
                     where obj (Lbl nm pos' strs) expr msub  =
                             P_Obj { obj_nm   = nm
                                   , obj_pos  = pos'
                                   , obj_ctx  = expr
                                   , obj_msub = msub
                                   , obj_strs = strs
                                   }
--- Box ::= '[' ObjDefList ']'
pBox :: AmpParser [P_ObjectDef]
pBox              = pSpec '[' *> pList1Sep (pSpec ',') pObjDef <* pSpec ']'

--- Sqlplug ::= 'SQLPLUG' ObjDef
pSqlplug :: AmpParser P_ObjectDef
pSqlplug          = pKey_pos "SQLPLUG" *> pObjDef

--- Phpplug ::= 'PHPPLUG' ObjDef
pPhpplug :: AmpParser P_ObjectDef
pPhpplug          = pKey_pos "PHPPLUG" *> pObjDef

--- Purpose ::= 'PURPOSE' Ref2Obj LanguageRef? TextMarkup? ('REF' StringListSemi)? Expl
pPurpose :: AmpParser PPurpose
pPurpose          = rebuild <$> pKey_pos "PURPOSE"  -- "EXPLAIN" has become obsolete
                            <*> pRef2Obj
                            <*> pMaybe pLanguageRef
                            <*> pMaybe pTextMarkup
                            <*> ((pKey "REF" *> (pList1Sep pSemi pString)) `opt` [])
                            <*> pExpl
     where
       rebuild :: Origin -> PRef2Obj -> Maybe Lang -> Maybe PandocFormat -> [String] -> String -> PPurpose
       rebuild    orig      obj         lang          fmt                   refs       str
           = PRef2 orig obj (P_Markup lang fmt str) (concatMap (splitOn ";") refs)
              where splitOn :: Eq a => [a] -> [a] -> [[a]]
                    splitOn [] s = [s]
                    splitOn s t  = case findIndex (isPrefixOf s) (tails t) of
                                     Nothing -> [t]
                                     Just i  -> [take i t]  ++ splitOn s (drop (i+length s) t)
       --- Ref2Obj ::= 'CONCEPT' ConceptName | 'RELATION' RelSign | 'RULE' ADLid | 'IDENT' ADLid | 'VIEW' ADLid | 'PATTERN' ADLid | 'PROCESS' ADLid | 'INTERFACE' ADLid | 'CONTEXT' ADLid
       pRef2Obj :: AmpParser PRef2Obj
       pRef2Obj = PRef2ConceptDef  <$ pKey "CONCEPT"   <*> pConceptName <|>
                  PRef2Declaration <$ pKey "RELATION"  <*> pRelSign     <|>
                  PRef2Rule        <$ pKey "RULE"      <*> pADLid       <|>
                  PRef2IdentityDef <$ pKey "IDENT"     <*> pADLid       <|>
                  PRef2ViewDef     <$ pKey "VIEW"      <*> pADLid       <|>
                  PRef2Pattern     <$ pKey "PATTERN"   <*> pADLid       <|>
                  PRef2Process     <$ pKey "PROCESS"   <*> pADLid       <|>
                  PRef2Interface   <$ pKey "INTERFACE" <*> pADLid       <|>
                  PRef2Context     <$ pKey "CONTEXT"   <*> pADLid

--- Population ::= 'POPULATION' RelSign 'CONTAINS' Content | 'POPULATION' ConceptName 'CONTAINS' '[' ValueList ']'
pPopulation :: AmpParser P_Population
pPopulation = prelpop <$> pKey_pos "POPULATION" <*> pRelSign     <* pKey "CONTAINS" <*> pContent <|>
              pcptpop <$> pKey_pos "POPULATION" <*> pConceptName <* pKey "CONTAINS" <*> (pSpec '[' *> pListSep pComma pString <* pSpec ']')
    where
      prelpop :: Origin -> TermPrim -> Pairs -> P_Population
      prelpop    orig     (Prel _ nm)  contents
       = P_RelPopu { p_rnme   = nm
                   , p_orig   = orig
                   , p_popps  = contents
                   }
      prelpop orig (PTrel _ nm sgn) contents
       = P_TRelPop { p_rnme   = nm
                   , p_type   = sgn
                   , p_orig   = orig
                   , p_popps  = contents
                   }
      prelpop _ expr _ = fatal 429 ("Expression "++show expr++" should never occur in prelpop.")
      pcptpop :: Origin -> String -> [String] -> P_Population
      pcptpop    orig      cnm       contents
       = P_CptPopu { p_cnme   = cnm
                   , p_orig   = orig
                   , p_popas  = contents
                   }

--- RoleRelation ::= 'ROLE' ADLidList 'EDITS' RelSignList
pRoleRelation :: AmpParser P_RoleRelation
pRoleRelation      = rr <$> pKey_pos "ROLE"              <*>
                            pList1Sep (pSpec ',') pADLid <*
                            pKey "EDITS"                 <*>
                            pList1Sep (pSpec ',') pRelSign
                     where rr p roles rels = P_RR roles rels p

--- RoleRule ::= 'ROLE' ADLidList 'MAINTAINS' ADLidList
pRoleRule :: AmpParser RoleRule
pRoleRule         = rr <$> pKey_pos "ROLE"               <*>
                           pList1Sep (pSpec ',') pADLid  <*
                           pKey "MAINTAINS"              <*>
                           pList1Sep (pSpec ',') pADLid
                    where rr p roles rulIds = Maintain roles rulIds p

--- PrintThemes ::= 'THEMES' ConceptNameList
pPrintThemes :: AmpParser [String]
pPrintThemes = pKey "THEMES"
            *> pList1Sep (pSpec ',') pConceptName  -- Patterns, processes and concepts share the same name space, so these names must be checked whether the processes and patterns exist.

--- Meaning ::= 'MEANING' LanguageRef? TextMarkup? (String | Expl)
pMeaning :: AmpParser PMeaning
pMeaning = rebuild <$  pKey "MEANING"
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> (pString <|> pExpl)
   where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMeaning
         rebuild    lang          fmt                   mkup   =
            PMeaning (P_Markup lang fmt mkup)

--- Message ::= 'MESSAGE' LanguageRef? TextMarkup? (String | Expl)
pMessage :: AmpParser PMessage
pMessage = rebuild <$ pKey "MESSAGE"
                   <*> pMaybe pLanguageRef
                   <*> pMaybe pTextMarkup
                   <*> (pString <|> pExpl)
   where rebuild :: Maybe Lang -> Maybe PandocFormat -> String -> PMessage
         rebuild    lang          fmt                   mkup   =
            PMessage (P_Markup lang fmt mkup)

{-  Basically we would have the following expression syntax:
pRule ::= pTrm1   "="    pTerm                           |  -- equivalence
       pTrm1   "|-"   pTerm                           |  -- implication or subset
       pTrm1 .
pTerm ::= pList1Sep "/\\" pTrm2                          |  -- intersection
       pList1Sep "\\/" pTrm2                          |  -- union
       pTrm2 .
pTrm2 ::= pTrm3    "-"    pTrm3                          |  -- set difference
       pTrm3 .
pTrm3 ::= pTrm4   "\\"   pTrm4                           |  -- right residual
       pTrm4   "/"    pTrm4                           |  -- left residual
       pTrm4 .
pTrm4 ::= pList1Sep ";" pTrm5                            |  -- composition       (semicolon)
       pList1Sep "!" pTrm5                            |  -- relative addition (dagger)
       pList1Sep "#" pTrm5                            |  -- cartesian product (asterisk)
       pTrm5 .
pTrm5 ::= "-"     pTrm6                                  |  -- unary complement
       pTrm6   pSign                                  |  -- unary type cast
       pTrm6   "~"                                    |  -- unary flip
       pTrm6   "*"                                    |  -- unary Kleene star
       pTrm6   "+"                                    |  -- unary Kleene plus
       pTrm6 .
pTrm6 ::= pRelation                                      |
       "("   pTerm   ")" .
In practice, we have it a little different.
 - In order to avoid "associative" brackets, we parse the associative operators "\/", "/\", ";", and "!" with pList1Sep. That works.
 - We would like the user to disambiguate between "=" and "|-" by using brackets.
-}

{- In theory, the expression is parsed by:
pRule :: AmpParser (Term TermPrim)
pRule  =  fEequ <$> pTrm1  <*>  pKey_pos "="   <*>  pTerm   <|>
          fEimp <$> pTrm1  <*>  pKey_pos "|-"  <*>  pTerm   <|>
          pTrm1
          where fequ  lExp orig rExp = PEqu orig lExp rExp
                fEimp lExp orig rExp = PImp orig lExp rExp
-- However elegant, this solution needs to be left-factored in order to get a performant parser.
-}
--- Rule ::= Term ('=' Term | '|-' Term)?
pRule :: AmpParser (Term TermPrim)
pRule  =  pTerm <??> (fEqu  <$> pKey_pos "="  <*> pTerm <|>
                      fImpl <$> pKey_pos "|-" <*> pTerm )
          where fEqu  orig rExp lExp = PEqu orig lExp rExp
                fImpl orig rExp lExp = PImp orig lExp rExp

{-
pTrm1 is slightly more complicated, for the purpose of avoiding "associative" brackets.
The idea is that each operator ("/\\" or "\\/") can be parsed as a sequence without brackets.
However, as soon as they are combined, brackets are needed to disambiguate the combination.
There is no natural precedence of one operator over the other.
Brackets are enforced by parsing the subexpression as pTrm5.
In order to maintain performance standards, the parser is left factored.
The functions pars and f have arguments 'combinator' and 'operator' only to avoid writing the same code twice.
-}
--- Term ::= Trm2 (('\/' Trm2)* | ('\/' Trm2)*)?
pTerm :: AmpParser (Term TermPrim)
pTerm   = pTrm2 <??> (f PIsc <$> pars PIsc "/\\" <|> f PUni <$> pars PUni "\\/")
          where pars combinator operator
                 = g <$> pKey_pos operator <*> pTrm2 <*> pMaybe (pars combinator operator)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)
                f combinator (orig, y) x = combinator orig x y

-- The left factored version of difference: (Actually, there is no need for left-factoring here, but no harm either)
--- Trm2 ::= Trm3 ('-' Trm3)?
pTrm2 :: AmpParser (Term TermPrim)
pTrm2   = pTrm3 <??> (f <$> pKey_pos "-" <*> pTrm3)
          where f orig rExp lExp = PDif orig lExp rExp

-- The left factored version of right- and left residuals:
--- Trm3 ::= Trm4 ('/' Trm4 | '\' Trm4 | '<>' Trm4)?
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (fLrs <$> pKey_pos "/" <*> pTrm4 <|> fRrs <$> pKey_pos "\\"  <*> pTrm4 <|> fDia <$> pKey_pos "<>" <*> pTrm4 )
          where fLrs orig rExp lExp = PLrs orig lExp rExp
                fRrs orig rExp lExp = PRrs orig lExp rExp
                fDia orig rExp lExp = PDia orig lExp rExp

{- by the way, a slightly different way of getting exactly the same result is:
pTrm3 :: AmpParser (Term TermPrim)
pTrm3  =  pTrm4 <??> (f <$>  (pKey_val_pos "/" <|> pKey_val_pos "\\" <|> pKey_val_pos "<>") <*> pTrm4 )
          where f ("\\", orig) rExp lExp = PRrs orig lExp rExp
                f ("/" , orig) rExp lExp = PLrs orig lExp rExp
                f (_   , orig) rExp lExp = PDia orig lExp rExp
-}

-- composition and relational addition are associative, and parsed similar to union and intersect...
--- Trm4 ::= Trm5 ((';' Trm5)+ | ('!' Trm5)+ | ('#' Trm5)+)?
pTrm4 :: AmpParser (Term TermPrim)
pTrm4   = pTrm5 <??> (f PCps <$> pars PCps ";" <|> f PRad <$> pars PRad "!" <|> f PPrd <$> pars PPrd "#")
          where pars combinator operator
                 = g <$> pKey_pos operator <*> pTrm5 <*> pMaybe (pars combinator operator)
                          where g orig y Nothing  = (orig, y)
                                g orig y (Just (org,z)) = (orig, combinator org y z)
                f combinator (orig, y) x = combinator orig x y

--- Trm5 ::= '-'* Trm6 ('~' | '*' | '+')*
pTrm5 :: AmpParser (Term TermPrim)
pTrm5  =  f <$> pList (pKey_val_pos "-") <*> pTrm6  <*> pList ( pKey_val_pos "~" <|> pKey_val_pos "*" <|> pKey_val_pos "+" )
          where f ms pe (("~",_):ps) = let x=f ms pe ps in PFlp (origin x) x  -- the type checker requires that the origin of x is equal to the origin of its converse.
                f ms pe (("*",orig):ps) = PKl0 orig (f ms pe ps)              -- e*  Kleene closure (star)
                f ms pe (("+",orig):ps) = PKl1 orig (f ms pe ps)              -- e+  Kleene closure (plus)
                f (_:_:ms) pe ps        = f ms pe ps                          -- -e  complement     (unary minus)
                f ((_,orig):ms) pe ps   = let x=f ms pe ps in PCpl orig x     -- the type checker requires that the origin of x is equal to the origin of its complement.
                f _ pe _                = pe

--- Trm6 ::= RelationRef | '(' Term ')'
pTrm6 :: AmpParser (Term TermPrim)
pTrm6  =  (Prim <$> pRelationRef)  <|>
          PBrk <$>  pSpec_pos '('  <*>  pTerm  <*  pSpec ')'

--- RelationRef ::= RelSign | 'I' ('[' ConceptOneRef ']')? | 'V' Sign? | Atom ('[' ConceptOneRef ']')?
pRelationRef :: AmpParser TermPrim
pRelationRef      = pRelSign                                                                         <|>
                    pid   <$> pKey_pos "I"  <*> pMaybe (pSpec '[' *> pConceptOneRef <* pSpec ']')  <|>
                    pfull <$> pKey_pos "V"  <*> pMaybe pSign                                       <|>
                    singl <$> pAtom_val_pos <*> pMaybe (pSpec '[' *> pConceptOneRef <* pSpec ']')
                    where pid orig Nothing = PI orig
                          pid orig (Just c)= Pid orig c
                          pfull orig Nothing = PVee orig
                          pfull orig (Just (P_Sign src trg, _)) = Pfull orig src trg
                          singl (nm,orig) x  = Patm orig nm x

--- RelSignList ::= RelSign (',' RelSign)*
--- RelSign ::= Varid Sign?
pRelSign :: AmpParser TermPrim
pRelSign          = prel  <$> pVarid_val_pos <*> pMaybe pSign
                    where prel (nm,orig) Nothing = Prel orig nm
                          prel (nm,_) (Just (sgn,orig)) = PTrel orig nm sgn

--- Sign ::= '[' ConceptOneRef ('*' ConceptOneRef)? ']'
pSign :: AmpParser (P_Sign,Origin)
pSign = rebuild <$> pSpec_pos '[' <*> pConceptOneRef <*> pMaybe (pKey "*" *> pConceptOneRef) <* pSpec ']'
   where
     rebuild :: Origin -> P_Concept -> Maybe P_Concept -> (P_Sign,Origin)
     rebuild orig a mb
      = case mb of
          Just b  -> (P_Sign a b, orig)
          Nothing -> (P_Sign a a, orig)

--- ConceptName ::= Conid | String
--- ConceptNameList ::= ConceptName (',' ConceptName)
pConceptName ::   AmpParser String
pConceptName    = pConid <|> pString

--- ConceptRef ::= ConceptName
pConceptRef ::    AmpParser P_Concept
pConceptRef     = PCpt <$> pConceptName

--- ConceptOneRef ::= 'ONE' | ConceptRef
pConceptOneRef :: AmpParser P_Concept
pConceptOneRef  = (P_Singleton <$ pKey "ONE") <|> pConceptRef

--- ConceptRefPos ::= Conid | String
pConceptRefPos :: AmpParser (P_Concept, Origin)
pConceptRefPos     = conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                     where conid :: (String, Origin) ->  (P_Concept, Origin)
                           conid (c,orig) = (PCpt c, orig)

--- ConceptOneRefPos ::= 'ONE' | Conid | String
pConceptOneRefPos :: AmpParser (P_Concept, Origin)
pConceptOneRefPos  = singl <$> pKey_pos "ONE"   <|>   conid <$> pConid_val_pos   <|>   conid <$> pString_val_pos
                     where singl :: Origin ->  (P_Concept, Origin)
                           singl orig     = (P_Singleton, orig)
                           conid :: (String, Origin) ->  (P_Concept, Origin)
                           conid (c,orig) = (PCpt c, orig)

--  (SJ) Why does a label have (optional) strings?
--  (GM) This is a binding mechanism for implementation specific properties, such as SQL/PHP plug,PHP web app,etc.
--  (SJ April 15th, 2013) Since KEY has been replaced by IDENT and VIEW, there is a variant with props  (pLabelProps) and one without (pLabel).
--- LabelProps ::= ADLid LabelPropsArgs? ':'
pLabelProps :: AmpParser Label
pLabelProps       = lbl <$> pADLid_val_pos
                        <*> (pArgs `opt` [])
                        <*  pKey_pos ":"
                    where lbl :: (String, Origin) -> [[String]] -> Label
                          lbl (nm,pos') strs = Lbl nm pos' strs
                          --- LabelPropsArgs ::= '{' ADLidListList '}'
                          pArgs = pSpec '{' *> pList1Sep (pSpec ',') (pList1 pADLid) <* pSpec '}'

--- Label ::= ADLid ':'
pLabel :: AmpParser Label
pLabel       = lbl <$> pADLid_val_pos <*  pKey ":"
               where lbl :: (String, Origin) -> Label
                     lbl (nm,pos') = Lbl nm pos' []
--- Content ::= '[' RecordList? ']' | '[' RecordObsList? ']'
pContent :: AmpParser Pairs
pContent          = pSpec '[' *> pListSep pComma pRecord <* pSpec ']'
                <|> pSpec '[' *> pListSep (pKey ";") pRecordObs <* pSpec ']' --obsolete
    where
    --- RecordList ::= Record (',' Record)*
    --- Record ::= String '*' String
    pRecord = mkPair<$> pString <* pKey "*" <*> pString
    --- RecordObsList ::= RecordObsList (';' RecordObsList)
    --- RecordObs ::= '(' String ',' String ')'
    pRecordObs = mkPair<$ pSpec '(' <*> pString <* pComma   <*> pString <* pSpec ')' --obsolete

--- ADLid ::= Varid | Conid | String
--- ADLidList ::= ADLid (',' ADLid)*
--- ADLidListList ::= ADLid+ (',' ADLid+)*
pADLid :: AmpParser String
pADLid            = pVarid <|> pConid <|> pString

pADLid_val_pos :: AmpParser (String, Origin)
pADLid_val_pos    = pVarid_val_pos <|> pConid_val_pos <|> pString_val_pos

pMaybe :: AmpParser a -> AmpParser (Maybe a)
pMaybe p = Just <$> p <|> pSucceed Nothing

-- Gets the location of the token in the file
get_tok_pos :: Token -> Origin
get_tok_pos (src,tok) = FileLoc(FilePos (sourceName src,src,show tok))

-- Gets the location of the token in the file and it's value
get_tok_val_pos :: Token -> (String, Origin)
get_tok_val_pos tok = (show tok, get_tok_pos tok)

-- Key has no EBNF because in EBNF it's just the given keyword.
pKey_pos :: String -> AmpParser Origin
pKey_pos = pKey

-- Spec just matches the given character so it has no EBNF
pSpec_pos :: Char -> AmpParser Origin
pSpec_pos = pSpec

pString_val_pos, pVarid_val_pos, pConid_val_pos, pAtom_val_pos ::  AmpParser (String,Origin)
pString_val_pos    =   pString
pVarid_val_pos     =   pVarid
pConid_val_pos     =   pConid
pAtom_val_pos      =   L.pAtom

pKey_val_pos ::  String -> AmpParser (String,Origin)
pKey_val_pos keyword = pKey

--   pSpec_val_pos ::  IsParser p Token => Char -> p (String,Origin)
--   pSpec_val_pos s      = gsym_val_pos TkSymbol    [s]       [s]