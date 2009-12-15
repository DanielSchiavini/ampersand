  module FspecDef
         ( module Data.Fspec
         , module CommonClasses
         , module Auxiliaries
         , module Strings 
         , Fidentified(..)
     --    , fspc_patterns
     --    , themesOfPatterns
         )

  where

   import Adl
   import CommonClasses(Identified(name,typ))
   import Auxiliaries(showL)
   import Strings(chain)
   import Data.Fspec

   --fspc_patterns :: Fspc -> Patterns
   --fspc_patterns spec = themesOfPatterns (themes spec)     
   --themesOfPatterns :: [Ftheme] -> [Pattern]
   --themesOfPatterns themes = [ftpat tm | tm <-themes]





   instance Fidentified Morphism where
     fsid m = FS_id (name m++name (source m)++name(target m))  --Hier moet nog goed naar worden gekeken....
 --        where 
 --          source (Mph nm pos atts (a,b) _ s) = a
 --          source m = error ("FspecDef.lhs : Cannot evaluate the source expression of the current morphism (yet)")
 --          target (Mph nm pos atts (a,b) _ s) = b    
 --          target m = error ("FspecDef.lhs : Cannot evaluate the target expression of the current morphism (yet)")
  --   typ m  = "f_morph"
   instance Fidentified Concept where
     fsid c = FS_id (name c)
  --   typ m  = "f_cpt"
   instance Fidentified ObjectDef where
     fsid o = FS_id (name o)
  --   typ m  = "f_objdef"


   class Fidentified a where
     fsid :: a -> FSid
   --  typ  :: a -> String



-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fspc                          ***
-- \***********************************************************************
   instance Fidentified Fspc where  -- WAAROM moet er een tweede vorm van Identified zijn? Dit is in tegenspraak met het principe van code-ontdubbeling.
    fsid    spec = fsfsid spec
  --  typ     _ = "f_Ctx"


-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: Fservice                         ***
-- \***********************************************************************
     
   instance Fidentified Fservice where
     fsid fservice = fsid (fsv_objectdef fservice)

-- \***********************************************************************
-- \*** Eigenschappen met betrekking tot: FSid                          ***
-- \***********************************************************************

