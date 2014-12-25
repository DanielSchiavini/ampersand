module Database.Design.Ampersand.Basics.Collection
  (  Collection ( eleM
                , uni
                , isc
                ,(>-)
                ,empty
                ,elems)
  )where
   import Data.Set (Set)
   import qualified Data.Set as Set
  
   ----------------------------------------------
   ---- Collection of type a --------------------
   ----------------------------------------------
   infixl 5  >-

   class Collection a where      -- TODO Vervangen door efficient algorithme: Data.Set
    eleM :: Ord b => b -> a b -> Bool
    uni, isc :: Ord b => a b -> a b -> a b
    (>-) :: Ord b => a b -> a b -> a b
    empty :: Ord b => a b
    elems :: Ord b => a b -> [b]
    fromList :: Ord b => [b] -> a b

   instance Collection [] where
    eleM         = elem
    xs `uni` ys  = xs++(ys>-xs)
    xs `isc` ys  = [y | y<-ys, y `elem` xs]
    xs >- ys     = [x | x<-xs, x `notElem` ys]
    empty        = []
    elems        = id
    fromList     = id

   instance Collection Set where
    eleM         = Set.member
    xs `uni` ys  = Set.union xs ys
    xs `isc` ys  = Set.intersection xs ys
    xs >- ys     = Set.difference xs ys
    empty        = Set.empty
    elems        = Set.elems
    fromList     = Set.fromList
