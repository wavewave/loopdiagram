{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}


-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.LoopCalculation.Graph
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Physics.LoopCalculation.Graph where 

import Control.Applicative ((<$>))
import Data.List (nub) 
import Data.Maybe (mapMaybe)
import Data.Ratio (Ratio(..),(%))
-- 
data Scalar
data Fermion 

data SF a where 
  S :: SF Scalar
  F :: SF Fermion 

instance Show (SF a) where
  show S = "S"
  show F = "F" 

instance Eq (SF a) where 
 -- a == b = show a == show b 
  S == S = True 
  F == F = True 
  _ == _ = False  


instance Ord (SF a) where 
  compare S S = EQ
  compare S _ = LT 
  compare F F = EQ
  compare F _ = GT  

data Dir = I | O 
         deriving (Show,Eq,Ord)


data Gen = G1 | G2 | G3 | GAll
         deriving (Show,Eq,Ord,Enum) 


data SFKind b = SM_U b | SM_Uc b | SM_D b | SM_Dc b | SM_v b | SM_E b | SM_Ec b
              | NP_X  
              | NP_D   | NP_Dc  
              | NP_U   | NP_Uc  
              | NP_E   | NP_Ec 
              | NP_Qu  | NP_Quc  | NP_Qd  | NP_Qdc  
              | NP_Lv  | NP_Lvc  | NP_Le  | NP_Lec 
           deriving (Show,Eq,Ord)

instance Functor SFKind where 
  -- fmap :: (a->b) -> Kind a b -> Kind a c
  fmap f (SM_U  b) = SM_U  (f b)
  fmap f (SM_Uc b) = SM_Uc (f b)
  fmap f (SM_D  b) = SM_D  (f b)
  fmap f (SM_Dc b) = SM_Dc (f b)
  fmap f (SM_v  b) = SM_v  (f b)
  fmap f (SM_E  b) = SM_E  (f b)
  fmap f (SM_Ec b) = SM_Ec (f b) 
  fmap f NP_X      = NP_X
  fmap f NP_D      = NP_D
  fmap f NP_Dc     = NP_Dc
  fmap f NP_U      = NP_U
  fmap f NP_Uc     = NP_Uc
  fmap f NP_E      = NP_E
  fmap f NP_Ec     = NP_Ec
  fmap f NP_Qu     = NP_Qu
  fmap f NP_Quc    = NP_Quc
  fmap f NP_Qd     = NP_Qd
  fmap f NP_Qdc    = NP_Qdc
  fmap f NP_Lv     = NP_Lv
  fmap f NP_Lvc    = NP_Lvc
  fmap f NP_Le     = NP_Le
  fmap f NP_Lec    = NP_Lec

type Kind a b = (a, SFKind b) 

conjugateKind :: SFKind b -> Maybe (SFKind b)
conjugateKind (SM_U  b) = Just (SM_Uc b)
conjugateKind (SM_Uc b) = Just (SM_U  b)
conjugateKind (SM_D  b) = Just (SM_Dc b)
conjugateKind (SM_Dc b) = Just (SM_D  b)
conjugateKind (SM_v  b) = Nothing
conjugateKind (SM_E  b) = Just (SM_Ec b)
conjugateKind (SM_Ec b) = Just (SM_E  b)
conjugateKind NP_X     = Nothing
conjugateKind NP_D     = Just NP_Dc 
conjugateKind NP_Dc    = Just NP_D 
conjugateKind NP_U     = Just NP_Uc 
conjugateKind NP_Uc    = Just NP_U 
conjugateKind NP_E     = Just NP_Ec 
conjugateKind NP_Ec    = Just NP_E 
conjugateKind NP_Qu    = Just NP_Quc 
conjugateKind NP_Quc   = Just NP_Qu 
conjugateKind NP_Qd    = Just NP_Qdc 
conjugateKind NP_Qdc   = Just NP_Qd 
conjugateKind NP_Lv    = Just NP_Lvc 
conjugateKind NP_Lvc   = Just NP_Lv 
conjugateKind NP_Le    = Just NP_Lec 
conjugateKind NP_Lec   = Just NP_Le 


conjugateParticle :: Kind a b -> Maybe (Kind a b)
conjugateParticle (sf,k) = (sf,) <$> conjugateKind k  

getSF :: Kind a b -> a 
getSF = fst 


type PtlKind a = Kind (SF a) Gen 

-- deriving instance Show (PtlKind a)

data SuperPot3 = SuperPot3 (SFKind ()) (SFKind ()) (SFKind ())

assignFS :: SF a -> Kind () b -> Kind (SF a) b
assignFS sf (_,x) = (sf,x)


assignGen :: Gen -> Kind a () -> Kind a Gen
assignGen g (sf,k) = (sf, fmap (const g) k)


class EMChargeable a where 
  emcharge :: a -> Ratio Integer 

instance EMChargeable (SFKind b) where 
  -- emcharge :: Kind a b -> Ratio Integer
  emcharge (SM_U  _) =    2 % 3
  emcharge (SM_Uc _) = - (2 % 3)
  emcharge (SM_D  _) = - (1 % 3)
  emcharge (SM_Dc _) =    1 % 3 
  emcharge (SM_v  _) =    0 
  emcharge (SM_E  _) = -  1 
  emcharge (SM_Ec _) = 1 
  emcharge NP_X     = 0 
  emcharge NP_D     = - (1 % 3) 
  emcharge NP_Dc    =    1 % 3 
  emcharge NP_U     =    2 % 3 
  emcharge NP_Uc    = - (2 % 3)
  emcharge NP_E     = -  1 
  emcharge NP_Ec    =    1 
  emcharge NP_Qu    =    2 % 3 
  emcharge NP_Quc   = - (2 % 3)
  emcharge NP_Qd    = - (1 % 3)
  emcharge NP_Qdc   =    1 % 3
  emcharge NP_Lv    =    0 
  emcharge NP_Lvc   =    0 
  emcharge NP_Le    = -  1
  emcharge NP_Lec   =    1

instance EMChargeable (Kind a b) where 
  emcharge (_,x) = emcharge x  

instance EMChargeable SuperPot3 where 
  emcharge (SuperPot3 x y z) = emcharge x + emcharge y + emcharge z 


data VertexFFS a = VertexFFS (Kind (SF Fermion) a,Dir) (Kind (SF Fermion) a, Dir) (Kind (SF Scalar) a, Dir) 

deriving instance (Show a) => Show (VertexFFS a)
 
superpot3toVertexFFS :: SuperPot3 -> [VertexFFS ()]
superpot3toVertexFFS (SuperPot3 x' y' z') =
    [ VertexFFS (assignFS F x,I) (assignFS F y,I) (assignFS S z,I)
    , VertexFFS (assignFS F y,I) (assignFS F z,I) (assignFS S x,I)
    , VertexFFS (assignFS F z,I) (assignFS F x,I) (assignFS S y,I)
    , VertexFFS (assignFS F x,O) (assignFS F y,O) (assignFS S z,O)
    , VertexFFS (assignFS F y,O) (assignFS F z,O) (assignFS S x,O)
    , VertexFFS (assignFS F z,O) (assignFS F x,O) (assignFS S y,O)
    ]
  where x = ((),x')
        y = ((),y')
        z = ((),z')

assignGenToVertexFFS :: VertexFFS () -> [VertexFFS Gen]
assignGenToVertexFFS (VertexFFS (k1,io1) (k2,io2) (k3,io3)) = do 
  let gen = [ G1, G2, G3 ] 
  k'1 <- (nub . map (flip assignGen k1)) gen 
  k'2 <- (nub . map (flip assignGen k2)) gen 
  k'3 <- (nub . map (flip assignGen k3)) gen 
  return (VertexFFS (k'1,io1) (k'2,io2) (k'3,io3))

data Handle = forall a. Handle (Kind (SF a) Gen,Dir)

instance Show Handle where
  show (Handle (k,d)) = "Handle " ++ show k ++ " " ++ show d 

instance Eq Handle where 
  Handle ((F,k1),d1) == Handle ((F,k2),d2) = k1 == k2 && d1 == d2 
  Handle ((S,k1),d1) == Handle ((S,k2),d2) = k1 == k2 && d1 == d2
  _ == _ = False 


instance Ord Handle where 
  compare (Handle ((F,k1),d1)) (Handle ((F,k2),d2)) = case compare k1 k2 of 
                                                        EQ -> compare d1 d2 
                                                        a -> a 
  compare (Handle ((F,k1),d1)) _ = GT 
  compare (Handle ((S,k1),d1)) (Handle ((S,k2),d2)) = case compare k1 k2 of 
                                                        EQ -> compare d1 d2 
                                                        a -> a
  compare (Handle ((S,k1),d1)) _ = LT 


data VLabel = V1 | V2 | V3 | V4 
            deriving (Show,Eq,Ord,Enum)

class Line l where 
  vertices :: l -> (VLabel,VLabel)

data FDir = FDir Dir Bool 
            deriving (Show,Eq,Ord) 

data FLine a = FL (VLabel,VLabel) FDir a 
           deriving (Show,Eq,Ord)

data SLine a = SL (VLabel,VLabel) Dir a
           deriving (Show,Eq,Ord)

instance Line (FLine a) where 
  vertices (FL vs _ _) = vs

instance Line (SLine a) where 
  vertices (SL vs _ _) = vs 

data External = forall a. External { extKind :: PtlKind a, extDir :: Dir }
          
instance Show External where 
  show (External k d) = "External " ++ show k ++ " " ++ show d

alldir = [I, O] 

allfdir = [FDir d havemass | d <- alldir, havemass <- [False, True] ] 

scalarVertexDir :: SLine () -> VLabel -> Maybe Dir 
scalarVertexDir (SL (v1,v2) d ()) v 
  | v == v1 && d == I = Just O  
  | v == v2 && d == I = Just I 
  | v == v1 && d == O = Just I
  | v == v2 && d == O = Just O 
  | otherwise = Nothing 

fermionVertexDir :: FLine () -> VLabel -> Maybe Dir 
fermionVertexDir (FL (v1,v2) (FDir d havemass) ()) v 
  | v == v1 && d == I && havemass     = Just O  
  | v == v1 && d == I && not havemass = Just O  
  | v == v2 && d == I && havemass     = Just O 
  | v == v2 && d == I && not havemass = Just I
  --  
  | v == v1 && d == O && havemass     = Just I
  | v == v1 && d == O && not havemass = Just I
  | v == v2 && d == O && havemass     = Just I 
  | v == v2 && d == O && not havemass = Just O 
  | otherwise = Nothing 

hasVertex :: Line l => l -> VLabel -> Maybe Dir
hasVertex l v 
    | v == v1 = Just I
    | v == v2 = Just O 
    | otherwise = Nothing 
  where (v1,v2) = vertices l 

selectVertexForExt :: External -> [VertexFFS Gen] -> [(Handle,[Handle])] 
selectVertexForExt (External k d) vs = 
    case getSF k of 
      S -> mapMaybe (checkScalar k d) vs  
      F -> mapMaybe (checkFermion k d) vs 
  where 
    -- 
    checkScalar :: PtlKind Scalar -> Dir -> VertexFFS Gen -> Maybe (Handle,[Handle])
    checkScalar k d (VertexFFS h1 h2 (k',d')) = 
      if k == k' && d == d' then Just (Handle (k',d'),[Handle h1, Handle h2]) else Nothing
    -- 
    checkFermion :: PtlKind Fermion -> Dir -> VertexFFS Gen -> Maybe (Handle,[Handle])
    checkFermion k d (VertexFFS (k1,d1) (k2,d2) h') 
      | k == k1 && d == d1 = Just (Handle (k1,d1),[Handle (k2,d2), Handle h'])
      | k == k2 && d == d2 = Just (Handle (k2,d2),[Handle (k1,d1), Handle h'])
      | otherwise = Nothing 

replaceInternalToGAll :: Handle -> Handle 
replaceInternalToGAll (Handle ((s,k),d)) = Handle ((s,fmap (const GAll) k),d) 

-- | 
getFermionKind :: Dir   -- ^ iof  
               -> FDir  -- ^ d in FLine 
               -> PtlKind Fermion 
               -> Maybe (PtlKind Fermion)
getFermionKind _ (FDir d False) k1 = Just k1
getFermionKind I (FDir _ True) k1 = Just k1 
getFermionKind O (FDir _ True) k1 = conjugateParticle k1 


-- | 
matchVertexSLineDir :: (Dir,Dir) -> SLine a -> Bool 
matchVertexSLineDir (ios,d1) (SL (v1,v2) d _) = 
  (ios == I && d == I && d1 == O) 
  || (ios == I && d == O && d1 == I) 
  || (ios == O && d == I && d1 == I)
  || (ios == O && d == O && d1 == O)
 
matchVertexFLineDir :: (Dir,Dir) -> FLine a -> Bool 
matchVertexFLineDir (iof,d1) (FL (v1,v2) (FDir d hasmass) _) = 
  (iof == I && d == I && not hasmass && d1 == O) 
  || (iof == I && d == O && not hasmass && d1 == I) 
  || (iof == O && d == I && not hasmass && d1 == I)
  || (iof == O && d == O && not hasmass && d1 == O)
  -- 
  || (iof == I && d == I && hasmass && d1 == O) 
  || (iof == I && d == O && hasmass && d1 == I) 
  || (iof == O && d == I && hasmass && d1 == O)
  || (iof == O && d == O && hasmass && d1 == I)
