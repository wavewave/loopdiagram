{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>),(<*>))
import Control.Monad ((>=>))
import Data.Foldable (msum, foldrM)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio 
-- 
import Debug.Trace 

data Gen = G1 | G2 | G3 
         deriving (Show,Eq,Ord,Enum) 

-- data SuperfieldKind = SF_SM_U | SF_SM_Uc | SF_SM_D | SF_SM_Dc | SF_SM_v | SF_SM_E | SF_SM_Ec
 
data Scalar
data Fermion 

data SF a where 
  S :: SF Scalar
  F :: SF Fermion 

instance Show (SF a) where
  show S = "S"
  show F = "F" 

instance Eq (SF a) where 
  S == S = True 
  F == F = True 
  _ == _ = False 

instance Ord (SF a) where 
  compare S S = EQ
  compare S _ = LT 
  compare F F = EQ
  compare F _ = GT  


data Kind a b = SM_U a b | SM_Uc a b | SM_D a b | SM_Dc a b | SM_v a b | SM_E a b | SM_Ec a b
              | NP_X a 
              | NP_D a | NP_Dc a 
              | NP_U a | NP_Uc a 
              | NP_E a | NP_Ec a
              | NP_Qu a | NP_Quc a | NP_Qd a | NP_Qdc a 
              | NP_Lv a | NP_Lvc a | NP_Le a | NP_Lec a   
           deriving (Show,Eq,Ord)

conjugateParticle :: Kind a b -> Maybe (Kind a b)
conjugateParticle (SM_U  a b) = Just (SM_Uc a b)
conjugateParticle (SM_Uc a b) = Just (SM_U  a b)
conjugateParticle (SM_D  a b) = Just (SM_Dc a b)
conjugateParticle (SM_Dc a b) = Just (SM_D  a b)
conjugateParticle (SM_v  a b) = Nothing
conjugateParticle (SM_E  a b) = Just (SM_Ec a b)
conjugateParticle (SM_Ec a b) = Just (SM_E  a b)
conjugateParticle (NP_X a)    = Nothing
conjugateParticle (NP_D a)    = Just (NP_Dc a)
conjugateParticle (NP_Dc a)   = Just (NP_D a)
conjugateParticle (NP_U a)    = Just (NP_Uc a)
conjugateParticle (NP_Uc a)   = Just (NP_U a)
conjugateParticle (NP_E a)    = Just (NP_Ec a)
conjugateParticle (NP_Ec a)   = Just (NP_E a)
conjugateParticle (NP_Qu a)   = Just (NP_Quc a)
conjugateParticle (NP_Quc a)  = Just (NP_Qu a)
conjugateParticle (NP_Qd a)   = Just (NP_Qdc a)
conjugateParticle (NP_Qdc a)  = Just (NP_Qd a)
conjugateParticle (NP_Lv a)   = Just (NP_Lvc a)
conjugateParticle (NP_Lvc a)  = Just (NP_Lv a)
conjugateParticle (NP_Le a)   = Just (NP_Lec a)
conjugateParticle (NP_Lec a)  = Just (NP_Le a)


getSF :: Kind a b -> a 
getSF (SM_U  a _) = a
getSF (SM_Uc a _) = a
getSF (SM_D  a _) = a
getSF (SM_Dc a _) = a
getSF (SM_v  a _) = a
getSF (SM_E  a _) = a
getSF (SM_Ec a _) = a
getSF (NP_X a)    = a
getSF (NP_D a)    = a
getSF (NP_Dc a)   = a
getSF (NP_U a)    = a
getSF (NP_Uc a)   = a
getSF (NP_E a)    = a
getSF (NP_Ec a)   = a
getSF (NP_Qu a)   = a
getSF (NP_Quc a)  = a
getSF (NP_Qd a)   = a
getSF (NP_Qdc a)  = a
getSF (NP_Lv a)   = a
getSF (NP_Lvc a)  = a
getSF (NP_Le a)   = a
getSF (NP_Lec a)  = a



type FieldKind = Kind () ()

type PtlKind a = Kind (SF a) Gen 

-- deriving instance Show (PtlKind a)

data Dir = I | O 
         deriving (Show,Eq,Ord)


data SuperPot3 = SuperPot3 FieldKind FieldKind FieldKind 

assignFS :: SF a -> Kind () b -> Kind (SF a) b
assignFS sf (SM_U () b)  = SM_U sf b 
assignFS sf (SM_Uc () b) = SM_Uc sf b 
assignFS sf (SM_D () b)  = SM_D sf b
assignFS sf (SM_Dc () b) = SM_Dc sf b
assignFS sf (SM_v () b)  = SM_v sf b
assignFS sf (SM_E () b)  = SM_E sf b
assignFS sf (SM_Ec () b) = SM_Ec sf b
assignFS sf (NP_X ())    = NP_X sf
assignFS sf (NP_D ())    = NP_D sf
assignFS sf (NP_Dc ())   = NP_Dc sf
assignFS sf (NP_U ())    = NP_U sf
assignFS sf (NP_Uc ())   = NP_Uc sf
assignFS sf (NP_E ())    = NP_E sf
assignFS sf (NP_Ec ())   = NP_Ec sf
assignFS sf (NP_Qu ())   = NP_Qu sf
assignFS sf (NP_Quc ())  = NP_Quc sf
assignFS sf (NP_Qd ())   = NP_Qd sf
assignFS sf (NP_Qdc ())  = NP_Qdc sf
assignFS sf (NP_Lv ())   = NP_Lv sf
assignFS sf (NP_Lvc ())  = NP_Lvc sf
assignFS sf (NP_Le ())   = NP_Le sf
assignFS sf (NP_Lec ())  = NP_Lec sf

instance Functor (Kind a) where 
  -- fmap :: (a->b) -> Kind a b -> Kind a c
  fmap f (SM_U  a b) = SM_U  a (f b)
  fmap f (SM_Uc a b) = SM_Uc a (f b)
  fmap f (SM_D  a b) = SM_D  a (f b)
  fmap f (SM_Dc a b) = SM_Dc a (f b)
  fmap f (SM_v  a b) = SM_v  a (f b)
  fmap f (SM_E  a b) = SM_E  a (f b)
  fmap f (SM_Ec a b) = SM_Ec a (f b) 
  fmap f (NP_X a)    = NP_X a
  fmap f (NP_D a)    = NP_D a
  fmap f (NP_Dc a)   = NP_Dc a
  fmap f (NP_U a)    = NP_U a
  fmap f (NP_Uc a)   = NP_Uc a
  fmap f (NP_E a)    = NP_E a
  fmap f (NP_Ec a)   = NP_Ec a
  fmap f (NP_Qu a)   = NP_Qu a
  fmap f (NP_Quc a)  = NP_Quc a
  fmap f (NP_Qd a)   = NP_Qd a
  fmap f (NP_Qdc a)  = NP_Qdc a
  fmap f (NP_Lv a)   = NP_Lv a
  fmap f (NP_Lvc a)  = NP_Lvc a
  fmap f (NP_Le a)   = NP_Le a
  fmap f (NP_Lec a)  = NP_Lec a

assignGen :: Gen -> Kind a () -> Kind a Gen
assignGen g = fmap (const g)

superpotXQLD :: [SuperPot3] 
superpotXQLD = [ SuperPot3 (NP_X ())     (SM_Dc () ()) (NP_D ())
               , SuperPot3 (NP_Dc ())    (SM_U  () ()) (SM_E () ())
               , SuperPot3 (NP_Dc ())    (SM_D  () ()) (SM_v () ())
               , SuperPot3 (NP_X ())     (SM_v  () ()) (NP_Lvc ()) 
               , SuperPot3 (NP_X ())     (SM_E  () ()) (NP_Lec ())
               , SuperPot3 (NP_Lv ())    (SM_D  () ()) (SM_Dc () ())
               , SuperPot3 (NP_Le ())    (SM_U  () ()) (SM_Dc () ()) 
               , SuperPot3 (NP_X ())     (SM_U  () ()) (NP_Quc ()) 
               , SuperPot3 (NP_X ())     (SM_D  () ()) (NP_Qdc ())
               , SuperPot3 (NP_Qu ())    (SM_E  () ()) (SM_Dc () ())
               , SuperPot3 (NP_Qd ())    (SM_v  () ()) (SM_Dc () ())
               ] 

class EMChargeable a where 
  emcharge :: a -> Ratio Integer 

instance EMChargeable (Kind a b) where 
  -- emcharge :: Kind a b -> Ratio Integer
  emcharge (SM_U  _ _) =    2 % 3
  emcharge (SM_Uc _ _) = - (2 % 3)
  emcharge (SM_D  _ _) = - (1 % 3)
  emcharge (SM_Dc _ _) =    1 % 3 
  emcharge (SM_v  _ _) =    0 
  emcharge (SM_E  _ _) = -  1 
  emcharge (SM_Ec _ _) = 1 
  emcharge (NP_X  _)   = 0 
  emcharge (NP_D  _)   = - (1 % 3) 
  emcharge (NP_Dc _)   =    1 % 3 
  emcharge (NP_U  _)   =    2 % 3 
  emcharge (NP_Uc _)   = - (2 % 3)
  emcharge (NP_E  _)   = -  1 
  emcharge (NP_Ec _)   =    1 
  emcharge (NP_Qu  _)  =    2 % 3 
  emcharge (NP_Quc _)  = - (2 % 3)
  emcharge (NP_Qd  _)  = - (1 % 3)
  emcharge (NP_Qdc _)  =    1 % 3
  emcharge (NP_Lv  _)  =    0 
  emcharge (NP_Lvc _)  =    0 
  emcharge (NP_Le  _)  = -  1
  emcharge (NP_Lec _)  =    1

instance EMChargeable SuperPot3 where 
  emcharge (SuperPot3 x y z) = emcharge x + emcharge y + emcharge z 


data VertexFFS a = VertexFFS (Kind (SF Fermion) a,Dir) (Kind (SF Fermion) a, Dir) (Kind (SF Scalar) a, Dir) 

deriving instance (Show a) => Show (VertexFFS a)
 
superpot3toVertexFFS :: SuperPot3 -> [VertexFFS ()]
superpot3toVertexFFS (SuperPot3 x y z) =
  [ VertexFFS (assignFS F x,I) (assignFS F y,I) (assignFS S z,I)
  , VertexFFS (assignFS F y,I) (assignFS F z,I) (assignFS S x,I)
  , VertexFFS (assignFS F z,I) (assignFS F x,I) (assignFS S y,I)
  , VertexFFS (assignFS F x,I) (assignFS F z,I) (assignFS S y,I)
  , VertexFFS (assignFS F z,I) (assignFS F y,I) (assignFS S x,I)
  , VertexFFS (assignFS F y,I) (assignFS F x,I) (assignFS S z,I)
  , VertexFFS (assignFS F x,O) (assignFS F y,O) (assignFS S z,O)
  , VertexFFS (assignFS F y,O) (assignFS F z,O) (assignFS S x,O)
  , VertexFFS (assignFS F z,O) (assignFS F x,O) (assignFS S y,O)
  , VertexFFS (assignFS F x,O) (assignFS F z,O) (assignFS S y,O)
  , VertexFFS (assignFS F z,O) (assignFS F y,O) (assignFS S x,O)
  , VertexFFS (assignFS F y,O) (assignFS F x,O) (assignFS S z,O)
  ]


assignGenToVertexFFS :: VertexFFS () -> [VertexFFS Gen]
assignGenToVertexFFS (VertexFFS (k1,io1) (k2,io2) (k3,io3)) = do 
  let gen = [ G1, G2, G3 ] 
  k'1 <- (nub . map (flip assignGen k1)) gen 
  k'2 <- (nub . map (flip assignGen k2)) gen 
  k'3 <- (nub . map (flip assignGen k3)) gen 
  return (VertexFFS (k'1,io1) (k'2,io2) (k'3,io3))
 
data External = forall a. External { extKind :: PtlKind a, extDir :: Dir }
          
instance Show External where 
  show (External k d) = "External " ++ show k ++ " " ++ show d

data Handle = forall a. Handle (Kind (SF a) Gen,Dir)

instance Show Handle where
  show (Handle (k,d)) = "Handle " ++ show k ++ " " ++ show d 

-- deriving instance Eq External 

-- deriving instance Ord External 


data Partition = I2 | I3 | I4 
               deriving (Show,Eq,Ord) 
 
data Comb a b = Comb a b -- Partition Partition

deriving instance (Eq a, Eq b) => Eq (Comb a b) 

deriving instance (Show a, Show b) => Show (Comb a b) 

deriving instance (Ord a, Ord b) => Ord (Comb a b) 

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

data Externals = Externals { extPtl1 :: External
                           , extPtl2 :: External
                           , extPtl3 :: External
                           , extPtl4 :: External } 
               deriving (Show)

data Blob comb = Blob { blobExternals :: Externals 
                      , blobComb :: comb } 
               deriving (Show)

{- 
 External External External External comb
               deriving (Show) --  ,Eq,Ord)
-}

-- data VertexFFS = VertexFFS (Label,Dir) (Label,Dir) (Label,Dir) 

isValidComb :: Comb Partition Partition -> Bool  
isValidComb (Comb p1 p2) = p1 /= p2 

makePair :: Partition -> (VLabel,VLabel)
makePair I2 = (V1,V2)
makePair I3 = (V1,V3)
makePair I4 = (V1,V4) 

makePairComplement :: Partition -> (VLabel,VLabel) 
makePairComplement I2 = (V3,V4)
makePairComplement I3 = (V2,V4)
makePairComplement I4 = (V2,V3)


makeFPair :: (FDir,FDir) -> Partition -> (FLine (),FLine ())   
makeFPair (dir1,dir2) p = 
  let n1 = makePair p 
      n2 = makePairComplement p 
  in (FL n1 dir1 (), FL n2 dir2 ())

makeSPair :: (Dir,Dir) -> Partition -> (SLine (),SLine ()) 
makeSPair (dir1,dir2) p = 
  let n1 = makePair p 
      n2 = makePairComplement p 
  in (SL n1 dir1 (), SL n2 dir2 ()) 



snumber :: External -> Int 
snumber (External l d) = 
  let s' = case l of 
             SM_D _ G2 -> 1 
             SM_Dc _ G2 -> -1 
             _ -> 0 
      sgn = case d of 
              I -> -1
              O -> 1 
  in sgn * s' 

deltaS :: Blob a -> Int 
deltaS (Blob (Externals p1 p2 p3 p4) _) = (sum . map snumber) [p1,p2,p3,p4]
 
quarkSc = SM_Dc F G2 

quarkS = SM_D F G2 

quarkDc = SM_Dc F G1 

quarkD = SM_D F G1 

io_bar_sR_Gamma_dR_squared :: Blob () 
io_bar_sR_Gamma_dR_squared = 
  Blob (Externals (External quarkSc I) (External quarkSc I) (External quarkDc O) (External quarkDc O)) ()




-- allcomb = [ Comb I2 I3, Comb I2 I4, Comb I3 I2, Comb I3 I4, Comb I4 I2, Comb I4 I3] 

allcomb = [ x | p1 <- [I2,I3,I4], p2 <- [I2,I3,I4], let x = Comb p1 p2, isValidComb x ] 

alldir = [I, O] 

allfdir = [FDir d havemass | d <- alldir, havemass <- [False, True] ] 

allfline p = [makeFPair (d1,d2) p | d1 <- allfdir, d2 <- allfdir] 

allsline p = [makeSPair (d1,d2) p | d1 <- alldir, d2 <- alldir ] 

makeAllCombFromPartition :: Comb Partition Partition -> [Comb (FLine (),FLine ()) (SLine (),SLine ())] 
makeAllCombFromPartition (Comb p1 p2) = 
   [Comb fpair spair | fpair <- allfline p1, spair <- allsline p2] 

makeAllBlob :: Blob () -> [Blob (Comb (FLine (),FLine ()) (SLine (),SLine ()))]
makeAllBlob (Blob (Externals pt1 pt2 pt3 pt4) ()) = 
  Blob (Externals pt1 pt2 pt3 pt4) <$>  [x | c <- allcomb, x <- makeAllCombFromPartition c ]

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

vertexDirection :: Blob (Comb (FLine (),FLine ()) (SLine (), SLine ())) -> VLabel -> (Dir,Dir,Dir)
vertexDirection (Blob (Externals p1 p2 p3 p4) (Comb (f1,f2) (s1,s2))) v = 
  let d1 = case v of 
             V1 -> extDir p1
             V2 -> extDir p2
             V3 -> extDir p3 
             V4 -> extDir p4 
      d2 = (fromJust . msum . map (flip fermionVertexDir v)) [f1,f2]
      d3 = (fromJust . msum . map (flip scalarVertexDir v)) [s1,s2] 
  in (d1,d2,d3)

matchDirection :: [(Dir,Dir,Dir)] -> Blob (Comb (FLine (),FLine ()) (SLine (),SLine ())) -> Bool 
matchDirection dirs blob = all (\x -> (vertexDirection blob x) `elem` dirs) [V1,V2,V3,V4]


findVertexEdgeRel :: Blob (Comb (FLine (),FLine ()) (SLine (),SLine ())) -> VLabel
                  -> (VLabel, ((Int,Dir),(Int,Dir)))
findVertexEdgeRel (Blob (Externals _ _ _ _) (Comb (f1,f2) (s1,s2))) v = 
  case (hasVertex f1 v, hasVertex s1 v, hasVertex f2 v, hasVertex s2 v) of 
    (Just iof, Just ios, Nothing, Nothing) -> (v,((1,iof),(1,ios)))
    (Just iof, Nothing, Nothing, Just ios) -> (v,((1,iof),(2,ios)))
    (Nothing, Just ios, Just iof, Nothing) -> (v,((2,iof),(1,ios)))
    (Nothing, Nothing, Just iof, Just ios) -> (v,((2,iof),(2,ios)))




makeVertexEdgeMap :: Blob (Comb (FLine (),FLine ()) (SLine (),SLine ())) 
                  -> M.Map VLabel ((Int,Dir),(Int,Dir))
makeVertexEdgeMap blob = let lst = map (findVertexEdgeRel blob) [V1,V2,V3,V4] 
                         in M.fromList lst 

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


type MatchF = Either (FLine ()) (FLine (PtlKind Fermion))

type MatchS = Either (SLine ()) (SLine (PtlKind Scalar))


matchFSLines :: ((Int,Dir),(Int,Dir)) 
             -> [Handle]
             -> Comb (MatchF, MatchF) (MatchS, MatchS) 
             -> Maybe (Comb (MatchF, MatchF) (MatchS, MatchS))
matchFSLines emap hs c = foldrM (matchFSLinesWorker emap) c hs

matchFSLinesWorker :: ((Int,Dir),(Int,Dir)) 
                   -> Handle
                   -> Comb (MatchF, MatchF) (MatchS, MatchS) 
                   -> Maybe (Comb (MatchF, MatchF) (MatchS, MatchS))
matchFSLinesWorker ((i,iof),(j,ios)) (Handle (k1,d1)) c@(Comb (f1,f2) (s1,s2)) = 
  case getSF k1 of 
    S -> if | j == 1 -> 
              case s1 of 
                Left s@(SL (v1,v2) d ()) ->
                  let ms' = if matchVertexSLineDir (ios,d1) s then Just (SL (v1,v2) d k1) else Nothing
                  in maybe Nothing (\s'->Just (Comb (f1,f2) (Right s',s2))) ms' 
                Right s@(SL (v1,v2) d k) -> 
                  if matchVertexSLineDir (ios,d1) s && k == k1 then Just c else Nothing
            | j == 2 ->
              case s2 of 
                Left s@(SL (v1,v2) d ()) ->
                  let ms' = if matchVertexSLineDir (ios,d1) s then Just (SL (v1,v2) d k1) else Nothing
                  in maybe Nothing (\s'->Just (Comb (f1,f2) (s1,Right s'))) ms' 
                Right s@(SL (v1,v2) d k) -> 
                  if matchVertexSLineDir (ios,d1) s && k == k1 then Just c else Nothing
            | otherwise -> (error "error in matchFSLines")
    F -> if | i == 1 -> 
              case f1 of 
                Left f@(FL (v1,v2) d ()) ->
                  let mf' = if matchVertexFLineDir (iof,d1) f then Just (FL (v1,v2) d k1) else Nothing
                  in maybe Nothing (\f'->Just (Comb (Right f',f2) (s1,s2))) mf'
                Right f@(FL (v1,v2) d k) -> 
                  if matchVertexFLineDir (iof,d1) f && k == k1 then Just c else Nothing
            | i == 2 ->
              case f2 of 
                Left f@(FL (v1,v2) d ()) -> do 
                  k' <- getFermionKind iof d k1 
                  f' <- if matchVertexFLineDir (iof,d1) f 
                          then return (FL (v1,v2) d k') 
                          else Nothing
                  return (Comb (f1,Right f') (s1,s2))
                Right f@(FL (v1,v2) d k) -> do
                  k' <- getFermionKind iof d k1
                  if matchVertexFLineDir (iof,d1) f && k == k'
                    then return c 
                    else Nothing
            | otherwise -> (error "error in matchFSLines")

getFermionKind :: Dir -> FDir -> PtlKind Fermion -> Maybe (PtlKind Fermion)
getFermionKind _ (FDir d False) k1 = Just k1
getFermionKind I (FDir I True) k1 = Just k1 
getFermionKind O (FDir O True) k1 = Just k1
getFermionKind I (FDir O True) k1 = conjugateParticle k1 
getFermionKind O (FDir I True) k1 = conjugateParticle k1

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
  || (iof == I && d == I && hasmass && d1 == I) 
  || (iof == I && d == O && hasmass && d1 == O) 
  || (iof == O && d == I && hasmass && d1 == O)
  || (iof == O && d == O && hasmass && d1 == I)



{-
data Exts a = Exts { extsPtl1 :: (External, a)
                   , extsPtl2 :: (External, a)
                   , extsPtl3 :: (External, a)
                   , extsPtl4 :: (External, a) }
-}


{-
match :: Exts [VertexFFS Gen] -> M.Map VLabel (FLine,SLine) -> Bool 
match exts vmap = 
-} 


{-
tryVertex :: (VLabel,[Handle]External -> VertexFFS Gen -> VLabel -> M.Map VLabel -> Bool 
tryVertex p1 vtx v m = 
  p1 
-}

liftComb :: Comb (FLine (),FLine ()) (SLine (),SLine ()) -> Comb (MatchF, MatchF) (MatchS, MatchS)
liftComb (Comb (f1,f2) (s1,s2)) = Comb (Left f1, Left f2) (Left s1, Left s2)

data HandleSet = HandleSet { hsetVtx1Int :: [[Handle]] 
                           , hsetVtx2Int :: [[Handle]]
                           , hsetVtx3Int :: [[Handle]]
                           , hsetVtx4Int :: [[Handle]] }

prepareHandleSet :: [SuperPot3] -> Externals -> HandleSet 
prepareHandleSet superpots externals = 
  let vertexFFSwoGen = concatMap superpot3toVertexFFS  superpotXQLD 
      vertexFFSwGen = concatMap assignGenToVertexFFS vertexFFSwoGen
      (e1,e2,e3,e4) = ((,,,) <$> extPtl1 <*> extPtl2 <*> extPtl3 <*> extPtl4) externals
      vset1 = (map snd . selectVertexForExt e1) vertexFFSwGen
      vset2 = (map snd . selectVertexForExt e2) vertexFFSwGen
      vset3 = (map snd . selectVertexForExt e3) vertexFFSwGen
      vset4 = (map snd . selectVertexForExt e4) vertexFFSwGen
  in HandleSet { hsetVtx1Int = vset1 
               , hsetVtx2Int = vset2 
               , hsetVtx3Int = vset3
               , hsetVtx4Int = vset4 }
               
match :: HandleSet 
      -> Blob (Comb (FLine (), FLine ()) (SLine (), SLine ())) 
      -> Maybe (Blob (Comb (MatchF, MatchF) (MatchS, MatchS)))
match HandleSet{..} b@(Blob e c) = do
  let vemap = makeVertexEdgeMap b
  e1 <- M.lookup V1 vemap
  e2 <- M.lookup V2 vemap 
  e3 <- M.lookup V3 vemap 
  e4 <- M.lookup V4 vemap 
  let lc = liftComb c 
  let allhsets = [(h1,h2,h3,h4)| h1<-hsetVtx1Int, h2<-hsetVtx2Int, h3<-hsetVtx3Int, h4<-hsetVtx4Int] 
      (h1',h2',h3',h4') = head allhsets 
  lc' <- (matchFSLines e1 h1' >=> matchFSLines e2 h2' >=> matchFSLines e3 h3' >=> matchFSLines e4 h4') lc


  return (Blob e lc')
  -- trace (unlines [show e1, show e2, show e3, show e4]) $ Nothing 



main2 = do 
  putStrLn "superpotential test"
  mapM_ print $ map emcharge superpotXQLD
  let vertexFFSwoGen = concatMap superpot3toVertexFFS  superpotXQLD 
      vertexFFSwGen = concatMap assignGenToVertexFFS vertexFFSwoGen
  -- mapM_ (\x -> mapM_ print x >> putStrLn "----" ) $ map assignGenToVertexFFS vertexFFSwoGen
  let Blob (Externals e1 e2 e3 e4) _ = io_bar_sR_Gamma_dR_squared
  mapM_ print $ (selectVertexForExt e1 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ (selectVertexForExt e2 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ (selectVertexForExt e3 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ (selectVertexForExt e4 vertexFFSwGen)


main = do 
  putStrLn "loop" 
  print $ deltaS io_bar_sR_Gamma_dR_squared 
  -- let Blob _ _ _ _ cmb = test 
  -- print $ isValidComb cmb
  -- print ( allcomb' == allcomb)
  let allblobs = makeAllBlob io_bar_sR_Gamma_dR_squared
{-      matchedblobs =  filter (matchDirection [(I,I,I),(I,I,O),(O,O,I),(O,O,O)]) allblobs

  mapM_ print matchedblobs -}
  print (head allblobs)
  let blob = io_bar_sR_Gamma_dR_squared
      hset = prepareHandleSet superpotXQLD (blobExternals blob)

  print $ match hset (head allblobs)

{-       vertexFFSwoGen = concatMap superpot3toVertexFFS  superpotXQLD 
      vertexFFSwGen = concatMap assignGenToVertexFFS vertexFFSwoGen

      vset1 = (map snd . selectVertexForExt e1) vertexFFSwGen
      vset2 = (map snd . selectVertexForExt e2) vertexFFSwGen
      vset3 = (map snd . selectVertexForExt e3) vertexFFSwGen
      vset4 = (map snd . selectVertexForExt e4) vertexFFSwGen

      testblob = head allblobs 
      vemap = makeVertexEdgeMap testblob 
  -}
  {- 

  print vset1 
  let Just info = (M.lookup V1 vemap)

  print $ matchFSLines (head (head vset1)) info ((liftComb. blobComb) testblob) 
  -- mapM_ print (map makeVertexEdgeMap allblobs)
  -}
