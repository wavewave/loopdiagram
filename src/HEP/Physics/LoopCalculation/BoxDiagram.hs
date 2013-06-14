{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module HEP.Physics.LoopCalculation.BoxDiagram where 

import Control.Applicative ((<$>),(<*>))
import Control.Monad ((>=>))
import Data.Foldable (msum, foldrM)
import Data.List (nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio 
-- 
import Debug.Trace 

data Gen = G1 | G2 | G3 | GAll
         deriving (Show,Eq,Ord,Enum) 

 
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

data Dir = I | O 
         deriving (Show,Eq,Ord)


data SuperPot3 = SuperPot3 (SFKind ()) (SFKind ()) (SFKind ())

assignFS :: SF a -> Kind () b -> Kind (SF a) b
assignFS sf (_,x) = (sf,x)


assignGen :: Gen -> Kind a () -> Kind a Gen
assignGen g (sf,k) = (sf, fmap (const g) k)

superpotXQLD :: [SuperPot3] 
superpotXQLD = [ SuperPot3 NP_X  (SM_Dc ()) NP_D 
               , SuperPot3 NP_Dc (SM_U  ()) (SM_E ()) 
               , SuperPot3 NP_Dc (SM_D  ()) (SM_v ()) 
               , SuperPot3 NP_X  (SM_v  ()) NP_Lvc  
               , SuperPot3 NP_X  (SM_E  ()) NP_Lec 
               , SuperPot3 NP_Lv (SM_D  ()) (SM_Dc ())
               , SuperPot3 NP_Le (SM_U  ()) (SM_Dc ())  
               , SuperPot3 NP_X  (SM_U  ()) NP_Quc  
               , SuperPot3 NP_X  (SM_D  ()) NP_Qdc 
               , SuperPot3 NP_Qu (SM_E  ()) (SM_Dc ())
               , SuperPot3 NP_Qd (SM_v  ()) (SM_Dc ())
               ] 

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
 
data External = forall a. External { extKind :: PtlKind a, extDir :: Dir }
          
instance Show External where 
  show (External k d) = "External " ++ show k ++ " " ++ show d

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


data Partition = I2 | I3 | I4 
               deriving (Show,Eq,Ord) 
 
data Comb a b = Comb a b 

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
             (_, SM_D G2) -> 1 
             (_, SM_Dc G2) -> -1 
             _ -> 0 
      sgn = case d of 
              I -> -1
              O -> 1 
  in sgn * s' 

deltaS :: Blob a -> Int 
deltaS (Blob (Externals p1 p2 p3 p4) _) = (sum . map snumber) [p1,p2,p3,p4]
 
quarkSc = (F, SM_Dc G2)

quarkS = (F, SM_D G2)

quarkDc = (F, SM_Dc G1)

quarkD = (F, SM_D G1)

io_bar_sR_Gamma_dR_squared :: Blob () 
io_bar_sR_Gamma_dR_squared = 
  Blob (Externals (External quarkSc I)  (External quarkDc O)  (External quarkDc O) (External quarkSc I)) ()


io_bar_sL_Gamma_dL_squared :: Blob () 
io_bar_sL_Gamma_dL_squared = 
  Blob (Externals (External quarkS I)  (External quarkD O)  (External quarkD O) (External quarkS I)) ()


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

replaceInternalToGAll :: Handle -> Handle 
replaceInternalToGAll (Handle ((s,k),d)) = Handle ((s,fmap (const GAll) k),d) 


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
      vset1 = (nub . map (map replaceInternalToGAll . sort . snd) . selectVertexForExt e1) vertexFFSwGen
      vset2 = (nub . map (map replaceInternalToGAll . sort . snd) . selectVertexForExt e2) vertexFFSwGen
      vset3 = (nub . map (map replaceInternalToGAll . sort . snd) . selectVertexForExt e3) vertexFFSwGen
      vset4 = (nub . map (map replaceInternalToGAll . sort . snd) . selectVertexForExt e4) vertexFFSwGen
  in HandleSet { hsetVtx1Int = vset1 
               , hsetVtx2Int = vset2 
               , hsetVtx3Int = vset3
               , hsetVtx4Int = vset4 }
               
match :: HandleSet 
      -> Blob (Comb (FLine (), FLine ()) (SLine (), SLine ())) 
      -> [Comb (MatchF, MatchF) (MatchS, MatchS)]
match HandleSet{..} b@(Blob e c) = 
  let vemap = makeVertexEdgeMap b
      Just e1 = M.lookup V1 vemap
      Just e2 = M.lookup V2 vemap 
      Just e3 = M.lookup V3 vemap 
      Just e4 = M.lookup V4 vemap 
      lc = liftComb c 
      allhsets = [(h1,h2,h3,h4)| h1<-hsetVtx1Int, h2<-hsetVtx2Int, h3<-hsetVtx3Int, h4<-hsetVtx4Int] 
      matchForOneHandleCombination (h1',h2',h3',h4') = 
        (matchFSLines e1 h1' >=> matchFSLines e2 h2' >=> matchFSLines e3 h3' >=> matchFSLines e4 h4') lc
      lst = mapMaybe matchForOneHandleCombination allhsets 
      
  in lst 



