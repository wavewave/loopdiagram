{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative ((<$>))
import Data.Foldable (msum)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ratio 

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

data FLine = FL (VLabel,VLabel) FDir 
           deriving (Show,Eq,Ord)

data SLine = SL (VLabel,VLabel) Dir
           deriving (Show,Eq,Ord)

instance Line FLine where 
  vertices (FL vs _) = vs

instance Line SLine where 
  vertices (SL vs _) = vs 

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


makeFPair :: (FDir,FDir) -> Partition -> (FLine,FLine)   
makeFPair (dir1,dir2) p = 
  let n1 = makePair p 
      n2 = makePairComplement p 
  in (FL n1 dir1, FL n2 dir2)

makeSPair :: (Dir,Dir) -> Partition -> (SLine,SLine) 
makeSPair (dir1,dir2) p = 
  let n1 = makePair p 
      n2 = makePairComplement p 
  in (SL n1 dir1, SL n2 dir2) 



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

makeAllCombFromPartition :: Comb Partition Partition -> [Comb (FLine,FLine) (SLine,SLine)] 
makeAllCombFromPartition (Comb p1 p2) = 
   [Comb fpair spair | fpair <- allfline p1, spair <- allsline p2] 

makeAllBlob :: Blob () -> [Blob (Comb (FLine,FLine) (SLine,SLine))]
makeAllBlob (Blob (Externals pt1 pt2 pt3 pt4) ()) = 
  Blob (Externals pt1 pt2 pt3 pt4) <$>  [x | c <- allcomb, x <- makeAllCombFromPartition c ]

scalarVertexDir :: SLine -> VLabel -> Maybe Dir 
scalarVertexDir (SL (v1,v2) d) v 
  | v == v1 && d == I = Just O  
  | v == v2 && d == I = Just I 
  | v == v1 && d == O = Just I
  | v == v2 && d == O = Just O 
  | otherwise = Nothing 

fermionVertexDir :: FLine -> VLabel -> Maybe Dir 
fermionVertexDir (FL (v1,v2) (FDir d havemass)) v 
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

vertexDirection :: Blob (Comb (FLine,FLine) (SLine,SLine)) -> VLabel -> (Dir,Dir,Dir)
vertexDirection (Blob (Externals p1 p2 p3 p4) (Comb (f1,f2) (s1,s2))) v = 
  let d1 = case v of 
             V1 -> extDir p1
             V2 -> extDir p2
             V3 -> extDir p3 
             V4 -> extDir p4 
      d2 = (fromJust . msum . map (flip fermionVertexDir v)) [f1,f2]
      d3 = (fromJust . msum . map (flip scalarVertexDir v)) [s1,s2] 
  in (d1,d2,d3)

matchDirection :: [(Dir,Dir,Dir)] -> Blob (Comb (FLine,FLine) (SLine,SLine)) -> Bool 
matchDirection dirs blob = all (\x -> (vertexDirection blob x) `elem` dirs) [V1,V2,V3,V4]


findVertexEdgeRel :: Blob (Comb (FLine,FLine) (SLine,SLine)) -> VLabel -> (VLabel, (FLine,SLine))
findVertexEdgeRel (Blob (Externals _ _ _ _) (Comb (f1,f2) (s1,s2))) v = 
  case (hasVertex f1 v, hasVertex s1 v) of 
    (Just _, Just _)  -> (v, (f1,s1))
    (Just _, Nothing) -> (v, (f1,s2))
    (Nothing, Just _) -> (v, (f2,s1))
    (Nothing,Nothing) -> (v, (f2,s2))




makeVertexEdgeMap :: Blob (Comb (FLine,FLine) (SLine,SLine)) -> M.Map VLabel (FLine,SLine)
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

{-
data Exts a = Exts { extsPtl1 :: (External, a)
                   , extsPtl2 :: (External, a)
                   , extsPtl3 :: (External, a)
                   , extsPtl4 :: (External, a) }
-}

{-
matchFSLines :: Handle -> (FLine,SLine) -> Bool 
matchFSLines (Handle (k1,d1)) = 
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







main = do 
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


main2 = do 
  putStrLn "loop" 
  print $ deltaS io_bar_sR_Gamma_dR_squared 
  -- let Blob _ _ _ _ cmb = test 
  -- print $ isValidComb cmb
  -- print ( allcomb' == allcomb)
  let allblobs = makeAllBlob io_bar_sR_Gamma_dR_squared
{-      matchedblobs =  filter (matchDirection [(I,I,I),(I,I,O),(O,O,I),(O,O,O)]) allblobs

  mapM_ print matchedblobs -}
  print (head allblobs)
  print (makeVertexEdgeMap (head allblobs))
 
