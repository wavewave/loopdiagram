{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.LoopCalculation.Box
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Physics.LoopCalculation.Box where 

import Control.Applicative ((<$>),(<*>))
import Control.Monad ((>=>))
import Data.Foldable (msum, foldrM)
import Data.List (nub,sort)
import qualified Data.Map as M
import Data.Maybe (fromJust,mapMaybe)
--
import HEP.Physics.LoopCalculation.Graph 
--
import Debug.Trace 

 

data Partition = I2 | I3 | I4 
               deriving (Show,Eq,Ord) 
 
data Comb a b = Comb a b 

deriving instance (Eq a, Eq b) => Eq (Comb a b) 

deriving instance (Show a, Show b) => Show (Comb a b) 

deriving instance (Ord a, Ord b) => Ord (Comb a b) 

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

quarkCc = (F, SM_Uc G2)
quarkC = (F, SM_U G2)
quarkSc = (F, SM_Dc G2)
quarkS = (F, SM_D G2)
quarkUc = (F, SM_Uc G1)
quarkU = (F, SM_U G1)
quarkDc = (F, SM_Dc G1)
quarkD = (F, SM_D G1)

leptonMu = (F, SM_E G2)
leptonMuc= (F, SM_Ec G2)
leptonE = (F, SM_E G1)
leptonEc = (F, SM_Ec G1)
allcomb = [ x | p1 <- [I2,I3,I4], p2 <- [I2,I3,I4], let x = Comb p1 p2, isValidComb x ] 

allfline p = [makeFPair (d1,d2) p | d1 <- allfdir, d2 <- allfdir] 

allsline p = [makeSPair (d1,d2) p | d1 <- alldir, d2 <- alldir ] 

makeAllCombFromPartition :: Comb Partition Partition -> [Comb (FLine (),FLine ()) (SLine (),SLine ())] 
makeAllCombFromPartition (Comb p1 p2) = 
   [Comb fpair spair | fpair <- allfline p1, spair <- allsline p2] 

makeAllBlob :: Blob () -> [Blob (Comb (FLine (),FLine ()) (SLine (),SLine ()))]
makeAllBlob (Blob (Externals pt1 pt2 pt3 pt4) ()) = 
  Blob (Externals pt1 pt2 pt3 pt4) <$>  [x | c <- allcomb, x <- makeAllCombFromPartition c ]


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
                Left f@(FL (v1,v2) d ()) -> do 
                  k' <- getFermionKind iof d k1 
                  f' <- if matchVertexFLineDir (iof,d1) f 
                          then return (FL (v1,v2) d k') 
                          else Nothing
                  return (Comb (Right f',f2) (s1,s2))
                Right f@(FL (v1,v2) d k) -> do
                  k' <- getFermionKind iof d k1
                  if matchVertexFLineDir (iof,d1) f && k == k'
                    then return c 
                    else Nothing
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



liftComb :: Comb (FLine (),FLine ()) (SLine (),SLine ()) -> Comb (MatchF, MatchF) (MatchS, MatchS)
liftComb (Comb (f1,f2) (s1,s2)) = Comb (Left f1, Left f2) (Left s1, Left s2)

data HandleSet = HandleSet { hsetVtx1Int :: [[Handle]] 
                           , hsetVtx2Int :: [[Handle]]
                           , hsetVtx3Int :: [[Handle]]
                           , hsetVtx4Int :: [[Handle]] }

prepareHandleSet :: [SuperPot3] -> Externals -> HandleSet 
prepareHandleSet superpot externals = 
  let vertexFFSwoGen = concatMap superpot3toVertexFFS  superpot
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



