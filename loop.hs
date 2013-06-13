{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative ((<$>))
import Data.Foldable (msum)
import Data.Maybe (fromJust)

data Label = S | Sc | D | Dc 
           deriving (Show,Eq,Ord)

data Dir = I | O 
         deriving (Show,Eq,Ord)

data Ptl = Ptl { ptlLabel :: Label , ptlDir :: Dir }
         deriving (Show,Eq,Ord)

data Partition = I2 | I3 | I4 
               deriving (Show,Eq,Ord) 
 
data Comb a b = Comb a b -- Partition Partition

deriving instance (Eq a, Eq b) => Eq (Comb a b) 

deriving instance (Show a, Show b) => Show (Comb a b) 

deriving instance (Ord a, Ord b) => Ord (Comb a b) 

data VLabel = V1 | V2 | V3 | V4 
            deriving (Show,Eq,Ord,Enum)


data FDir = FDir Dir Bool 
            deriving (Show,Eq,Ord) 

data FLine = FL (VLabel,VLabel) FDir 
           deriving (Show,Eq,Ord)

data SLine = SL (VLabel,VLabel) Dir
           deriving (Show,Eq,Ord)

data Blob comb = Blob Ptl Ptl Ptl Ptl comb
               deriving (Show,Eq,Ord)

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



snumber :: Ptl -> Int 
snumber (Ptl l d) = 
  let s' = case l of 
             S -> 1 
             Sc -> -1 
             _ -> 0 
      sgn = case d of 
              I -> -1
              O -> 1 
  in sgn * s' 

deltaS :: Blob a -> Int 
deltaS (Blob  p1 p2 p3 p4 _) = (sum . map snumber) [p1,p2,p3,p4]
 
test :: Blob () -- (Comb Partition Partition)
test = Blob (Ptl Sc I) (Ptl Sc I) (Ptl Dc O) (Ptl Dc O) () --  (Comb I2 I3) 

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
makeAllBlob (Blob pt1 pt2 pt3 pt4 ()) = 
  Blob pt1 pt2 pt3 pt4 <$>  [x | c <- allcomb, x <- makeAllCombFromPartition c ]

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



vertexDirection :: Blob (Comb (FLine,FLine) (SLine,SLine)) -> VLabel -> (Dir,Dir,Dir)
vertexDirection (Blob p1 p2 p3 p4 (Comb (f1,f2) (s1,s2))) v = 
  let d1 = case v of 
             V1 -> ptlDir p1
             V2 -> ptlDir p2
             V3 -> ptlDir p3 
             V4 -> ptlDir p4 
      d2 = (fromJust . msum . map (flip fermionVertexDir v)) [f1,f2]
      d3 = (fromJust . msum . map (flip scalarVertexDir v)) [s1,s2] 
  in (d1,d2,d3)

matchDirection :: [(Dir,Dir,Dir)] -> Blob (Comb (FLine,FLine) (SLine,SLine)) -> Bool 
matchDirection dirs blob = all (\x -> (vertexDirection blob x) `elem` dirs) [V1,V2,V3,V4]


{-
vertexDirection (Blob p1 p2 p3 p4 (Comb (f1,f2) (s1,s2))) V2 = ptlDir p2
vertexDirection (Blob p1 p2 p3 p4 (Comb (f1,f2) (s1,s2))) V3 = ptlDir p3
vertexDirection (Blob p1 p2 p3 p4 (Comb (f1,f2) (s1,s2))) V4 = ptlDir p4
-}

main = do 
  putStrLn "loop" 
  print $ deltaS test 
  -- let Blob _ _ _ _ cmb = test 
  -- print $ isValidComb cmb
  -- print ( allcomb' == allcomb)
  let allblobs = makeAllBlob test
      matchedblobs =  filter (matchDirection [(I,I,I),(I,I,O),(O,O,I),(O,O,O)]) allblobs

  mapM_ print matchedblobs
