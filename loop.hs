{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative ((<$>))

data Label = S | Sc | D | Dc 
           deriving (Show,Eq,Ord)

data Dir = I | O 
         deriving (Show,Eq,Ord)

data Ptl = Ptl Label Dir 
         deriving (Show,Eq,Ord)

data Partition = I2 | I3 | I4 
               deriving (Show,Eq,Ord) 
 
data Comb a b = Comb a b -- Partition Partition

deriving instance (Eq a, Eq b) => Eq (Comb a b) 

deriving instance (Show a, Show b) => Show (Comb a b) 

deriving instance (Ord a, Ord b) => Ord (Comb a b) 


data FDir = FDir Dir Bool 
            deriving (Show,Eq,Ord) 

data FLine = FL (Int,Int) FDir 
           deriving (Show,Eq,Ord)

data SLine = SL (Int,Int) Dir
           deriving (Show,Eq,Ord)

data Blob comb = Blob Ptl Ptl Ptl Ptl comb
               deriving (Show,Eq,Ord)

isValidComb :: Comb Partition Partition -> Bool  
isValidComb (Comb p1 p2) = p1 /= p2 


makePair :: Partition -> (Int,Int)
makePair I2 = (1,2)
makePair I3 = (1,3)
makePair I4 = (1,4) 

makePairComplement :: Partition -> (Int,Int) 
makePairComplement I2 = (3,4)
makePairComplement I3 = (2,4)
makePairComplement I4 = (2,3)


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


main = do 
  putStrLn "loop" 
  print $ deltaS test 
  -- let Blob _ _ _ _ cmb = test 
  -- print $ isValidComb cmb
  -- print ( allcomb' == allcomb)
  print $ makeAllBlob test 
