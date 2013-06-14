import HEP.Physics.LoopCalculation.BoxDiagram 

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

superpotXUDD :: [SuperPot3] 
superpotXUDD = [ SuperPot3 NP_X  (SM_Uc ()) NP_U 
               , SuperPot3 NP_Uc (SM_Dc ()) (SM_Dc ()) 
               , SuperPot3 NP_X  (SM_Dc ()) NP_D
               , SuperPot3 NP_Dc (SM_Uc ()) (SM_Dc ())  
               ] 



test_superpotXQLD :: [SuperPot3]
test_superpotXQLD = [ SuperPot3 NP_X  (SM_Dc ()) NP_D 
                    , SuperPot3 NP_Dc (SM_U  ()) (SM_E ()) 
                    , SuperPot3 NP_Dc (SM_D  ()) (SM_v ()) 
                    ] 

io_bar_sR_Gamma_dR_squared :: Blob () 
io_bar_sR_Gamma_dR_squared = 
  Blob (Externals (External quarkSc I)  (External quarkDc O)  (External quarkDc O) (External quarkSc I)) ()

io_bar_sL_Gamma_dL_squared :: Blob () 
io_bar_sL_Gamma_dL_squared = 
  Blob (Externals (External quarkS I)  (External quarkD O)  (External quarkD O) (External quarkS I)) ()

io_bar_sL_dR_bar_sR_dL :: Blob () 
io_bar_sL_dR_bar_sR_dL = 
  Blob (Externals (External quarkS O)  (External quarkD I) (External quarkDc O)  (External quarkSc I)) ()

io_bar_cR_Gamma_uR_squared :: Blob () 
io_bar_cR_Gamma_uR_squared = 
  Blob (Externals (External quarkCc I)  (External quarkUc O) (External quarkUc O)  (External quarkCc I)) ()

io_bar_cL_Gamma_uL_squared :: Blob () 
io_bar_cL_Gamma_uL_squared = 
  Blob (Externals (External quarkC O)  (External quarkU I) (External quarkU I)  (External quarkC O)) ()



vertextest superpot blob = do 
  putStrLn "superpotential test"
  mapM_ print $ map emcharge superpot 
  let vertexFFSwoGen = concatMap superpot3toVertexFFS  superpot 
      vertexFFSwGen = concatMap assignGenToVertexFFS vertexFFSwoGen
  mapM_ (\x -> mapM_ print x >> putStrLn "----" ) $ map assignGenToVertexFFS vertexFFSwoGen
  
  let hset = prepareHandleSet superpot (blobExternals blob)
  mapM_ print $ hsetVtx1Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx2Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx3Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx4Int hset 
  putStrLn "---"

analysis superpot blob = do 
  putStrLn "loop" 
  print $ deltaS blob
  let allblobs = makeAllBlob blob
      matchedblobs =  filter (matchDirection [(I,I,I),(O,O,O)]) allblobs
  mapM_ print matchedblobs 
  let hset = prepareHandleSet {- test_superpotXQLD -} superpot (blobExternals blob)
  putStrLn "--------------"
  mapM_ (\xs -> mapM_ print xs >> putStrLn "========") (map (match hset) matchedblobs)
  -- mapM_ print $ match hset (matchedblobs !! 1) 

main = analysis superpotXUDD io_bar_sR_Gamma_dR_squared
  -- vertextest superpotXUDD io_bar_sR_Gamma_dR_squared
  -- io_bar_cL_Gamma_uL_squared
  -- io_bar_cR_Gamma_uR_squared
   

  -- io_bar_sL_dR_bar_sR_dL
  -- io_bar_sL_Gamma_dL_squared

