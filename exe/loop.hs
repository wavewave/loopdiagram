import HEP.Physics.LoopCalculation.BoxDiagram 

main' = do 
  putStrLn "superpotential test"
  mapM_ print $ map emcharge superpotXQLD
  let vertexFFSwoGen = concatMap superpot3toVertexFFS  superpotXQLD 
      vertexFFSwGen = concatMap assignGenToVertexFFS vertexFFSwoGen
  mapM_ (\x -> mapM_ print x >> putStrLn "----" ) $ map assignGenToVertexFFS vertexFFSwoGen

  
  let blob = io_bar_sR_Gamma_dR_squared
      hset = prepareHandleSet superpotXQLD (blobExternals blob)
  mapM_ print $ hsetVtx1Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx2Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx3Int hset 
  putStrLn "---"
  mapM_ print $ hsetVtx4Int hset 
  putStrLn "---"
  {-
  mapM_ print $ {- sort $ map snd -} (selectVertexForExt e1 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ sort $ map snd (selectVertexForExt e2 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ sort $ map snd (selectVertexForExt e3 vertexFFSwGen)
  putStrLn "---"
  mapM_ print $ {- sort $ map snd -} (selectVertexForExt e4 vertexFFSwGen)
  -}

analysis blob = do 
  putStrLn "loop" 
  print $ deltaS blob
  let allblobs = makeAllBlob blob
      matchedblobs =  filter (matchDirection [(I,I,I),(O,O,O)]) allblobs
  mapM_ print matchedblobs 
  let hset = prepareHandleSet superpotXQLD (blobExternals blob)
  putStrLn "--------------"
  mapM_ (\xs -> mapM_ print xs >> putStrLn "========") (map (match hset) matchedblobs)

main = analysis io_bar_sL_Gamma_dL_squared

