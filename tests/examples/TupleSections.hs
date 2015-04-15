{-# LANGUAGE TupleSections #-}

foo = do
  liftIO $ atomicModifyIORef ciTokens ((,()) . f)

