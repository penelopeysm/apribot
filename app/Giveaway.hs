{-# LANGUAGE TypeApplications #-}

module Giveaway where

import Control.Exception (try)
import Data.Text (Text)
import Pokeapi

data Mew = Mew {tera :: Type, nature :: Nature} deriving (Eq, Show)

getMew :: Text -> Text -> IO (Maybe Mew)
getMew t n = do
  t' <- try @PokeException $ get @Type t
  n' <- try @PokeException $ get @Nature n
  case (t', n') of
    (Right t'', Right n'') -> pure $ Just $ Mew t'' n''
    _ -> pure Nothing
