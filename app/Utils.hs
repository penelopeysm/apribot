module Utils (markdownEscape, randomText) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Random

-- | In principle, more characters should be escaped. However, the only texts
-- we're escaping (fornow) are Reddit usernames, and I think the underscore is
-- the only character that is both allowed in a username and has a special
-- meaning in Markdown.
markdownEscape :: Text -> Text
markdownEscape = T.replace "_" "\\_"

chars :: String
chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

randomText :: Int -> IO Text
randomText len = do
  T.pack . map (chars !!) . take len . randomRs (0, length chars - 1) <$> initStdGen
