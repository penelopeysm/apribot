module Utils (markdownEscape, randomText, makeTable) where

import Data.List (transpose)
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

makeTable :: [[Text]] -> Bool -> Maybe Int -> Text
makeTable rows hasHeader extraLinesEvery =
  let
    fieldWidths = map (maximum . map T.length) (transpose rows)
    makeRow row = T.intercalate "|" $ zipWith (`T.justifyLeft` ' ') fieldWidths row
    rows' = map makeRow rows
    dashes = T.intercalate "+" $ map (`T.replicate` "-") fieldWidths
    separatedRows = case (hasHeader, extraLinesEvery) of
      (True, Just n) -> head rows' : dashes : intercalateEvery n dashes (tail rows')
      (True, Nothing) -> head rows' : dashes : tail rows'
      (False, Just n) -> intercalateEvery n dashes rows'
      (False, Nothing) -> rows'
  in
    T.unlines separatedRows

intercalateEvery :: Int -> a -> [a] -> [a]
intercalateEvery n s xs =
  case splitAt n xs of
    (ys, []) -> ys
    (ys, zs) -> ys ++ s : intercalateEvery n s zs
