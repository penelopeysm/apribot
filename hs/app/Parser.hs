module Parser
  ( DiscordCommand (..),
    parseDiscordCommand,
    parseDiscordCommand',
  )
where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Setup.Game (Game (..)) -- from apripsql
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

data DiscordCommand
  = Help
  | Thread
  | CloseThread
  | PotluckVotes
  | PotluckSignup
  | Sandwich
  | HA [Text]
  | EM (Maybe Game) [Text]
  | EMParents (Maybe Game) [Text]
  | Nature [Text]
  | Legality [Text]
  | Sprite [Text]
  | Info [Text]
  deriving (Eq, Show)

help :: Parser DiscordCommand
help = Help <$ C.string' "!help"

thread :: Parser DiscordCommand
thread = Thread <$ C.string' "!thread"

closeThread :: Parser DiscordCommand
closeThread = CloseThread <$ choice [C.string' "!close", C.string' "[close]"]

potluck1 :: Parser DiscordCommand
potluck1 = PotluckVotes <$ C.string' "!potluck1"

potluck2 :: Parser DiscordCommand
potluck2 = PotluckSignup <$ C.string' "!potluck2"

sandwich :: Parser DiscordCommand
sandwich = Sandwich <$ C.string' "!sandwich"

-- This parses a list of words separated by whitespace
parseWords :: Parser [Text]
parseWords = map T.stripEnd <$> many (C.space1 *> takeWhile1P Nothing (not . isSpace))

ha :: Parser DiscordCommand
ha = HA <$> (C.string' "!ha" *> parseWords)

nature :: Parser DiscordCommand
nature = Nature <$> (C.string' "!nature" *> parseWords)

legality :: Parser DiscordCommand
legality = Legality <$> (C.string' "!legality" *> parseWords)

parseGame :: Parser Game
parseGame =
  choice
    [ USUM <$ C.string' "usum",
      SwSh <$ C.string' "swsh",
      BDSP <$ C.string' "bdsp",
      SV <$ C.string' "sv"
    ]

parseGameAndPkmn :: Parser (Maybe Game, [Text])
parseGameAndPkmn = do
  game <- optional (C.space1 *> parseGame) -- don't backtrack on this one
  pkmnNames <- parseWords
  pure (game, pkmnNames)

em :: Parser DiscordCommand
em = do
  void $ C.string' "!em"
  (game, pkmnNames) <- parseGameAndPkmn
  pure $ EM game pkmnNames

emParents :: Parser DiscordCommand
emParents = do
  void $ C.string' "!emp"
  (game, pkmnNames) <- parseGameAndPkmn
  pure $ EMParents game pkmnNames

sprite :: Parser DiscordCommand
sprite = Sprite <$> (C.string' "!sprite" *> parseWords)

info :: Parser DiscordCommand
info = Info <$> (C.string' "!info" *> parseWords)

parser :: Parser DiscordCommand
parser = do
  cmd <-
    lexeme $
      choice
        [ help,
          thread,
          closeThread,
          potluck1,
          potluck2,
          ha,
          nature,
          legality,
          emParents, -- Must come before `em`
          em,
          sprite,
          info,
          sandwich
        ]
  -- Child parsers don't need eof because there's one here
  eof
  pure cmd

parseDiscordCommand :: Text -> Maybe DiscordCommand
parseDiscordCommand = parseMaybe parser

parseDiscordCommand' :: Text -> Either String DiscordCommand
parseDiscordCommand' t = case parse parser "" t of
  Left err -> Left $ errorBundlePretty err
  Right cmd -> Right cmd
