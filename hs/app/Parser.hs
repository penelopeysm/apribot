module Parser
  ( DiscordCommand (..),
    parseDiscordCommand,
    parseDiscordCommand',
  )
where

import Control.Monad (void)
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
  | HA (Maybe Text)
  | EM (Maybe Game) (Maybe Text)
  | EMParents (Maybe Game) (Maybe Text)
  | Nature (Maybe Text)
  | Legality (Maybe Text)
  | Sprite (Maybe Text)
  | Info (Maybe Text)
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

-- This consumes the rest of the input, and also removes trailing whitespace
parsePkmnName :: Parser Text
parsePkmnName = T.stripEnd <$> takeWhile1P Nothing (const True)

-- Helper function
spTry :: Parser a -> Parser (Maybe a)
spTry p = optional $ try (C.space1 *> p)

ha :: Parser DiscordCommand
ha = HA <$> (C.string' "!ha" *> spTry parsePkmnName)

nature :: Parser DiscordCommand
nature = Nature <$> (C.string' "!nature" *> spTry parsePkmnName)

legality :: Parser DiscordCommand
legality = Legality <$> (C.string' "!legality" *> spTry parsePkmnName)

parseGame :: Parser Game
parseGame =
  choice
    [ USUM <$ C.string' "usum",
      SwSh <$ C.string' "swsh",
      BDSP <$ C.string' "bdsp",
      SV <$ C.string' "sv"
    ]

parseGameAndPkmn :: Parser (Maybe Game, Maybe Text)
parseGameAndPkmn = do
  game <- optional (C.space1 *> parseGame) -- don't backtrack on this one
  pkmnName <- spTry parsePkmnName
  pure (game, pkmnName)

em :: Parser DiscordCommand
em = do
  void $ C.string' "!em"
  (game, pkmnName) <- parseGameAndPkmn
  pure $ EM game pkmnName

emParents :: Parser DiscordCommand
emParents = do
  void $ C.string' "!emp"
  (game, pkmnName) <- parseGameAndPkmn
  pure $ EMParents game pkmnName

sprite :: Parser DiscordCommand
sprite = Sprite <$> (C.string' "!sprite" *> spTry parsePkmnName)

info :: Parser DiscordCommand
info = Info <$> (C.string' "!info" *> spTry parsePkmnName)

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
