module Parser
  ( DiscordCommand (..),
    parseDiscordCommand,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Pokemon (Game (..))
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
  | HA (Maybe Text)
  | EM (Maybe Game) (Maybe Text)
  | EMParents (Maybe Game) (Maybe Text)
  | Nature (Maybe Text)
  | Legality (Maybe Text)
  deriving (Eq, Show)

help :: Parser DiscordCommand
help = Help <$ lexeme (C.string' "!help")

thread :: Parser DiscordCommand
thread = Thread <$ lexeme (C.string' "!thread")

closeThread :: Parser DiscordCommand
closeThread = CloseThread <$ lexeme (choice [C.string' "!close", C.string' "[close]"])

potluck1 :: Parser DiscordCommand
potluck1 = PotluckVotes <$ lexeme (C.string' "!potluck1")

potluck2 :: Parser DiscordCommand
potluck2 = PotluckSignup <$ lexeme (C.string' "!potluck2")

ha :: Parser DiscordCommand
ha = HA <$> (lexeme (C.string' "!ha") *> lexeme takeRest)

nature :: Parser DiscordCommand
nature = Nature <$> (lexeme (C.string' "!nature") *> lexeme takeRest)

legality :: Parser DiscordCommand
legality = Legality <$> (lexeme (C.string' "!legality") *> lexeme takeRest)

parseGame :: Parser Game
parseGame =
  lexeme $
    choice
      [ USUM <$ C.string' "usum",
        SwSh <$ C.string' "swsh",
        BDSP <$ C.string' "bdsp",
        SV <$ C.string' "sv"
      ]

parsePkmnName :: Parser Text
parsePkmnName = lexeme (takeWhile1P Nothing (const True))

parseGameAndPkmn :: Parser (Maybe Game, Maybe Text)
parseGameAndPkmn = do
  game <- optional parseGame
  pkmnName <- optional parsePkmnName
  pure (game, pkmnName)

em :: Parser DiscordCommand
em = do
  void $ lexeme (C.string' "!em")
  (game, pkmnName) <- parseGameAndPkmn
  pure $ EM game pkmnName

emParents :: Parser DiscordCommand
emParents = do
  void $ lexeme (C.string' "!emp")
  (game, pkmnName) <- parseGameAndPkmn
  pure $ EMParents game pkmnName

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
          em
        ]
  -- Child parsers don't need eof because there's one here
  eof
  pure cmd

parseDiscordCommand :: Text -> Maybe DiscordCommand
parseDiscordCommand = parseMaybe parser
