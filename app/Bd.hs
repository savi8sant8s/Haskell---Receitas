{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Bd where

import           Control.Monad (mapM_)
import           Data.Int (Int64)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import GHC.Generics

data Receita =
  Receita
    { id_rec :: Int64
    , nome_rec :: Text
    , ingredientes :: Text
    , preparo :: Text
    , categoria :: Text
    } deriving (Eq, Read, Show, Generic)

instance FromRow Receita where
  fromRow = Receita <$> field <*> field <*> field <*> field <*> field
instance ToRow Receita where
  toRow (Receita _rId rNome rIngredientes rPreparo rCategoria) = toRow (rNome, rIngredientes, rPreparo, rCategoria)

iniciarBD :: IO ()
iniciarBD = do
  conn <- open "receitas.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS receitas (id_rec INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, nome_rec TEXT NOT NULL UNIQUE, ingredientes BLOB NOT NULL, preparo TEXT NOT NULL, categorias TEXT NOT NULL)"
  close conn

criarReceita :: Text -> Text -> Text -> Text -> IO ()
criarReceita nome_rec ingredientes preparo categorias = do
  conn <- open "receitas.db"
  execute conn "INSERT OR IGNORE INTO receitas (nome_rec, ingredientes, preparo, categorias) VALUES (?,?,?,?)" (Receita 0 nome_rec ingredientes preparo categorias)
  close conn

listarReceitas :: IO [Receita]
listarReceitas = do
  conn <- open "receitas.db"
  receitas <- query_ conn "SELECT * from receitas" :: IO [Receita]
  exit <- close conn
  return receitas


deletarReceita :: Int64 -> IO ()
deletarReceita id_rec = do
  conn <- open "receitas.db"
  execute conn "DELETE FROM receitas WHERE id_rec = ?" (Only (id_rec :: Int64))
  close conn

--main = do
 -- a <- liftIO iniciarBD
 -- b <- liftIO (criarReceita (pack "vitamina") (pack "banana leite") (pack "Bebida"))
 --listarReceitas