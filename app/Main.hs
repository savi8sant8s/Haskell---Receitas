{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

import Web.Spock
import Web.Spock.Config
import Data.Aeson       hiding (json)
import Data.Monoid      ((<>))
import Data.Text        (Text, length, pack)
import Prelude hiding   (length)
import Data.Int         (Int64)
import Data.List hiding (delete)
import GHC.Generics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Bd (iniciarBD, criarReceita, listarReceitas, deletarReceita, Receita)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

data ReceitaB = ReceitaB
  { nome :: Text
  , ingredientes :: Text
  , categoria :: Text
  , preparo :: Text
  } deriving (Generic, Show)

instance ToJSON ReceitaB
instance FromJSON ReceitaB

data Receitas = Receitas {
  receitas :: [Bd.Receita]  
} deriving (Generic, Show)

instance ToJSON Receitas
instance FromJSON Receitas

instance ToJSON Bd.Receita 
instance FromJSON Bd.Receita

data ReceitaDel = ReceitaDel
  { id_rec :: Int64
  } deriving (Generic, Show)

instance ToJSON ReceitaDel
instance FromJSON ReceitaDel

corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

main :: IO ()
main = do
  start <- liftIO iniciarBD
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = prehook corsHeader $ 
  do
    post "v1/receitas" $ do
      theReceita <- jsonBody' :: ApiAction ReceitaB
      addReceita <- liftIO (criarReceita (nome theReceita) (ingredientes theReceita) (preparo theReceita) (categoria theReceita))
      json (object ["code_status".= message])
    
    post "v1/del-receitas" $ do
      theReceita <- jsonBody' :: ApiAction ReceitaDel
      delReceita <- liftIO (deletarReceita (id_rec theReceita))
      json (object ["code_status".= message2])

    get "v1/receitas" $ do
      receitas <- liftIO listarReceitas
      json Receitas {receitas = receitas}
      
    get "v1/categorias" $ do
      json (object ["categorias".= categorias])    

  where categorias = [pack "Prato", pack "Sobremesa", pack "Bebida"]
        message = pack "Receita cadastrada com sucesso!"
        message2 = pack "Receita deletada com sucesso!"