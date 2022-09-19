{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server (runApp, app) where

import Network.Wai (Application)
import Network.Wai.Parse (fileContent)
import qualified Web.Scotty as S

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer (execWriterT, tell)
import Control.Exception (catch, displayException, SomeException)

import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text.Lazy (pack, Text)
import Data.Aeson hiding (Result)

import GHC.Generics (Generic)

import Text.PrettyPrint.Leijen.Text (Doc,(<$$>))
import qualified Text.PrettyPrint.Leijen.Text as PP (text)

import Language.Prolog (Program, Clause(..), Term(..), VariableName(..), Goal, consultString)
import Prolog.Programming.Task (verifyConfig, checkTask)
import Prolog.Programming.Data (Config(..),Code(..))

import IndexContent (indexHTML)
import Web.Scotty (middleware)
import Network.Wai.Middleware.Cors (simpleCors)

import Util (Mode(..))
import DropClauseMutation
import ToAnonymousVarMutation

app' :: S.ScottyM ()
app' = do
  middleware simpleCors
  S.get "/" $ do
    S.html indexHTML

  S.post "/test-files" $ do
    [("config",configBS),("solution",codeBS)] <- S.files
    let
      config = Config . toString $ fileContent configBS
      code = Code . toString $ fileContent codeBS
    result <- liftIO $ catch (runMain config code)
      (\(e:: SomeException) -> pure . Reject . PP.text . pack $ displayException e)
    case result of
      Inform info -> S.text . pack $ show (PP.text "Success\n" <$$> info)
      Reject err -> S.text . pack $ show (PP.text "Failure\n" <$$> err)

  S.post "/parse-file" $ do
    [("pl-source",file)] <- S.files
    case consultString . toString $ fileContent file of
      Right program -> S.json program
      Left err -> S.text "parse error"

  S.post "/get-pl-file" $ do
    cs <- S.jsonData @[Clause]
    S.text . pack . unlines $ show <$> cs

  S.post "/drop-clause-mutation/summ" $ do
    fileMutation Summary

  S.post "/drop-clause-mutation/indiv/:num" $ do
     num <- S.param "num"
     fileMutation (Indiviual num)

  S.post "/to-anon-var-mutation/summ" $ do
    anonVarMutation Summary

  S.post "/to-anon-var-mutation/indiv/:num" $ do
     num <- S.param "num"
     anonVarMutation (Indiviual num)

fileMutation m = do
  [("program",programFile)] <- S.files
  case dropClauseMutation m (toString $ fileContent programFile) of
    Left e -> S.text . pack $ "Error: " ++ show e
    Right ps -> S.json $ MutationResult ps

anonVarMutation m = do
  [("program",programFile)] <- S.files
  case toAnonVarMutation m (toString $ fileContent programFile) of
    Left e -> S.text . pack $ "Error: " ++ show e
    Right ps -> S.json $ MutationResult ps

newtype MutationResult = MutationResult [String]
instance ToJSON MutationResult where
  toJSON (MutationResult ps) = object ["result" .= ps]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'

data Result = Reject Doc | Inform Doc

instance Semigroup Result where
  Reject x <> _ =  Reject x
  Inform x <> Reject y =  Reject (x <$$> y)
  Inform x <> Inform y =  Inform (x <$$> y)

instance Monoid Result where
  mempty = Inform mempty

runMain :: Config -> Code -> IO Result
runMain config code = do
  verifyConfig config
  execWriterT $
    checkTask
      (\doc -> tell (Reject doc) >> pure undefined)
      (tell . Inform)
      (\_ -> tell . Inform $ PP.text "*** image output is currently not supported" )
      config code

-- JSON conversion
deriving instance Generic Term
deriving instance Generic VariableName

instance ToJSON Term
instance ToJSON VariableName
instance ToJSON Clause where
  toJSON (Clause lhs rhs) = object ["lhs" .= lhs, "rhs" .= rhs]
  toJSON (ClauseFn _ _) = error "impossible?"

instance FromJSON Term
instance FromJSON VariableName
instance FromJSON Clause where
  parseJSON = withObject "Clause" $ \v -> Clause <$> v .: "lhs" <*> v .: "rhs"
