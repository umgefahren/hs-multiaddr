{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.UnixPath
  (
    UnixPath (..)
  ) where

import qualified Text.ParserCombinators.ReadP as Parser

import GHC.Generics (Generic)
import System.FilePath (FilePath)
import Data.Maybe (listToMaybe)

newtype UnixPath = UnixPath { path :: FilePath }
                 deriving (Show, Eq, Generic)

instance Read UnixPath where
  readsPrec _ = fmap (maybe [] (:[]) . listToMaybe) $ Parser.readP_to_S $ do
    path <- Parser.many1 Parser.get
    Parser.manyTill (Parser.char '/') Parser.eof
    return $ UnixPath $ "/" ++ path
