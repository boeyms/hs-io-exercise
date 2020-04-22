{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-} -- For plucking version from Cabal file

module Main where

import           Control.Monad
import Core.Data
import           Core.Program
import           Core.System
import           Core.Text
import qualified Data.ByteString.Lazy          as B
import           Data.Csv
import qualified Data.Vector                   as V
import GHC.Generics (Generic)

data Metric = Metric { ts :: !Int, fieldname :: !B.ByteString, value :: !Int }
    deriving (Generic, Show)

instance FromRecord Metric
instance ToRecord Metric

version :: Version
version = $(fromPackage)

argConfig :: Config
argConfig = simple [Argument "filename" "File to read."]

addMetricToMap :: Metric -> Map B.ByteString [Metric] -> Map B.ByteString [Metric]
addMetricToMap metric map =
    case lookupKeyValue k map of
        Nothing -> insertKeyValue k [metric] map
        Just vs -> insertKeyValue k (metric : vs) map
    where
        k = fieldname metric

buildMetricMap :: V.Vector Metric -> Map B.ByteString [Metric]
buildMetricMap = V.foldr addMetricToMap emptyMap

-- TODO: Use proper types to distinguish Ints as seconds, milliseconds, etc.

millisToSecs :: Int -> Int
millisToSecs = flip div 1000

program :: Program None ()
program = do
    params <- getCommandLine
    withContext $ \runProgram -> do
        csvData <- mapM B.readFile $ lookupArgument "filename" params
        metrics <- forM csvData $ \d ->
            case decode NoHeader d :: Either String (V.Vector Metric) of
                Left  err -> runProgram $ event $ intoRope err
                Right v   -> return ()
        -- Structure:
        -- * read file
        -- * parse each line to a record
        -- * group records by field name and timestamp
        -- * calculate desired value over each
        -- * determine output
        -- * do output loop
        return ()
    return ()

main :: IO ()
main = do
    context <- configure version None argConfig
    executeWith context $ write "Hello World"
