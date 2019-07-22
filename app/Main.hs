module Main where

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Base16 as Hex

import Data.Char (toLower)
import Data.Hashable
import Data.Map.Strict as Map
import Data.Maybe
import Data.ByteArray ()

import System.IO
import System.FilePath
import System.FilePath.Find
import System.Directory
import Options.Applicative

import Crypto.Hash (Digest(..), SHA1(..), digestFromByteString, HashAlgorithm)
import Crypto.Hash.Conduit

import Debug.Trace

data Command
  = Dups    UnaryOpts
  | Summary UnaryOpts
  | Diff    BinaryOpts

data UnaryOpts  = Unary  { dir :: String  , filt :: Maybe Filter }
data BinaryOpts = Binary { left :: String , right :: String , filt :: Maybe Filter }

data Filter = Filter { extensions :: String }

fileFilters :: Parser Filter
fileFilters = Filter <$> argument str (metavar "filter" <> help "Comma separated file exts to include")

unaryOpts :: Parser UnaryOpts
unaryOpts = 
   Unary
    <$> argument str (metavar "PATH" <> help "the archive to recursively check for dupliate files")
    <*> optional fileFilters

binaryOpts :: Parser Command
binaryOpts = Diff <$>
  ( Binary
    <$> argument str (metavar "MASTER" <> help "the master archive")
    <*> argument str (metavar "CLIENT" <> help "the client archive")
    <*> optional fileFilters
  )

commands :: Parser Command
commands = subparser
   ( command "dups"
     (info (Dups <$> unaryOpts <**> helper)
           (fullDesc <> progDesc "Find duplicates in an archive"))
   <> command "diff"
     (info (binaryOpts <**> helper)
           (fullDesc <> progDesc "Asymmetrical difference between archives")
     )
   <> command "summary"
     (info (Summary <$> unaryOpts <**> helper)
           (fullDesc <> progDesc "Summarize archive content")
     )
   )

data Object = Object {
  path   :: FilePath,
  digest :: Digest SHA1
}

instance Show Object where
  show (Object p _) = p

getObject :: FilePath → IO Object
getObject fp = do
  dig ← hashFile fp
  return $ Object fp dig
  
dupsOf :: [Object] → [[Object]]
dupsOf objs =
  let
    bysha = Map.fromListWith (++) ((\o → (digest o , [o])) <$> objs)
    dups  = Map.filter ((>1) . length) bysha
  in Map.elems dups

regularFiles = find always (fileType ==? RegularFile)

duplicates :: UnaryOpts → IO ()
duplicates (Unary fp' ft) = do
  fp ← makeAbsolute fp'
  paths ← regularFiles fp
  objs  ← forM paths getObject
  let dups = dupsOf objs
  forM_ dups $ \dup → do
    putStrLn (show dup)

type Summary = Map (Digest SHA1) Object

writeSummary :: Summary → IO ()
writeSummary = print . fmap (\(k , v) → (show k , show v)) . toList

-- readSummary :: String → Maybe Summary
-- readSummary = do
--   return $ readMaybe txt

mkSummary :: UnaryOpts → IO Summary
mkSummary (Unary pth flt) = do
  pth  ← makeAbsolute pth
  hPutStrLn stderr $ "Summarizing " ++ pth
  fs   ← regularFiles pth
  objs ← forM fs $ \fp → do
    hPutStrLn stderr $ "Summarizing " ++ fp
    hFlush stdout
    getObject fp
  return $ Map.fromList $ (fmap (\o → (digest o , o)) objs) 

summarize :: UnaryOpts → IO Summary
summarize opts@(Unary pth flt) =
  if isExtensionOf ".arx" pth
  then readSummary pth else do mkSummary opts

readSummary :: FilePath → IO Summary
readSummary pth = do
  tups :: [(String, String)] ← read <$> readFile pth
  return $ fromList $ (\o → (digest o, o))<$> readObject <$> tups

  where
    readObject :: (String , String) → Object
    readObject (digest, path) =
      let
        dig = fromJust (digestFromByteString (fst $ Hex.decode $ BSC.pack digest))
      in Object path dig

diff :: BinaryOpts → IO ()
diff (Binary mst cli ft) = do
  mst ← summarize (Unary mst ft)
  cli ← summarize (Unary cli ft)

  hPutStrLn stderr $ (show $ length mst) ++ " unique files in master"
  hPutStrLn stderr $ (show $ length cli) ++ " unique files in client"

  forM_ (Map.difference cli mst) $ putStrLn . path

-- fullDiff :: BinaryOpts → IO ()
-- fullDiff 

summary :: UnaryOpts → IO ()
summary ops = do
  s ← summarize ops
  writeSummary s

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  case cmd of
    Dups ops → duplicates ops
    Diff ops → diff ops
    Summary ops → summary ops
