{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Utils.File where
import System.Posix.Files(fileExist)
import System.Posix.Directory(readDirStream,openDirStream)
import System.FilePath.Posix
import System.Cmd
import System.Directory(createDirectoryIfMissing
                       ,setCurrentDirectory
                       ,getCurrentDirectory)
import Control.Exception as E(bracket_,catch,evaluate) 
import qualified System.FilePath.Posix as Posix
import System.Directory
import System.IO

import Control.Parallel.Strategies
import Control.Exception
import IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip

import Data.Binary

-- | Cache results of operation op under name 'fn'. 
--   if correct file is found, its contents are used, otherwise 'op' is performed and it's results
--   are saved to the filed and returned from the call. 

class Cacheable a where
    writeCache :: FilePath -> a -> IO ()
    readCache  :: FilePath -> IO a

instance (Binary a) => Cacheable a
    where
     readCache  fn   = do
                        (BS.readFile fn >>= E.evaluate . decode . decompress) 
                            `E.catch` (\err -> error $ "Error reading cache "++fn++": "
                                                        ++show (err:: IOException))
     writeCache fn x = (BS.writeFile fn . compress . encode $ x)
                            `E.catch` (\err -> error $ "Error writing cache "++fn++": "
                                                        ++show (err:: IOException))

cached :: (Cacheable a) => FilePath -> IO a -> IO a
cached fn op = do
        let fn' = fn++".CACHE"
        e <- fileExist fn'
        if e
            then readCache fn'
            else do
                    x <- op
                    writeCache fn' x
                    return x


-- Check that file is not . or ..
notABackLink = (not . (flip elem) [".",".."] . takeFileName)

-- Get directory contents with path appended to them
getDirectoryContentsWithPath path = do
  dc <- getDirectoryContents path
  return $ map (path Posix.</>) dc

-- Return a filename that does not exists
genFileName base ext = genFileName' names
            where 
                genFileName' :: [FilePath] -> IO FilePath
                genFileName' (n:ns) = do 
                                          exists <- fileExist n
                                          if exists
                                           then genFileName' ns
                                           else return n 
                names :: [FilePath]
                names = [base ++ show x++ext | x <- [1..]]

-- return files from directory
getDirectoryList fp = openDirStream fp >>= getDSContent 
                where 
                 getDSContent ds = do 
                                 x <- readDirStream ds
                                 if x == "" then return []
                                  else do 
                                        xs <- getDSContent ds
                                        return ((fp++x):xs)

-- Does file have extension `ext`  
hasExt ext fp = all (uncurry (==)) $ zip (reverse ext) (reverse fp)

-- Retreive files from `path` that have extension `ext`
getFilesOfExt ext path = getDirectoryList path >>= return.filter (hasExt ext)

-- Perform operation  `op` in directory `dir`. If directory does not exist
-- It will be created
inDirectory :: FilePath -> IO a -> IO a 
inDirectory dir op = do
                createDirectoryIfMissing True dir
                currDir <- getCurrentDirectory
                Control.Exception.bracket_ 
                    (setCurrentDirectory dir)
                    (setCurrentDirectory currDir)
                    op

-- Append text to `file` strictly and hax it so that parallel writes are forcefully done 
strictPersistentAppendFile p file string = B.appendFile file (B.pack string)
    where 
     catch :: Int -> IOException -> IO ()
     catch 0 e = Control.Exception.throw e
     catch n e = strictPersistentAppendFile (n-1) file string

strictAppendFile file string = B.appendFile file (B.pack string)
--strictAppendFile file text = strictAppendFile' file text `demanding` rnf text
--strictAppendFile' outputfile text = Control.Exception.catch 
--        (do 
--            appendhandle <- openFile outputfile (AppendMode)
--            hPutStr appendhandle text
--            hFlush appendhandle
--            hClose appendhandle)
--        (\e ->fail  "")


