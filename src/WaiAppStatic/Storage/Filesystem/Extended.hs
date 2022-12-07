{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{- |
Module: WaiAppStatic.Storage.Filesystem.Extended

Most of this is taken from WaiAppStatic.Storage.Filesystem.
The goal is to allow to re-define the above `ssLookupFile` in
`defaultWebAppSettings`: instead of looking up files as-is, we make it so
that given a URL such as /some/path, we act as if it was /some/path.html,
thus making nicer URLs (where the .html is not necessary).
TODO Move this to hypered/commence.
TODO Don't rewrite paths when there is an extension, otherwise it won't work
for e.g. images.
Note: we copy more code than necessary because it is not in the export list
of the original module.

-}
module WaiAppStatic.Storage.Filesystem.Extended
  ( hashFileIfExists
  , webAppLookup
  ) where

import qualified Data.ByteString.Lazy          as BL
                                                ( hGetContents )
import           Data.List                      ( init
                                                , last
                                                )
import qualified Data.Text                     as T
import qualified Network.Wai                   as Wai
import           Prelude                 hiding ( hash )
import           System.FilePath                ( (</>)
                                                , takeExtension
                                                )
import           System.PosixCompat.Files       ( fileSize
                                                , getFileStatus
                                                , isRegularFile
                                                , modificationTime
                                                )
import           WaiAppStatic.Storage.Filesystem
                                                ( ETagLookup )
import           WaiAppStatic.Types             ( File(..)
                                                , LookupResult(..)
                                                , Piece(..)
                                                , Pieces
                                                , toPiece
                                                )

import           System.IO                      ( withBinaryFile )

import           Crypto.Hash                    ( Digest
                                                , MD5
                                                , hashlazy
                                                )
import           Data.ByteArray.Encoding


--------------------------------------------------------------------------------
-- | Construct a new path from a root and some @Pieces@.
pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (\fp p -> fp </> T.unpack (fromPiece p))

-- | Construct a @Piece@ without input validation.
-- The original can use a non-exported `Piece` constructor. Here we use the one
-- that does some validation, but in an unsafe way.
unsafeToPiece :: Text -> Piece
unsafeToPiece t = let Just p = toPiece t in p

-- | More efficient than @fileSystemLookup@ as it only concerns itself with
-- finding files, not folders.
-- This is the main change compared to WaiAppStatic.Storage.Filesystem:
-- If the last piece is empty, this lookup index.html.
-- Otherwise, this appends ".html" before trying to lookup that.
-- This means that if you want `documentation/` to be a directory, you can
-- decide if you want to support `documentation/`, by putting an `index.html`
-- within, or support `documentation`, by making a `documentation.html` file
-- instead.
webAppLookup :: ETagLookup -> FilePath -> Pieces -> IO LookupResult
webAppLookup hashFunc prefix pieces = fileHelperLR hashFunc fp lastPiece
 where
  fp      = pathFromPieces prefix pieces'
  pieces' = initPieces ++ [lastPiece]
  (initPieces, lastPiece)
    | null pieces
    = ([], unsafeToPiece "index.html")
    | Just (last pieces) == toPiece ""
    = (init pieces, unsafeToPiece "index.html")
    | otherwise
    = let lastP = case fromPiece (last pieces) of
            s | takeExtension (T.unpack s) `elem` extensions -> last pieces
            s -> unsafeToPiece $ s <> ".html"
      in  (init pieces, lastP)
  extensions =
    [ ".css"
    , ".ico"
    , ".jpg"
    , ".js"
    , ".otf"
    , ".png"
    , ".ttf"
    , ".svg"
    , ".st"
    , ".txt"
    , ".woff"
    , ".woff2"
    ]

-- | Convenience wrapper for @fileHelper@.
fileHelperLR
  :: ETagLookup
  -> FilePath -- ^ file location
  -> Piece -- ^ file name
  -> IO LookupResult
fileHelperLR a b c = fmap (maybe LRNotFound LRFile) $ fileHelper a b c

-- | Attempt to load up a @File@ from the given path.
-- This is taken and modified from WaiAppStatic.Storage.Filesystem.
fileHelper
  :: ETagLookup
  -> FilePath -- ^ file location
  -> Piece -- ^ file name
  -> IO (Maybe File)
fileHelper hashFunc fp name = do
  efs <- try $ getFileStatus fp
  case efs of
    Left (_ :: SomeException)   -> return Nothing
    Right fs | isRegularFile fs -> return $ Just File
      { fileGetSize     = fromIntegral $ fileSize fs
      , fileToResponse  = \s h -> Wai.responseFile s h fp Nothing
      , fileName        = name
      , fileGetHash     = hashFunc fp
      , fileGetModified = Just $ modificationTime fs
      }
    Right _ -> return Nothing

hashFileIfExists :: ETagLookup
hashFileIfExists fp = do
  res <- try $ hashFile fp
  return $ case res of
    Left  (_ :: SomeException) -> Nothing
    Right x                    -> Just x

-- | MD5 hash and base64-encode the file contents. Does not check if the file
-- exists.
hashFile :: FilePath -> IO ByteString
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
  f <- BL.hGetContents h
  let !hash = hashlazy f :: Digest MD5
  return $ convertToBase Base64 hash
