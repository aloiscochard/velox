module Sarsi where

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (MD5)
import Network.Socket (Family(AF_UNIX), SockAddr(SockAddrUnix), Socket, SocketType(Stream), defaultProtocol, socket)
import System.Directory (getTemporaryDirectory, makeAbsolute)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BSC8

title :: String
title = "sarsi"

-- TODO Windows compat: create a TCP socket.

workspaceSocket :: IO Socket
workspaceSocket = socket AF_UNIX Stream defaultProtocol

getWorkspaceSockAddr :: FilePath -> IO SockAddr
getWorkspaceSockAddr fp' = do
  fp  <- makeAbsolute fp'
  tmp <- getTemporaryDirectory
  return . SockAddrUnix $ tmp </> concat ["velox-", show $ (hash $ BSC8.pack fp :: Digest MD5)]

