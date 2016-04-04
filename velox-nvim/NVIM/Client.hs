module NVIM.Client where

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.MessagePack.Object (Object(..), toObject)
import Data.MessagePack.RPC (Answer, Request(..), Message(..), getMessage, putRequest)
import Data.Text (Text)
import Network.Socket (Family(AF_UNIX), SockAddr(SockAddrUnix), Socket, SocketType(Stream)
                      , connect, close, defaultProtocol, socket)
import Network.Socket.ByteString (sendAll, recv)

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Map as Map
import qualified Data.MessagePack as MP
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- TODO Use machines to safely ignore notification on synchronous `runCommand`.

data Command = VimCommand [Object] | VimCallFunction Text [Object]

runCommand :: Socket -> Command -> IO Answer
runCommand sk cmd = do
  sendAll sk $ toStrict $ runPut $ putRequest $ mkRequest cmd
  xs <- recv sk 1024
  return . unpack $ runGet getMessage $ fromStrict xs
    where
      unpack (Response _ a) = a
      mkRequest (VimCommand xs) =
        Request 0 (Text.pack "vim_command") $ Vector.fromList xs
      mkRequest (VimCallFunction m xs) =
        Request 0 (Text.pack "vim_call_function") $ Vector.fromList $
          [ toObject m
          , toObject $ Vector.fromList xs ]

mainClient :: IO ()
mainClient = do
  sk <- socket AF_UNIX Stream defaultProtocol
  connect sk addr
  a0 <- runCommand sk $ VimCommand [toObject "echo \"velox: hello world!\""]
  print a0
  a1 <- runCommand sk $ VimCallFunction (Text.pack "setqflist") [toObject [item], toObject "r"]
  print a1
  return ()
    where
      item = toObject . Map.fromList $
        [ ("filename", toObject "velox-nvim/Main.hs")
        , ("lnum", ObjectInt 5)
        , ("col", ObjectInt 12)
        , ("type", toObject "W") -- W Warning / E Error
        , ("text", toObject "Hello World") ]
      addr = SockAddrUnix "/run/user/1000/nvim48dWQe/0"
