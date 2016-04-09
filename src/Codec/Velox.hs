module Codec.Velox where

import Data.Text (Text)
import Data.Binary (Get, Put)
import Data.Vector

import qualified Data.MessagePack.Get as Get
import qualified Data.MessagePack.Put as Put

data Event
  = Start { title :: Text }
  | Finish { errors :: Int, warnings :: Int }
  | Notify { message :: Message }

getEvent :: Get Event
getEvent = do
  tpe <- Get.getInt
  case tpe of
    0 -> Start <$> Get.getStr
    1 -> Finish <$> Get.getInt <*> Get.getInt
    2 -> Notify <$> getMessage
    _ -> fail "unsupported event type"

putEvent :: Event -> Put
putEvent (Start t) = Put.putInt 0 *> Put.putStr t
putEvent (Finish es ws) = Put.putInt 1 *> Put.putInt es *> Put.putInt ws
putEvent (Notify m) = Put.putInt 2 *> putMessage m

data Message = Message Location Level (Vector Text)
  deriving Show

getMessage :: Get Message
getMessage = Message <$> getLocation <*> getLevel <*> Get.getArray Get.getStr

putMessage :: Message -> Put
putMessage (Message loc lvl txt) = putLocation loc *> putLevel lvl *> Put.putArray Put.putStr txt

data Location = Location { filePath :: Text, column :: Int, line :: Int }
  deriving Show

getLocation :: Get Location
getLocation = Location <$> Get.getStr <*> Get.getInt <*> Get.getInt

putLocation :: Location -> Put
putLocation (Location fp c l) = Put.putStr fp *> Put.putInt c *> Put.putInt l

data Level = Warning | Error
  deriving (Enum, Show)

getLevel :: Get Level
getLevel = fmap toEnum Get.getInt

putLevel :: Level -> Put
putLevel = Put.putInt . fromEnum

