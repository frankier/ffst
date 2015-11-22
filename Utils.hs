module Utils where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import qualified Data.Map.Strict as SM
import qualified Data.ByteString.Lazy.UTF8 as U8L
import qualified Data.ByteString.UTF8 as U8S

import Data.Binary.Get

bssMapToStringMap :: SM.Map BSS.ByteString BSS.ByteString -> SM.Map String String
bssMapToStringMap bssMap = SM.map U8S.toString $ SM.mapKeys U8S.toString bssMap

getByteStringNul :: Get BSS.ByteString
getByteStringNul = BSL.toStrict <$> getLazyByteStringNul

getStringNul :: Get String
getStringNul = U8L.toString <$> getLazyByteStringNul

getBool32 :: Get Bool
getBool32 = fmap (/= 0) getWord32le

getWord16leAsNum :: (Num a) => Get a
getWord16leAsNum =
  fmap fromIntegral getWord16le

getWord32leAsNum :: (Num a) => Get a
getWord32leAsNum = fmap fromIntegral getWord32le

azip :: (Applicative f) => f a -> f b -> f (a, b)
azip fa fb = (,) <$> fa <*> fb
