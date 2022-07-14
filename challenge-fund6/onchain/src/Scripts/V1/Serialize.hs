{-# LANGUAGE OverloadedStrings #-}

module Scripts.V1.Serialize (writePlutusScript, writePlutusValidator, writePlutusMintingPolicy) where

import Codec.Serialise (serialise)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text

import qualified Cardano.Binary as CBOR
import Plutus.V1.Ledger.Scripts (MintingPolicy (getMintingPolicy), Script, Validator (getValidator), scriptSize)

writePlutusValidator :: String -> FilePath -> Validator -> IO ()
writePlutusValidator t f = writePlutusScript t f . getValidator

writePlutusMintingPolicy :: String -> FilePath -> MintingPolicy -> IO ()
writePlutusMintingPolicy t f = writePlutusScript t f . getMintingPolicy

writePlutusScript :: String -> FilePath -> Script -> IO ()
writePlutusScript title filepath scrpt = do
  let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
      scriptRawCBOR = CBOR.serialize' scriptSBS
      scriptType = "PlutusScriptV1" :: String
      plutusJson =
        object
          [ "type" .= scriptType
          , "description" .= title
          , "cborHex" .= Text.decodeUtf8 (Base16.encode scriptRawCBOR)
          ]
      content = encodePretty plutusJson
  print title >> putStr "Script size " >> print (scriptSize scrpt)
  LBS.writeFile filepath content
