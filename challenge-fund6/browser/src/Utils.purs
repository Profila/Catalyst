module Utils (jsonReader, minLovelace) where 

import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Contract.Prelude (Either(..), Maybe(..), bind, pure, ($), (/\))
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (PlutusScript(..))
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.BigInt (BigInt, fromInt)
import Types.Scripts (Language(..))

jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ PlutusScript $ (bytes /\ PlutusV1)

minLovelace :: BigInt
minLovelace = fromInt 2000000

