module Utils (jsonReader, minLovelace) where 

import Contract.Aeson (JsonDecodeError(..))
import Contract.Prelude (Either(..), Maybe(..), bind, pure, wrap, ($))
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (PlutusScript)
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.BigInt (BigInt, fromInt)


jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ wrap bytes

minLovelace :: BigInt
minLovelace = fromInt 2000000

