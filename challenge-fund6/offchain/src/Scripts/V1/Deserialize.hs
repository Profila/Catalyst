module Scripts.V1.Deserialize (readPlutusScript, readPlutusValidator, readPlutusMintingPolicy) where

import Codec.Serialise (deserialise)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Short as SBS

import Cardano.Api.Shelley (
  AsType (AsPlutusScript, AsPlutusScriptV1),
  PlutusScript (PlutusScriptSerialised),
  readFileTextEnvelope,
 )

import qualified Plutus.V1.Ledger.Scripts as PlutusScript

readPlutusValidator :: FilePath -> IO PlutusScript.Validator
readPlutusValidator = fmap PlutusScript.Validator . readPlutusScript

readPlutusMintingPolicy :: FilePath -> IO PlutusScript.MintingPolicy
readPlutusMintingPolicy = fmap PlutusScript.MintingPolicy . readPlutusScript

readPlutusScript :: FilePath -> IO PlutusScript.Script
readPlutusScript path = do
  eith <- readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) path
  case eith of
    Left err -> fail $ show err
    Right (PlutusScriptSerialised scriptSBS) -> pure . deserialise . BS.fromStrict $ SBS.fromShort scriptSBS
