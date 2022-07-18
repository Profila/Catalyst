module Test.Main (main) where

import Contract (mintFeedbackNFT)
import Contract.Address (NetworkId(..))
import Contract.Monad (ConfigParams(..), LogLevel(..), defaultDatumCacheWsConfig, defaultOgmiosWsConfig, defaultServerConfig, liftContractE, liftContractM, mkContractConfig, runContract_)
import Contract.Prelude (Effect, Either, Maybe(..), Unit, bind, wrap, ($), (=<<))
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (PlutusScript)
import Contract.Value as Value
import Contract.Wallet.KeyFile (mkKeyWalletFromFile)
import Control.Monad.Error.Class (liftMaybe)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Effect.Aff (error, launchAff_)
import Serialization.Hash (ed25519KeyHashFromBytes)
import Types.RawBytes (hexToRawBytesUnsafe)
import Utils (jsonReader)

-- | Run test on the testnet. Mint token from wallet `testdata/wallet1` 
--   and send to `testdata/wallet2`
main :: Effect Unit
main = launchAff_ $ do
  priv' <- mkKeyWalletFromFile "test/testdata/wallet1/payment.skey"
  priv <- liftMaybe (error "Can't get private key") priv'

  pkh <- liftMaybe (error "Failed to parse public key hash") $ 
           ed25519KeyHashFromBytes $ hexToRawBytesUnsafe 
             "6d183d0c967f2f6d42dc2211350a4cf0956937bdf9c36a6ded40449d"

  cfg <- mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Trace
    , extraConfig: {}
    , wallet: Just priv
    }

  runContract_ cfg $ do
    tn <- liftContractM "Cannot make token name" $ Value.mkTokenName =<< byteArrayFromAscii "TheToken"
    mintingPolicyScript <- liftContractE mintingPolicyScript'
    let fp = { mintingPolicyScript: mintingPolicyScript, tokenName: tn, beneficiary: wrap (wrap pkh) }
    (mintFeedbackNFT fp)

mintingPolicyScript' :: Either JsonDecodeError PlutusScript
mintingPolicyScript' = jsonReader "script" _mintingPolicy

foreign import _mintingPolicy :: Json
