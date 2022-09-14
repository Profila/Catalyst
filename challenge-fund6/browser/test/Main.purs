module Test.Main (main) where

import Contract (mintFeedbackNFT)
import Contract.Address (NetworkId(..))
import Contract.Monad (ConfigParams(..), defaultDatumCacheWsConfig, defaultOgmiosWsConfig, defaultServerConfig, liftContractE, liftContractM, runContract)
import Contract.Prelude (Effect, Either, Maybe(..), Unit, bind, wrap, ($), (=<<), (/\))
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (PlutusScript(..))
import Contract.Value as Value
import Contract.Wallet.KeyFile (mkKeyWalletFromFiles)
import Control.Monad.Error.Class (liftMaybe)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Log.Level (LogLevel(..))
import Effect.Aff (error, launchAff_)
import Serialization.Address (intToNetworkId)
import Serialization.Hash (ed25519KeyHashFromBytes)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.RawBytes (hexToRawBytesUnsafe)
import Types.Scripts (Language(..))
import Utils (jsonReader)
import Wallet.Spec (WalletSpec(..), PrivatePaymentKeySource(..))

-- | Run test on the testnet. Mint token from wallet `testdata/wallet1` 
--   and send to `testdata/wallet2`
main :: Effect Unit
main = launchAff_ $ do
  pkh <- liftMaybe (error "Failed to parse public key hash") $ 
           ed25519KeyHashFromBytes $ hexToRawBytesUnsafe 
             "2ce973a21391dfd058ec8d641a3976a9ae3e5457a7741d671111a92c"

  netId <- liftMaybe (error "Invalid network id") $ intToNetworkId 0
  let cfg = { 
    ogmiosConfig: defaultOgmiosWsConfig,
    datumCacheConfig: defaultDatumCacheWsConfig,
    ctlServerConfig: Just defaultServerConfig,
    networkId: netId,
    logLevel: Trace,
    extraConfig: {},
    walletSpec: Just $ UseKeys (PrivatePaymentKeyFile "test/testdata/wallet1/payment.skey") Nothing,
    customLogger: Nothing,
    suppressLogs: false
  }

  runContract cfg $ do
    tn <- liftContractM "Cannot make token name" $ Value.mkTokenName =<< byteArrayFromAscii "TheToken"
    let fp = { mintingPolicyScript: mintingPolicyScript, tokenName: tn, beneficiary: wrap (wrap pkh) }
    (mintFeedbackNFT fp)

mintingPolicyScript :: PlutusScript
mintingPolicyScript = PlutusScript (hexToByteArrayUnsafe
  "590152010000323232323232323232323222223232323232323232533300f3370e90000010a999807a99911998088010008a50332301222533301500114a02a66446660280042944004c00cc0600044c008c05c0048cdd7980b180b806980b180b800802899b8733223253330150011480004c8c94ccc05c004520001375a6038603400266022466e3cdd7180a8008020009bab301a30180013300f23371e6eb8c04c00400c018dd7180a800804240042930b0b180b00118070009baa301230110043756602264602260220026020004664601a44a666020002297ae01330113003301300130023012001230113012001375860200026020601e002601e0046eb800c8c014894ccc02000440104c94ccc018c0100044cc028004c00cc02c0084c00cc02c008c02c0052f5bded8c0ae6955ce919180111980100100091801119801001000aab9f5740ae895d0918011baa0015573c1" /\ PlutusV1)
