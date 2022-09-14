module Contract (mintFeedbackNFT, FeedbackParams) where

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeyHash, pubKeyHashAddress)
import Contract.Log (logError', logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prelude (Maybe(..), Unit, bind, show, ($), (<>), (=<<))
import Contract.ScriptLookups as Lookups
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (PlutusScript, MintingPolicy(..), applyArgs)
import Contract.Transaction (balanceAndSignTx, submit)
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf, scriptCurrencySymbol)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Aeson (encode, toJSON)
import Data.BigInt as BigInt
import Data.List (List(..), (:))
import Data.Map.Internal as Map
import Data.Void (Void)
import Effect.Aff (error)
import Prelude (discard)
import Utils (minLovelace)

type FeedbackParams =
  { mintingPolicyScript :: PlutusScript
  , tokenName :: Value.TokenName
  , beneficiary :: PaymentPubKeyHash
  }

mintFeedbackNFT :: forall r. FeedbackParams -> Contract r Unit
mintFeedbackNFT fp = do
  pkh <- liftMaybe (error "Can't get own payment public key hash") =<< ownPaymentPubKeyHash
  utxos <- liftMaybe (error "Can't get UTXOs at address") =<< utxosAt (pubKeyHashAddress pkh Nothing)
  case Map.keys utxos of
    Nil -> logError' "no utxo found, can't mint NFT"
    (oref : _) -> do
      let
        tn = _.tokenName fp

        mintingPolicyScript :: PlutusScript
        mintingPolicyScript = _.mintingPolicyScript fp
      logInfo' "Applying Args"
      mintingPolicy <- liftedE $ applyArgs (MintingPolicy mintingPolicyScript) [ toData oref, toData tn ]
      logInfo' "Getting currency symbol"
      cs <- liftContractM "Couldn't get currency symbol from minting policy" $ scriptCurrencySymbol mintingPolicy
      let
        value = Value.singleton cs tn (BigInt.fromInt 1)
        lookups :: ScriptLookups.ScriptLookups Void
        lookups =
          ScriptLookups.mintingPolicy mintingPolicy
            <> ScriptLookups.unspentOutputs utxos

        tx =
          TxConstraints.mustMintValue value
            <> TxConstraints.mustSpendPubKeyOutput oref
            <> TxConstraints.mustPayToPubKey
              (_.beneficiary fp)
              (value <> lovelaceValueOf minLovelace)
      ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups tx
      bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
      txId <- submit bsTx
      logInfo' $ "Tx ID: " <> show txId
