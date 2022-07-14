module ProfilaOffchain (mintFeedbackNFT, FeedbackParams (..), endpoints) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Fixed (Micro)
import Data.Map qualified as Map (keys)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (
  MintingPolicy (MintingPolicy),
  applyArguments,
  scriptCurrencySymbol,
 )
import Ledger.Address (PaymentPubKeyHash, pubKeyHashAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Script)
import Ledger.Tx (getCardanoTxId)
import Ledger.Value qualified as Value
import Plutus.Contract (
  AsContractError,
  Contract,
  Endpoint,
  awaitPromise,
  awaitTxConfirmed,
  endpoint,
  logError,
  logInfo,
  ownPaymentPubKeyHash,
  submitTxConstraintsWith,
  utxosAt,
 )
import Plutus.V1.Ledger.Ada (adaValueOf)
import PlutusTx qualified

data FeedbackParams = FeedbackParams
  { mintingPolicyScript :: !Script
  , tokenName :: !Value.TokenName
  , beneficiary :: !PaymentPubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

type MintingSchema = Endpoint "MintFeedbackNFT" FeedbackParams

mintFeedbackNFT :: AsContractError e => FeedbackParams -> Contract w s e ()
mintFeedbackNFT fp = do
  pkh <- ownPaymentPubKeyHash
  utxos <- utxosAt (pubKeyHashAddress pkh Nothing)

  case Map.keys utxos of
    [] -> logError @String "no utxo found, can't mint NFT"
    (oref : _) -> do
      -- create minting policy from the script
      let mintingPolicy =
            MintingPolicy $
              applyArguments
                (mintingPolicyScript fp)
                [PlutusTx.toData oref, PlutusTx.toData tn]
          tn = tokenName fp
          value = Value.singleton (scriptCurrencySymbol mintingPolicy) tn 1
          lookups =
            Constraints.mintingPolicy mintingPolicy
              <> Constraints.unspentOutputs utxos
          tx =
            Constraints.mustMintValue value
              <> Constraints.mustSpendPubKeyOutput oref
              <> Constraints.mustPayToPubKey
                (beneficiary fp)
                (value <> adaValueOf minAda)
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "NFT minted and sent to " ++ (show . beneficiary $ fp)

endpoints :: Contract () MintingSchema Text ()
endpoints = awaitPromise mintFeedbackNFT' >> endpoints
  where
    mintFeedbackNFT' = endpoint @"MintFeedbackNFT" mintFeedbackNFT

minAda :: Micro
minAda = 2
