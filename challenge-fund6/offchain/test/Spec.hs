module Main (main) where

import Cardano.Prelude (Text)
import Data.Default (Default (def))
import Ledger (PaymentPubKeyHash, Script)
import Paths_profila_offchain (getDataFileName)
import Plutus.Contract (AsContractError, Contract)
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import ProfilaOffchain (
  FeedbackParams (
    FeedbackParams,
    beneficiary,
    mintingPolicyScript,
    tokenName
  ),
  mintFeedbackNFT,
 )
import Scripts.V1.Deserialize (readPlutusScript)
import Test.Plutip.Contract (assertExecution, initAda, withContract)
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Predicate (shouldSucceed)
import Test.Tasty (TestTree, defaultMain)

tests :: Script -> Test.Tasty.TestTree
tests pScript =
  withConfiguredCluster
    def
    "Integration tests"
    [ assertExecution
        "Mint Feedback NFT"
        (initAda [100] <> initAda [100])
        (withContract (mkContract' pScript))
        [shouldSucceed]
    ]

mkContract' :: Script -> [PaymentPubKeyHash] -> Contract () EmptySchema Text ()
mkContract' = mkContract

mkContract ::
  AsContractError e =>
  Script ->
  [PaymentPubKeyHash] ->
  Contract w s e ()
mkContract pScript (pkh : _) = mintFeedbackNFT fp
  where
    fp =
      FeedbackParams
        { mintingPolicyScript = pScript
        , tokenName = "Test"
        , beneficiary = pkh
        }
mkContract _ _ = error "This shouldn't happen"

main :: IO ()
main = do
  scrPath <- scriptPath
  print scrPath
  pScript <- readPlutusScript scrPath
  Test.Tasty.defaultMain (tests pScript)

scriptPath :: IO String
scriptPath = getDataFileName "scripts/policyScript.plutus"
