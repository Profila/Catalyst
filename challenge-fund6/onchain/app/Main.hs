module Main (main) where

import Profila (policyScript)
import Scripts.V1.Serialize (writePlutusScript)

main :: IO ()
main = do
  writePlutusScript 
    "policy script" "../scripts/policyScript.plutus"
    policyScript
