module Profila (policyScript) where

import Plutarch (compile)
import Plutus.V1.Ledger.Api (Script)
import Plutarch.Api.V1
  ( PCurrencySymbol
  , PTokenName
  , PValue
  , PTxInInfo
  , PScriptContext
  , PScriptPurpose (PMinting)
  , PTxOutRef
  )
import Plutarch.Monadic qualified as P


pvalueOf :: Term s (PValue :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $ plam $ \v expectedCS expectedTN -> P.do
  csMap <- plet $ pto $ pto v
  filteredCSMap <- plet $ 
    pfilter # 
    (plam $ \(pfromData . (pfstBuiltin #) -> cs) -> cs #== expectedCS) #
    csMap
  pmatch filteredCSMap $ \case
    PNil -> 0
    PCons csHead _ -> P.do
      tnMap <- plet $ pfromData $ psndBuiltin # csHead
      filteredTNMap <- plet $ 
        pfilter # 
        (plam $ \(pfromData . (pfstBuiltin #) -> tn) -> tn #== expectedTN) #
        pto tnMap
      pmatch filteredTNMap $ \case
        PNil -> 0
        PCons tnHead _ -> pfromData $ psndBuiltin # tnHead


mkPolicy :: 
  Term s 
    (PAsData PTxOutRef :--> 
     PAsData PTokenName :--> 
     PUnit :-->
     PScriptContext :-->
     PUnit
    )
mkPolicy = phoistAcyclic $ plam $ \oref' tn' _ ctx' -> P.do 
  oref <- plet $ pfromData oref'
  tn   <- plet $ pfromData tn'
  ctx <- pletFields @'["txInfo", "purpose"] ctx'

  txInfo <- pletFields @'["inputs", "mint"] ctx.txInfo
  inputs <- plet $ pmap # toTxOutRef # txInfo.inputs
  val <- plet $ pfromData txInfo.mint

  PMinting cs' <- pmatch $ pfromData ctx.purpose
  let cmpRef :: Term _ (PTxOutRef) -> Term _ PBool
      cmpRef x = (pfield @"id" # oref) #== (pfield @"id" # x)
      cs :: Term _ PCurrencySymbol
      cs = pfield @"_0" # cs'
  pif 
    ((pany # plam cmpRef # inputs) #&& ((pvalueOf # val # cs # tn) #== 1))
    (pconstant ())
    perror
  where 
    toTxOutRef :: Term s (PAsData PTxInInfo :--> PTxOutRef)
    toTxOutRef = plam $ \inInfo' -> P.do 
      inInfo <- pletFields @'["outRef"] $ pfromData inInfo'
      pfromData inInfo.outRef


policyScript :: Script
policyScript = compile mkPolicy
