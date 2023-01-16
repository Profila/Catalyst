# plutus-scaffold
This project contains the build systems and scripts needed to scaffold a plutus project quickly.

## Opening a shell

- `nix develop .#offchain`
- `nix develop .#onchain`
- `cabal repl --repl-options -Wwarn`

## Running
To get everything set up, copy the contents of this repo to your new project. You will then need to run the setup.sh script, which can take 0, 2, or 3 arguments.
* When given 0 arguments, it will prompt you to provide the project name, module name, and github URL.
* When given 2 arguments, the first should be the project name, and the second should be the module name, e.g., `./setup.sh gero-gov GeroGov`. In this case, you will still be prompted for a github URL.
* Finally, you may provide all three pieces of information at the command line, e.g., `./setup.sh gero-gov GeroGov https://github.com/mlabs-haskell/gero-gov`.

## Script Compilation and Loading
Due to the repo split, it might seem non-trivial to obtain the validators and minting policies from the onchain segment, into the offchain segment. However, all you have to do is compile the Plutarch scripts using `Plutarch.compile` and write the resulting script into a file - ideally using something like [`writeFileTextEnvelope`](https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-SerialiseTextEnvelope.html#v:writeFileTextEnvelope). You can then later read the resulting file in your offchain project using [`readFileTextEnvelope`](https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-SerialiseTextEnvelope.html#v:writeFileTextEnvelope).

However, `cardano-api` is not available in the onchain project, so you can't actually use `writeFileTextEnvelope`. Thus, the scaffold
includes a monomorphized implementation of `writeFileTextEnvelope` in `Scripts.V1.Serialize`.

```hs
writePlutusValidator :: String -> FilePath -> Validator -> IO ()

writePlutusMintingPolicy :: String -> FilePath -> MintingPolicy -> IO ()

writePlutusScript :: String -> FilePath -> Script -> IO ()
```

You can use these functions to write your scripts to the filesystem. Finally, in your offchain project, you should use `readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1)` to read the file and deserialize it as a [`Script`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Script), then you can wrap it into a [`Validator`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Validator) or [`MintingPolicy`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:MintingPolicy) as necessary.

This is also facilitated by the offchain scaffold. In particular, you can use the functions in `Scripts.V1.Deserialize` in the offchain project to load up these scripts.

```hs
readPlutusValidator :: FilePath -> IO Validator

readPlutusMintingPolicy :: FilePath -> IO MintingPolicy

readPlutusScript :: FilePath -> IO Script
```

**NOTE**: You might need to add the compiled scripts filepaths to `extra-source-files` in your offchain project's cabal file to be able to access it.

## Potential Issues
* If you submit your pull request, but get an error on the GitHub CIs saying something to the effect of "Binary cache mlabs doesn't exist or it's private," or that MLabs.cachix.com doesn't exist, then the cachix key is not setup, and you or whoever owns your repository will have to add that.
* If you get an error saying that "The package directory './.' does not contain any .cabal file," then you probably should either start with a fresh repository with no commits, or you should make a commit with git and rerun `nix-build nix/ci.nix`. This issue arises because nix is looking in your .git folder to try and identify what your .cabal file is, and since that has been renamed, nix seems to assume that there is no .cabal file. Starting with a fresh repo causes it to search the directory for your .cabal file, and making a commit changes the file name in the .git folder.

## Nix cache

You must have the following in your nix.conf:
```
substituters = https://public-plutonomicon.cachix.org https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

For MLabs cache:
```
cachix authtoken eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJjJDI5NjUiLCJqdGkiOiIyNDc3MmJiMC0yYmFhLTQwMzItYjViNi03YTFlNjkwZDVlZDgiLCJzY29wZXMiOiJjYWNoZSJ9.UIE0NrDx8Xt3mkQY9GIw3Orz7HmXAU7A9t7dnpjXmiU
cachix use mlabs
```

## Demonstration 

https://profilaorg.sharepoint.com/sites/product/_layouts/15/stream.aspx?id=%2Fsites%2Fproduct%2FShared%20Documents%2FProduct3%2FMLabs%20Delivery%2FProfila%20Video%20Tour%2Emp4

### Troubleshooting

See: [Nix cache tips / troubleshooting](https://mlabs.slab.com/posts/mlabs-cachix-key-o6sx2nrm#h89kq-nix-cache-tips-troubleshooting)
