{
  description = "profila";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]profila \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip?rev=017fbaf9a080c592a8882abd03e331c1a05be3f2";

    plutarch.url = "github:Plutonomicon/plutarch";
    plutarch.inputs.haskell-nix.follows = "plutip/haskell-nix";
    plutarch.inputs.nixpkgs.follows = "plutip/nixpkgs";

    cardano-transaction-lib = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      # Should match the same revision as the one in `packages.dhall`
      rev = "9954427f28d949743da5b36c801facd13fb0426d";
    };

    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };


  outputs = inputs@{ self, nixpkgs, haskell-nix, plutarch, plutip, cardano-transaction-lib, ... }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      ctlNixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ cardano-transaction-lib.overlay.${system} ];
      };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              (pkgs.haskell-nix.tools onchain.ghcVersion { inherit (plutarch.tools) fourmolu; }).fourmolu
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      deferPluginErrors = true;

      pursVersion = "purs-0_14_5";

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc921";

        projectFor = system:
          let pkgs = nixpkgsFor system; in
          let pkgs' = nixpkgsFor' system; in
          (nixpkgsFor system).haskell-nix.cabalProject' {
            src = ./onchain;
            compiler-nix-name = ghcVersion;
            inherit (plutarch) cabalProjectLocal;
            extraSources = plutarch.extraSources ++ [
              {
                src = inputs.plutarch;
                subdirs = [ "." ];
              }
            ];
            modules = [ (plutarch.haskellModule system) ];
            shell = {
              withHoogle = true;

              exactDeps = true;

              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.fd
                pkgs'.haskellPackages.apply-refact
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.hlint
                pkgs'.nixpkgs-fmt
              ];

              inherit (plutarch) tools;

              additional = ps: [
                ps.plutarch
                ps.tasty-quickcheck
              ];
            };
          };
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = rec {
        ghcVersion = "ghc8107";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            plutipin = inputs.plutip.inputs;
            fourmolu = pkgs.haskell-nix.tool "ghc921" "fourmolu" { };
            project = pkgs.haskell-nix.cabalProject' {
              src = ./offchain;
              compiler-nix-name = ghcVersion;
              inherit (plutip) cabalProjectLocal;
              extraSources = plutip.extraSources ++ [
                {
                  src = "${plutip}";
                  subdirs = [ "." ];
                }
              ];
              modules = [
                ({ config, ... }: {
                  packages.profila-offchain.components.tests.profila-offchain-test.build-tools = [
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];

                })
              ] ++ plutip.haskellModules;

              shell = {
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.haskellPackages.apply-refact
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.hlint
                  pkgs'.nixpkgs-fmt

                  project.hsPkgs.cardano-cli.components.exes.cardano-cli
                  project.hsPkgs.cardano-node.components.exes.cardano-node

                  fourmolu
                ];

                tools.haskell-language-server = { };

                additional = ps: [ ps.plutip ];
              };
            };
          in
          project;
      };

      # CTL / Web
      psProjectFor = system:
        let
          pkgs = ctlNixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          src = ./browser;
          nodejs = pkgs.nodejs-14_x;
          easy-ps = import inputs.easy-purescript-nix { inherit pkgs'; };
          spagoPkgs = import (src + /spago-packages.nix) { inherit pkgs'; };
          nodePkgs =
            import (src + /node2nix.nix) { inherit pkgs system nodejs; };
          purs = easy-ps.${pursVersion};
        in
        pkgs.purescriptProject {
          inherit pkgs src spagoPkgs purs easy-ps system;
          projectName = "profila";
          shell = {
            packages = [ pkgs'.fd ];
          };
        };
    in
    {
      inherit nixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      browser = perSystem psProjectFor;

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // self.offchain.flake.${system}.packages
        // {
          profila-build-web = (psProjectFor system).buildPursProject {
            sources = [ "src" ];
          };
          profila-bundle-web = (psProjectFor system).bundlePursProject {
            sources = [ "src" ];
            main = "Main";
          };
        }
      );
      apps = perSystem (system: {
        ctl-runtime = (ctlNixpkgsFor system).launchCtlRuntime { };
      });
      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // self.offchain.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
        }
      );
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
        browser = self.browser.${system}.devShell;
      });

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}

