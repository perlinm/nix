{
  description = "NixOS and Home Manager configuration of perlinm";

  inputs = {
    # nixos repositories
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-23.05"; };
    nixpkgs-unstable = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # common dependencies of external repositories
    naersk = {
      url = "github:nix-community/naersk/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";

    # external repositories
    trashy = {
      # url = "github:oberblastmeister/trashy";
      url = "github:perlinm/trashy";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.naersk.follows = "naersk";
      inputs.flake-utils.follows = "flake-utils";
    };
    helix = {
      # url = "github:paholg/helix/temp";
      url = "github:paholg/helix/mikabranch";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    simple-completion-language-server = {
      url = "github:estin/simple-completion-language-server";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.naersk.follows = "naersk";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs:
    with inputs;
    let
      linux = "x86_64-linux";
      pkgs = import nixpkgs {
        system = linux;
        overlays = [ pkgs_overlay ];
      };

      pkgs_overlay = final: prev: {
        unfree = import nixpkgs {
          system = prev.system;
          config.allowUnfree = true;
        };
        unstable = import nixpkgs-unstable { system = prev.system; };
        unstable-unfree = import nixpkgs-unstable {
          system = prev.system;
          config.allowUnfree = true;
        };

        external.trashy = trashy.defaultPackage.${prev.system};
        external.helix = helix.packages.${prev.system}.default;
        external.simple-completion-language-server =
          simple-completion-language-server.defaultPackage.${prev.system};
      };

    in {
      nixosConfigurations.map-work = nixpkgs.lib.nixosSystem {
        inherit pkgs;
        system = linux;
        modules = [ ./nixos.nix home-manager.nixosModules.home-manager ];
      };

      homeConfigurations.perlinm = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
      };
    };
}
