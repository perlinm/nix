{
  description = "NixOS and Home Manager configuration of perlinm";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-23.05"; };
    nixpkgs-unstable = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    helix = {
      url = "github:paholg/helix/temp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    snippets-ls = {
      url = "github:quantonganh/snippets-ls";
      inputs.nixpkgs.follows = "nixpkgs";
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

        external.helix = helix.packages.${prev.system}.default;
        external.snippets-ls = snippets-ls.packages.${prev.system}.snippets-ls;
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
