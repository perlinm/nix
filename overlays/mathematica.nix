{ pkgs }:
self: super: {
  mathematia = super.mathematica.override {
    source = pkgs.requireFile {
      name = "Mathematica_XX.X.X_BNDL_LINUX.sh";
      # Get this hash via a command similar to this:
      # nix-store --query --hash \
      # $(nix store add-path Mathematica_XX.X.X_BNDL_LINUX.sh --name 'Mathematica_XX.X.X_BNDL_LINUX.sh')
      sha256 = "0000000000000000000000000000000000000000000000000000";
      message = ''
        Your override for Mathematica includes a different src for the installer,
        and it is missing.
      '';
      hashMode = "recursive";
    };
  };
}
