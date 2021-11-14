let sources = import ../nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, callPackage ? pkgs.callPackage
, lib ? pkgs.lib

, callCabal2nix ? pkgs.callCabal2nix
, cabal-install ? pkgs.cabal-install
, haskellPackages ? pkgs.haskellPackages
, justStaticExecutables ? pkgs.haskell.lib.justStaticExecutables

, easy-hls-nix ? sources.easy-hls-nix
, getRelativeFileName ? (callPackage ../nix/utils.nix {}).getRelativeFileName

, withHoogle ? true
}:

let
  hsPkgs = haskellPackages.extend (self: super: {
    inherit gl-playground;
  });

  gl-playground =
    let
      name = "gl-playground";
      path = ./.;
      prefix = toString path;

      cleanSource = lib.cleanSourceWith {
        name = "${name}-source";
        src = lib.cleanSource path;
        filter = fileName: fileType:
          let rFileName = getRelativeFileName prefix fileName; in
          (
            (fileType == "regular") &&
            (rFileName == "gl-playground.cabal")
          )
          ||
          (
            (fileType == "directory") &&
            (builtins.match "^src(/.+)?$" rFileName != null)
          )
          ||
          (
            (fileType == "regular") &&
            (builtins.match "^src/.+\.hs$" rFileName != null)
          )
          ||
          (
            (fileType == "directory") &&
            (builtins.match "^app$" rFileName != null)
          )
          ||
          (
            (fileType == "regular") &&
            (builtins.match "^app/.+\.hs$" rFileName != null)
          );
      };
    in
      hsPkgs.callCabal2nix name cleanSource {};

  easy-hls = callPackage easy-hls-nix {
    ghcVersions = [ hsPkgs.ghc.version ];
  };
in

{
  inherit gl-playground easy-hls;
  gl-playground-exe = justStaticExecutables gl-playground;

  shell = hsPkgs.shellFor {
    packages = p: [ gl-playground ];
    inherit withHoogle;

    buildInputs = [
      cabal-install
      easy-hls
      hsPkgs.hlint
      hsPkgs.hoogle
    ];
  };
}
