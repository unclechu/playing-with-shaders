let sources = import ../nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, callPackage ? pkgs.callPackage
, lib ? pkgs.lib

, haskell ? pkgs.haskell
, justStaticExecutables ? haskell.lib.justStaticExecutables
, callCabal2nix ? pkgs.callCabal2nix
, cabal-install ? pkgs.cabal-install

# “easy-hls” doesn’t support GHC 8.10.4 which was the default one in the
# “release-21.05” nixpkgs pin at the moment of writing this.
# P.S. Some dependencies are broken in “haskell.packages.ghc901”.
, haskellPackages ? haskell.packages.ghc8107

, easy-hls-nix ? sources.easy-hls-nix
, getRelativeFileName ? (callPackage ../nix/utils.nix {}).getRelativeFileName

, withHoogle ? true

, projectName ? "gl-playground"
, projectPath ? ./.
, shadersPath ? ./shaders # directory or a symlink to a directory
}:

let
  esc = lib.escapeShellArg;

  hsPkgs = haskellPackages.extend (self: super: {
    ${projectName} =
      super.callCabal2nix projectName glPlaygroundFullSource {};
  });

  glPlaygroundFullSource = pkgs.symlinkJoin {
    name = "${projectName}-full-source";
    paths = [ glPlaygroundSrcStorePath shadersStorePath ];
  };

  glPlaygroundSrcStorePath = let
    path = projectPath;
    prefix = toString path;
  in lib.cleanSourceWith {
    name = "${projectName}-source";
    src = lib.cleanSource path;
    filter = fileName: fileType:
      let rFileName = getRelativeFileName (toString path) fileName; in
      (
        (fileType == "regular") &&
        (rFileName == "gl-playground.cabal")
      )
      ||
      (
        (fileType == "directory") &&
        (builtins.match "^(src|test)(/.+)?$" rFileName != null)
      )
      ||
      (
        (fileType == "regular") &&
        (builtins.match "^(src|test)/.+\.hs$" rFileName != null)
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

  shadersStorePath =
    let
      paths = lib.pipe shadersPath [
        builtins.readDir
        (lib.filterAttrs (n: v: v == "directory"))
        (builtins.mapAttrs (n: v: shadersPath + "/${n}"))
        (builtins.mapAttrs (_: takeShaderSrc))
      ];

      takeShaderSrc =
        let
          filter = prefix: fileName: fileType:
            let rFileName = getRelativeFileName prefix fileName; in
            (fileType == "regular") &&
            (builtins.match "^.+\.glsl$" rFileName != null);
        in
        path: builtins.filterSource (filter (toString path)) path;
    in
    pkgs.runCommand "${projectName}-shaders-source" {} ''
      set -u || exit
      mkdir -p -- "$out"/shaders
      ${builtins.concatStringsSep "\n" (
        lib.mapAttrsToList (n: v: ''
          ln -s -- ${esc "${v}"} "$out"/shaders/${esc n}
        '') paths
      )}
    '';

  easy-hls = callPackage easy-hls-nix {
    ghcVersions = [ hsPkgs.ghc.version ];
  };
in
{
  inherit easy-hls;
  gl-playground = hsPkgs.${projectName};
  gl-playground-exe = justStaticExecutables hsPkgs.${projectName};

  shell = hsPkgs.shellFor {
    packages = p: [ p.${projectName} ];
    inherit withHoogle;

    buildInputs = [
      cabal-install
      easy-hls
      hsPkgs.hlint
      hsPkgs.hoogle
    ];
  };
}
