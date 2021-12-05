let sources = import ../nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, callPackage ? pkgs.callPackage
, lib ? pkgs.lib

, haskell ? pkgs.haskell
, justStaticExecutables ? haskell.lib.justStaticExecutables
, cabal-install ? pkgs.cabal-install

, haskellPackages ? haskell.packages.ghc921

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
      self.callCabal2nix projectName glPlaygroundFullSource {};

    # Taken from “nixpkgs” “master”. In “release-21.11” it’s 1.0.8 and that
    # version has outdated version bounds for “base” package.
    cabal-doctest = haskell.lib.overrideCabal super.cabal-doctest (_: {
      version = "1.0.9";
      sha256 = "0wxs0xkspc80h0g8ks792lrzldxvcnhc9rja1j0k678ijs20hmjm";
      revision = null;
      editedCabalFile = null;
    });

    # 3.3.4.0 has this issue:
    #   Setup: Encountered missing or private dependencies:
    #   bytestring >=0.9 && <0.11
    OpenGLRaw = haskell.lib.overrideCabal super.OpenGLRaw (_: {
      version = "3.3.4.1";
      sha256 = "07nk0rgm6jcxz6yshwhv5lj5frs6371w3hdjxwa4biws2kmbs6hj";
    });

    # Configuring OpenGL-3.0.3.0...
    #
    # Setup: Encountered missing or private dependencies:
    # bytestring >=0.9 && <0.11
    #
    # There is no update for “OpenGL” package yet.
    #
    OpenGL = haskell.lib.doJailbreak super.OpenGL;

    # See https://github.com/sjakobi/bsb-http-chunked/issues/38
    bsb-http-chunked = haskell.lib.dontCheck super.bsb-http-chunked;

    # Building library for warp-3.3.17..
    #
    # …
    #
    # Network/Wai/Handler/Warp/ReadInt.hs:44:39: error:
    #     • Couldn't match expected type ‘Word#’ with actual type ‘Word8#’
    #     • In the first argument of ‘word2Int#’, namely
    #         ‘(indexWord8OffAddr# addr (word2Int# i))’
    #       In the first argument of ‘I#’, namely
    #         ‘(word2Int# (indexWord8OffAddr# addr (word2Int# i)))’
    #       In the expression:
    #         I# (word2Int# (indexWord8OffAddr# addr (word2Int# i)))
    #    |
    # 44 | mhDigitToInt (W8# i) = I# (word2Int# (indexWord8OffAddr# addr (word2Int# i)))
    #    |                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #
    # Network/Wai/Handler/Warp/ReadInt.hs:44:74: error:
    #     • Couldn't match expected type ‘Word#’ with actual type ‘Word8#’
    #     • In the first argument of ‘word2Int#’, namely ‘i’
    #       In the second argument of ‘indexWord8OffAddr#’, namely
    #         ‘(word2Int# i)’
    #       In the first argument of ‘word2Int#’, namely
    #         ‘(indexWord8OffAddr# addr (word2Int# i))’
    #    |
    # 44 | mhDigitToInt (W8# i) = I# (word2Int# (indexWord8OffAddr# addr (word2Int# i)))
    #
    # ---
    #
    # Also some tests are failing:
    #
    # Failures:
    #
    #   test/WithApplicationSpec.hs:20:5:
    #   1) WithApplication.withApplication runs a wai Application while executing the given action
    #        uncaught exception: IOException of type NoSuchThing
    #        curl: readCreateProcess: posix_spawnp: does not exist (No such file or directory)
    #
    #   To rerun use: --match "/WithApplication/withApplication/runs a wai Application while executing the given action/"
    #
    #   test/WithApplicationSpec.hs:26:5:
    #   2) WithApplication.withApplication does not propagate exceptions from the server to the executing thread
    #        uncaught exception: IOException of type NoSuchThing
    #        curl: readCreateProcess: posix_spawnp: does not exist (No such file or directory)
    #
    #   To rerun use: --match "/WithApplication/withApplication/does not propagate exceptions from the server to the executing thread/"
    #
    #   test/WithApplicationSpec.hs:33:5:
    #   3) WithApplication.testWithApplication propagates exceptions from the server to the executing thread
    #        uncaught exception: IOException of type NoSuchThing
    #        curl: readCreateProcess: posix_spawnp: does not exist (No such file or directory)
    #
    #   To rerun use: --match "/WithApplication/testWithApplication/propagates exceptions from the server to the executing thread/"
    warp = haskell.lib.dontCheck (haskell.lib.overrideCabal super.warp (_: {
      version = "3.3.18";
      sha256 = "1m93s3p2zz00fdgkisl6sbnqnc6vvq0vz997i5y4mk9a3ssjflqw";
    }));

    # Not supported yet for 9.2.1
    # See https://github.com/ndmitchell/hlint/issues/1314
    hlint = null;
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

  # FIXME Wait before 9.2.x is supported
  #       See https://github.com/haskell/haskell-language-server/issues/2179
  #
  # easy-hls = callPackage easy-hls-nix {
  #   ghcVersions = [ hsPkgs.ghc.version ];
  # };
in
{
  # inherit easy-hls;
  gl-playground = hsPkgs.${projectName};
  gl-playground-exe = justStaticExecutables hsPkgs.${projectName};

  shell = hsPkgs.shellFor {
    packages = p: [ p.${projectName} ];
    inherit withHoogle;

    buildInputs = [
      hsPkgs.cabal-install
      # easy-hls # FIXME not supported yet for 9.2.1
      # hsPkgs.hlint # FIXME not supported yet for 9.2.1
      hsPkgs.hoogle
    ];
  };
}
