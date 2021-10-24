let sources = import nix/sources.nix; in
{ pkgs ? import sources.nixpkgs {}
, project-path ? ./.
}:

let
  getRelativeFileName = prefix: fileName:
    # +1 and -1 for the slash after the prefix
    builtins.substring
      (builtins.stringLength prefix + 1)
      (builtins.stringLength fileName - builtins.stringLength prefix - 1)
      fileName;

  src = let
    filter = prefix: fileName: fileType:
      let relativeFileName = getRelativeFileName prefix fileName; in
      (
        (fileType == "regular") &&
        (relativeFileName == "Makefile")
      )
      ||
      (
        (fileType == "directory") &&
        (builtins.match "^src(/.+)?$" relativeFileName != null)
      )
      ||
      (
        (fileType == "regular") &&
        (builtins.match "^src/.+\.(hpp|cpp|glsl)$" relativeFileName != null)
      );
  in
    assert builtins.isPath project-path;
    builtins.filterSource (filter (toString project-path)) project-path;
in

pkgs.stdenv.mkDerivation rec {
  name = "playing-with-glsl";
  inherit src;
  env = pkgs.buildEnv { inherit name; paths = buildInputs; };

  nativeBuildInputs = [
    pkgs.gcc
    pkgs.gnumake
    pkgs.pkg-config
    pkgs.unixtools.xxd
  ];

  buildInputs = [
    pkgs.glfw3
    pkgs.libglvnd
    pkgs.glew
    pkgs.xorg.libX11
  ];

  buildPhase = ''
    make build
  '';

  installPhase = ''
    mkdir -p -- "$out/bin"
    make install "PREFIX=$out"
  '';

  # shellHook = ''
  #   echo hi
  # '';
}
