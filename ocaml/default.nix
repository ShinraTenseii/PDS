with import <nixpkgs> {};

with ocamlPackages;
stdenv.mkDerivation rec {
  name = "pds-tp2-ocaml";
  buildInputs = [ocaml findlib camlp4 clang_4];
}
