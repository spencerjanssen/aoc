{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.mkShell {
  buildInputs = [
    (nixpkgs.haskellPackages.ghcWithPackages (p: [p.attoparsec p.haskell-language-server]))
  ];
}
