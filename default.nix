with import <nixpkgs> {};

  let tex = texlive.combine {
      inherit (texlive)
              scheme-basic
              latex
              amsmath
              beamer
              etoolbox
              wrapfig
              ulem
              capt-of
              cm-super
#              inputenc
#              fontenc
#              graphicx
#              grffile
#              longtable
#              rotating
#              normalem
#              textcomp
#              amssymb
              hyperref
              ;
    };
  in

  stdenv.mkDerivation rec {
    name = "env";
    buildInputs = [
      tex
    ];
}
