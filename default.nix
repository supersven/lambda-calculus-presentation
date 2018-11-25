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
              hyperref
              minted
              fvextra
              fancyvrb
              upquote
              lineno
              ifplatform
              xstring
              framed
              caption
              pgfopts
              float
              marvosym
              wasysym
              fontspec
              xetex
              collection-fontsrecommended
              translator
              ;
    };
  in

  stdenv.mkDerivation rec {
    name = "env";
    buildInputs = [
      tex python36Packages.pygments emacs25 graphviz gnumake tetex stack
    ];
}
