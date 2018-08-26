(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'ox-beamer)

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-minted-options
      '(("fontsize" "\\tiny") ("frame" "lines")))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t) (emacs-lisp . t) (haskell . t))
 )

