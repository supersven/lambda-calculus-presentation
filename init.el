(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'ox-beamer)

(require 'package)
(setq package-user-dir
      (expand-file-name ".emacs.d/elpa"))
(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
; (package-refresh-contents)


(defconst my-packages
  '(haskell-mode))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-minted-options
      '(("fontsize" "\\scriptsize") ("frame" "lines")))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t) (emacs-lisp . t) (haskell . t))
 )

