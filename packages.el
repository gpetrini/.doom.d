;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
(package! org-ref)
;(package! interleave)                                        not using anymore as org-noter is far superior.
(package! ivy-bibtex)
;; (package! ox-pandoc)
;; (package! org-download)
;; (package! org-pretty-table-mode
;;   :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-pretty-tags)
;; (package! sublimity)
;; (package! org-fancy-priorities)
(package! info-colors) ; pretty colors
(package! exec-path-from-shell)
(package! elfeed)
(package! org-bullets)
;; (package! peep-dired)
(package! rainbow-mode)
(package! writeroom-mode)
(package! zen-mode)
(package! ox-reveal)
(package! academic-phrases)
;; (package! org-fragtog)
(package! citeproc-org)
;; (package! org-superstar)
;; (package! synosaurus)
(package! async)
(package! elpy)
;; (package! nose) ;; Trying to fix nosetests warnings



;; (package! org-roam :disable t)

(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea"))

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; When using org-roam via the `+roam` flag
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam" :branch "master"))
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server" :branch "master"))
(package! company-org-roam
  :recipe (:host github :repo "org-roam/company-org-roam" :branch "master"))
(unpin! org-roam company-org-roam)
;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! org-roam-server)
(package! ess-view)
(package! ess-view-data)
;; (package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))

;; Use org-appear to reveal emphasis markers when moving the cursor over them.
(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))
;; (package! iedit)
(package! elfeed-goodies)
(package! flycheck-aspell)

(package! lsp-python-ms :disable t)
;; (package! org-noter-pdftools)
;; (package! org-pdftools)
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f25337..." :disable t)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! company-org-block)
(package! dashboard)

;; (package! netlogo-mode :recipe (:host github :repo "tgolsson/netlogo-mode" :files ("*.el" "emacs/")))
;; (package! company-netlogo :recipe (:host github :repo "tgolsson/company-netlogo" :files ("*.el")))

(package! org-transclusion
  :recipe (:host github
           :repo "nobiot/org-transclusion"
           :branch "main"
           :files ("*.el")))
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! ebib :recipe (:host github :repo "joostkremers/ebib" :files ("*.el")))
(package! ox-gemini)

