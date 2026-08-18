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

(package! org-pretty-tags)
;; (package! peep-dired)
;; (package! rainbow-mode)
(package! academic-phrases)
(package! async)





;; (package! websocket)
;; (package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff" :files ("*.el")))
(package! flymake-languagetool :recipe (:host github :repo "emacs-languagetool/flymake-languagetool"))

;; (package! citar)


(unpin! ox-pandoc) ;; Changing pandoc repo
(package! ox-pandoc :recipe (:host github :repo "emacsorphanage/ox-pandoc"))
(package! tldr)
;; (package! litex-mode :recipe (:host github :repo "Atreyagaurav/litex-mode"))
(package! org-glossary
  :recipe (:host github :repo "tecosaur/org-glossary"))
;; (package! org-pandoc-import
;;   :recipe (:host github
;;            :repo "tecosaur/org-pandoc-import"
;;            :files ("*.el" "filters" "preprocessors")))
;; (package! org-auto-tangle)


(package! unpackaged
  :recipe (:host github :repo "alphapapa/unpackaged.el"))
(package! org-remark)


;; (package! oxr
;;   :recipe (:host github :repo "bdarcus/oxr"))
;; (package! citar-org-roam :disable t) ;; Breaks org-roam template.
(package! engrave-faces :recipe (:repo "tecosaur/engrave-faces"))

(package! jinx)
;; (package! gptel)
;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

(package! matlab-mode)

(package! denote :recipe (:host github :repo "protesilaos/denote"))
(package! denote-org :recipe (:host github :repo "protesilaos/denote-org"))
(package! denote-sequence :recipe (:host github :repo "protesilaos/denote-sequence"))
(package! consult-denote :recipe (:host github :repo "protesilaos/consult-denote"))
(package! denote-menu :recipe (:host github :repo "namilus/denote-menu"))
(package! citar-denote :recipe (:host github :repo "pprevos/citar-denote"))
(package! denote-explore  :recipe (:host github :repo "pprevos/denote-explore"))
(package! org-transclusion)

(package! olivetti
  :recipe (:host github :repo "rnkn/olivetti"))


(package! nov)

;; (package! eglot-grammarly
;;   :recipe (:host github :repo "emacs-grammarly/eglot-grammarly"))
(package! org-super-agenda)
(package! org-ql)
(package! org-timeblock  :recipe (:host github :repo "ichernyshovvv/org-timeblock"))

(package! flymake-vale  :recipe (:host github :repo "tpeacock19/flymake-vale"))
;; (package! flycheck-grammarly)


(package! eglot-ltex  :recipe (:host github :repo "emacs-languagetool/eglot-ltex"))
(package! realgud)
(package! org-gcal)
(package! org-alert)
;; (package! exec-path-from-shell)

(package! calfw-blocks  :recipe (:host github :repo "ml729/calfw-blocks"))


(package! eglot-tempel :recipe (:host github :repo "fejfighter/eglot-tempel"))
(package! tempel)
(package! tempel-collection)

(package! org-gtd)
(package! djvu)

;; (package! org-repeat-by-cron)

;; (unpin! org-noter)

(package! org-noter :recipe (:host github :repo "org-noter/org-noter" :files ("*.el" "modules/*.el")))
