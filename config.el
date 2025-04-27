(setq user-full-name "Gabriel Petrini"
      user-mail-address "gpetrinidasilveira@gmail.com")

(setq evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t               ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t  ; When there are lots of glyphs, keep them in memory
                                        ;                                               ; undo-limit 80000000                          ; Raise undo-limit to 80Mb
      truncate-string-ellipsis "…")     ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)               ; Replace selection when inserting text
(display-time-mode 1)                   ; Enable time in the mode-line
(global-subword-mode 1)                 ; Iterate through CamelCase words
(setq
    storage-directory "/HDD/"           ;
    org-directory "~/Org/"
    org-agenda-files '(expand-file-name "agenda.org" org-directory)
    org-notes-directory org-directory
    gtd-directory (expand-file-name "gtd/" org-directory)
    inbox-file-path (expand-file-name "inbox.org" gtd-directory)
    notes-directory org-directory
    pdfs-directory  (expand-file-name "PDFs/" storage-directory)
    refs-files '((expand-file-name "zotero_refs.bib" org-directory))
    main-ref-file (expand-file-name "zotero_refs.bib" org-directory)
    org-roam-v2-ack t
 )

(setq
 org-cite-csl-styles-dir "~/Zotero/styles"
      )
(map! "C-c C-SPC" #'dabbrev-completion)

;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "biber %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
;; Pretty code blocs from https://tecosaur.github.io/emacs-config/config.html#pretty-code-blocks
;; (use-package! engrave-faces-latex
;;   :after ox-latex)

;; Now let’s have the example block be styled similarly.
;; (setq org-latex-listings 'engraved)
;; (defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
;;   "Like `org-latex-example-block', but supporting an engraved backend"
;;   :around #'org-latex-example-block
;;   (let ((output-block (funcall orig-fn example-block contents info)))
;;     (if (eq 'engraved (plist-get info :latex-listings))
;;         (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
;;       output-block)))

;; (setq doom-font (font-spec :family "Yanone Kaffeesatz" :size 30))
(setq  doom-font (font-spec :family "FiraCode Nerd Font" :size 25))
;; (setq  doom-font (font-spec :family "Roboto Mono" :size 20))
;; (setq doom-theme 'doom-material-dark)
(setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-challenger-deep)
;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq org-support-shift-select t)
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

  )
;; (after! org
;;   (setq org-highlight-latex-and-related nil)
;;   )

(setq
 delete-by-moving-to-trash t)
(setq evil-normal-state-cursor '(box "orange")
      evil-insert-state-cursor '(bar "orange")
      evil-visual-state-cursor '(hollow "orange"))
(setq org-export-headline-levels 5) ; I like nesting

(set-face-foreground 'vertical-border (doom-color 'red))
(setq window-divider-default-bottom-width 4  ; default is 1
      window-divider-default-right-width 4)  ; default is 1

(custom-set-faces! '(window-divider :foreground "grey"))
;; (use-package! org-modern :after org)
;; (with-eval-after-load 'org (global-org-modern-mode))

(after! dired-rsync
  (setq dired-rsync-options "-azhuv --info=progress2")
  )
;; Fix tabs in the daemon
;; (after! centaur-tabs
;;   (setq centaur-tabs-set-bar 'right)
;;   (centaur-tabs-group-by-projectile-project)
;;   )
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package! graphviz-dot-mode
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;; (add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "en"))
;; (setq ispell-program-name (executable-find "aspell")
;;       ispell-dictionary "en")
(setq flyspell-correct-popup t)
(after! langtool
  (setq langtool-disabled-rules '("WHITESPACE_RULE")
        langtool-language-tool-jar "/opt/LanguageTool-5.5/languagetool.jar"
        langtool-language-tool-server-jar "/opt/LanguageTool-5.5/languagetool-server.jar"
        )
  )

(use-package! jinx
  :defer t
  :init
  (add-hook 'doom-init-ui-hook #'global-jinx-mode)
  :config
  ;; Use my custom dictionary
  (setq jinx-languages "en_US")
  ;; Extra face(s) to ignore
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
  ;; I prefer for `point' to end up at the start of the word,
  ;; not just after the end.
  (advice-add 'jinx-next :after (lambda (_) (left-word))))

;; (use-package! eglot-grammarly
;;   :defer t  ; defer package loading
;;   :hook ((text-mode org-mode). (lambda ()
;;                                       (require 'eglot-grammarly)
;;                                       (eglot-ensure))))

(setq! tree-sitter-load-path '( "/home/gpetrini/.config/emacs/tree-sitter/" "/home/gpetrini/.config/emacs/.local/straight/build-29.3/tree-sitter-langs/bin/" "/home/gpetrini/.tree-sitter/bin/")
       )

(load! "~/.config/doom/dynare.el")

(after! magit
  ;; (magit-wip-mode)
  (setq magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit
        magit-inhibit-save-previous-winconf t
        magit-log-arguments '("--graph" "--decorate" "--color")
        ;; magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 120
        magit-diff-refine-hunk 'all
        ))
(setq auth-sources '("~/.authinfo"))

(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))

(setq! julia-snail-executable "~/.juliaup/bin/julia")
(setq! eglot-jl-julia-command "/home/gpetrini/.juliaup/bin/julia")

;; (setq-hook! 'c-mode-hook +format-inhibit t)
;; (setq-hook! 'c++-mode-hook +format-inhibit t)

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        c-mode
        cc-mode
        c++-mode
        latex-mode))

(load! "scimax-org-latex.el")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(setq org-latex-prefer-user-labels t)

(setq pdf-annot-activate-created-annotations t
      pdf-view-display-size 'fit-width
      pdf-view-resize-factor 1.1)

(after! org-roam
  (setq org-roam-db-location "~/Org/notes/org-roam.db")
)

(setq org-attach-use-inheritance nil)
(require 'org-id)
(setq org-id-track-globally t)
(setq org-roam-completion-everywhere t)

(setq bibtex-completion-bibliography main-ref-file)
(setq bibtex-completion-library-path pdfs-directory
      bibtex-completion-pdf-field "File"
      bibtex-completion-notes-path "~/Org" ;; FIXME generalize
      )

(setq!  citar-org-roam-subdir nil)
(setq! org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n
#+HUGO_AUTO_SET_LASTMOD: t
#+hugo_base_dir: ~/BrainDump/\n
#+hugo_section: notes\n
#+HUGO_TAGS: placeholder\n
#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
#+OPTIONS: num:nil ^:{} toc:nil\n
\n")
         :unnarrowed t)
      ("k" "Knowledge base" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n
#+HUGO_AUTO_SET_LASTMOD: t
#+hugo_base_dir: ~/BrainDump/\n
#+hugo_section: notes\n
#+HUGO_CATEGORIES: KnowledgeBase\n
#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
#+OPTIONS: num:nil ^:{} toc:nil\n
\n")
         :unnarrowed t)
      ("l" "Lecture notes" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n
#+HUGO_AUTO_SET_LASTMOD: t
#+hugo_base_dir: ~/BrainDump/\n
#+hugo_section: notes\n
#+HUGO_CATEGORIES: Lectures\n
#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
#+OPTIONS: num:nil ^:{} toc:nil\n
\n")
         :unnarrowed t)
      ("o" "Off office notes" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n
#+HUGO_AUTO_SET_LASTMOD: t
#+hugo_base_dir: ~/BrainDump/\n
#+hugo_section: offoffice\n
#+HUGO_CATEGORIES: OffOffice\n
#+OPTIONS: num:nil ^:{} toc:nil\n
\n")
         :unnarrowed t)
;;         ("b" "Bibliographic note" plain
;;          ""
;;          :if-new (file+head "%<%Y-%m-%d>_${citekey}.org"
;;                             ":PROPERTIES:
;; :ID: %<%Y%m%dT%H%M%S>
;; :CAPTURED: [%<%Y-%m-%d %H:%M:%S>]
;; :END:
;; ,#+TITLE: ${citekey}: ${title} - (%^{year}, %^{journal})
;; Time-stamp: %<%Y-%m-%d>
;; ,#+hugo_base_dir: ~/BrainDump/\n
;; ,#+hugo_section: notes\n
;; ,#+hugo_categories: %^journal
;; ,#+HUGO_TAGS: %^{keywords}\n
;; ,#+OPTIONS: num:nil ^:{} toc:nil
;; ,#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
;; ,#+cite_export: csl apa.csl


;; \n* FISH-5SS

;; |---------------------------------------------+-----|
;; | <40>                                        |<50> |
;; | *Background*                                  |     |
;; | *Supporting Ideas*                            |     |
;; | *Purpose*                                     |     |
;; | *Originality/value (Contribution)*            |     |
;; | *Relevance*                                   |     |
;; | *Design/methodology/approach*                 |     |
;; | *Results*                                     |     |
;; | *(Interesting) Findings*                      |     |
;; | *Research limitations/implications (Critics)* |     |
;; | *Uncategorized stuff*                         |     |
;; | *5SS*                                         |     |
;; |---------------------------------------------+-----|

;; \n** Abstract

;; ,#+BEGIN_ABSTRACT
;; ${abstract}
;; ,#+END_ABSTRACT

;; \n* Specific notes\n\n
;; \n* Annotations (zotero)\n\n
;; \n* Additional Backlinks\n
;; \n* References\n

;; ,#+print_bibliography:"
;;                             )
;;          :immediate-finish t
;;          :unnarrowed t
;;          :type org-roam-bibtex
;;          :jump-to-captured t )
        )
      )

(defun my-orb-latex-note-to-org (citekey)
  (let* ((entry (bibtex-completion-get-entry citekey))
         (note (bibtex-completion-get-value "note" entry ""))
         (pandoc-command "pandoc --from latex --to org")
         result)
    (with-temp-buffer
      (shell-command (format "echo \"%s\" | %s" note pandoc-command)
                     (current-buffer))
      (setq result (buffer-substring-no-properties (point-min) (point-max))))))

(setq! org-cite-csl-styles-dir "~/Zotero/styles")

(setq! ;; FIXME Generalize
 citar-bibliography '("~/Org/zotero_refs.bib")
 citar-library-paths '("/HDD/PDFs/")
 citar-notes-paths '("~/Org")
 )

(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (preview . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (note . "
#+OPTIONS: num:nil ^:{} toc:nil
#+TITLE: ${author}: (${date year}, ${shortjournal})
#+SUBTITLE: ${author editor}: ${title} - (${date year issued:4}, ${journal shortjournal})
#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
#+SIGNATURE: ~/Org/zotero_refs.bib
#+cite_export: csl apa.csl


\n* FISH-5SS

\n** 5SS

\n** Background and motivation

\n** Supporting Ideas and hypothesis

\n** Purpose, Relevance, and Contribution

\n** Methodology

\n** Results

\n** Interesting findings and not categorized stuff

\n** Critics

\n** Abstract

#+BEGIN_ABSTRACT
${abstract}
#+END_ABSTRACT

\n* Specific notes
\n* Annotations (zotero)
\n* Additional Backlinks
\n* References"

              )))

(use-package! org-glossary
  :hook (org-mode . org-glossary-mode))

(setq enable-remote-dir-locals t)

(use-package! denote
  :defer t
  :config
  (setq
   denote-save-buffers nil
   denote-sort-keywords t
   denote-file-type nil
   denote-prompts '(title keywords)
   denote-rename-confirmations '(rewrite-front-matter modify-file-name)
   denote-date-prompt-use-org-read-date t
   denote-backlinks-show-context t
   )
  )

(setq! denote-directory (expand-file-name "~/Org/"))
(setq! denote-templates
       '((biblio . "
\n* FISH-5SS

\n** 5SS

\n** Background and motivation

\n** Supporting Ideas and hypothesis

\n** Purpose, Relevance, and Contribution

\n** Methodology

\n** Results

\n** Interesting findings and not categorized stuff

\n** Critics

\n** Abstract

#+BEGIN_ABSTRACT
%^{abstract}
#+END_ABSTRACT

\n* Specific notes
\n* Annotations (zotero)
\n* Additional Backlinks


\n* References"
          )
         (plain . nil)
         )
       citar-denote-template 'biblio
       )

(use-package! citar-denote
  :defer t
  :init
  (citar-denote-mode)
  )

(use-package! denote-org
  :defer t
  :after org
  )

(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)
        ("@server" . ?S)


        ;; Institutions
        ("@hwr" . ?h)
        ("@santanna" . ?s)
        ("@unicamp" . ?u)
        ("@ysi" . ?y)

        ;; Activities
        ("@bureaucracy" . ?b)
        ("@planning" . ?n)
        ("@coding" . ?c)
        ("@tests" . ?t)
        ("@reading" . ?r)
        ("@writing" . ?w)
        ("@review" . ?v)
        ("@dissertation" . ?d)
        ("@paper" . ?p)
        ("@email" . ?e)
        ("@meetings" . ?m)
        ("@personal" . ?l)
        ("@free" . ?f)
        ))
(setq org-agenda-files '("~/Org/Personal.org"
                         "~/Org/YSI.org"
                         "~/PhD/Writings/AB-SFC-SSM-Dot-Com/AB-SFC-SSM-Dot-Com.org"
                         "~/Documents/KS_vs_DSGE/KS_vs_DSGE.org"
                         "~/LSD/Work/PhD/mkks_irf/mkks_irf.org"
                         ;; "~/Documents/KS-DA-Calibration/KS-DA-Calibration.org"
                         ))

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(setq shell-file-name (executable-find
      "bash"))
(setq-default vterm-shell
      "/usr/bin/fish")
(setq-default explicit-shell-file-name
      "/usr/bin/fish")
