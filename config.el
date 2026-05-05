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
 org-directory "~/Org/"
 org-log-done 'time
 org-export-with-sub-superscripts '{}
 org-export-allow-bind-keywords t
 )

(setq
 org-cite-csl-styles-dir "~/Zotero/styles"
 )
(map! "C-c C-SPC" #'dabbrev-completion)

;; (setq doom-font (font-spec :family "Yanone Kaffeesatz" :size 30))
(setq  doom-font (font-spec :family "FiraCode Nerd Font" :size 25))
;; (setq  doom-font (font-spec :family "Roboto Mono" :size 25))
;; (setq doom-theme 'doom-material-dark)
;; (setq doom-theme 'doom-dracula)
(setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-challenger-deep)
;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq org-support-shift-select t)
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (setq org-indirect-buffer-display 'dedicated-frame)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"  ; Next actinable task
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "TODO(t)"
           "NEXT(n)"
           "WAIT(w)"  ; Something external is holding up this task
           "|"
           "DONE(D)"
           "KILL(k)")
          )) ;; GTD
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

(setq org-src-window-setup 'other-frame)

(custom-set-faces! '(window-divider :foreground "grey"))
;; (use-package! org-modern :after org)
;; (after! org (global-org-modern-mode))

(after! dired-rsync
  (setq dired-rsync-options "-azhuv --info=progress2")
  )


(after! centaur-tabs
(centaur-tabs-mode -1)
 (setq centaur-tabs-height 36
       centaur-tabs-set-icons t
       centaur-tabs-modified-marker "o"
       centaur-tabs-close-button "×"
       centaur-tabs-set-bar 'above
       centaur-tabs-gray-out-icons 'buffer)
 (centaur-tabs-group-by-projectile-project)
)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package! graphviz-dot-mode
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

;; (setq ispell-program-name (executable-find "aspell")
;;       ispell-dictionary "en_US")
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;; (setq flyspell-correct-popup t)
;; (after! langtool
;;   (setq langtool-disabled-rules '("WHITESPACE_RULE")
;;         langtool-language-tool-jar "/opt/LanguageTool-5.5/languagetool.jar"
;;         langtool-language-tool-server-jar "/opt/LanguageTool-5.5/languagetool-server.jar"
;;         )
;;   :hook ((text-mode       . flymake-languagetool-load)
;;          (latex-mode      . flymake-languagetool-load)
;;          (org-mode        . flymake-languagetool-load)
;;          (markdown-mode   . flymake-languagetool-load))
;;   )

;; (use-package! eglot-ltex
;;   :defer t
;;   :hook (text-mode . (lambda ()
;;                        (require 'eglot-ltex)
;;                        (eglot-ensure)))
;;   :init
;;   (setq eglot-ltex-server-path "/opt/ltex-ls-16.0.0/bin/ltex-ls"
;;         eglot-ltex-communication-channel 'stdio))         ; 'stdio or 'tcp

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

;; (after! flyspell
;;   (setq flyspell-lazy-idle-seconds 2))

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_US,pt_BR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,pt_BR")
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; (unless (file-exists-p ispell-personal-dictionary)
;;   (write-region "" nil ispell-personal-dictionary nil 0))

(setq! tree-sitter-load-path '( "/home/gpetrini/.config/emacs/tree-sitter/" "/home/gpetrini/.config/emacs/.local/straight/build-29.3/tree-sitter-langs/bin/" "/home/gpetrini/.tree-sitter/bin/")
       )

(after! python
  (set-eglot-client! '(python-mode python-ts-mode) '("ty" "server")))

(load! "dynare")

(after! magit
  ;; (magit-wip-mode)
  (setq magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit
        magit-inhibit-save-previous-winconf t
        magit-log-arguments '("--graph" "--decorate" "--color")
        magit-delete-by-moving-to-trash t
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
(after! cc-mode
  (setq c-basic-offset 4)
  ;; (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy" "--query-driver"))
  (set-eglot-client! '(c-mode c++-mode c++-ts-mode)
                     '("clangd"
                       "-j=8"
                       "--clang-tidy"
                       "--background-index"
                       "--completion-style=detailed"
                       "--suggest-missing-includes"
                       ;; "--compile-commands-dir=."
                       "--query-driver=/usr/bin/g++"))
  )

(setq flymake-cc-command
      '("g++"
        "-fsyntax-only"
        "-ffp-contract=off"
        "-fsanitize=address,undefined"
        "-Wall"
        "-Wextra"
        "-Wuninitialized"
        "-Wshadow"
        "-Wno-unused-parameter"
        "-Wno-unused-variable"
        "-Wno-unused-but-set-variable"
        "-fno-omit-frame-pointer"
        "-D_NW_"
        "-O0"
        "-ggdb3"
        "-Isrc"
        "-I."
        "-"))


(add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))

(after! apheleia-formatters
  (add-to-list 'apheleia-formatters '(ess-r "R" "-s" "--no-save" "--no-restore" "-e" "styler::style_text(readLines(file('stdin')))"))
  (add-to-list 'apheleia-mode-alist '(ess-r-mode . ess-r)))
(after! ess
  (setq ess-use-tracebug nil)
  (setq ess-set-style 'RStudio)
  )

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        c-mode
        cc-mode
        c++-mode
        latex-mode))

(after! org
  ;; (setq org-highlight-latex-and-related '(native script entities))
  (setq org-highlight-latex-and-related '(native entities))
  )


(setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape  -interaction=nonstopmode -output-directory=%o %f"))

;; (setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -pdf -%latex -shell-escape -output-directory=%o %f"))
;; (setq org-latex-pdf-process
;;       '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
;;         "LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))


(setq reftex-default-bibliography '("~/Org/zotero_refs.bib"))
(setq +latex-viewers '(pdf-tools evince))



;; This would favor yasnippet’s expansion and cursor movement over cdlatex’s expansion and movement, but that shouldn’t matter if you’re not using yasnippet in latex buffers.
(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

(setq org-latex-listings 'engraved
      org-latex-engraved-theme 'doom-one-light)

(setq org-latex-default-packages-alist
      '(
        ("utf8" "inputenc" t)

        ;; ("" "lmodern" nil)
        ("" "mathptmx" nil)
        ("T1" "fontenc" t)

        ("top=3cm, bottom=2cm, left=3cm, right=2cm" "geometry" nil)
        ("" "graphicx" t)
        ("" "longtable" t)
        ("" "float" nil)
        ("" "wrapfig" nil)    ;makes it possible to wrap text around figures
        ("" "rotating" nil)
        ("normalem" "ulem" t)

        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amssymb" t)
        ("" "amsmath" t)
        ;; ("theorems, skins" "tcolorbox" t)

        ;; Abnt related configuration
        ;; ("style=authoryear,extrayear,uniquename=init,giveninits,justify,repeattitles,doi=false,isbn=false,url=true,minnames=2,maxcitenames=2,natbib=true,backend=biber" "biblatex" t)
        ;; ("style=authoryear,uniquename=init,giveninits,doi=false,isbn=false,url=true,minnames=2,maxcitenames=2,natbib=true,backend=biber" "biblatex" t)
        ;; ("style=abnt,noslsn,extrayear,uniquename=init,giveninits,justify,sccite,
        ;; scbib,repeattitles,doi=false,isbn=false,url=false,maxcitenames=2,
        ;; natbib=true,backend=biber" "biblatex" t)

        ("" "url" nil)
        ;; this is used for syntax highlighting of code
        ;; ("cache=false" "minted" nil)
        ;; this allows you to use underscores in places like filenames. I still
        ;; wouldn't do it.
        ;; ("strings" "underscore" nil)
        ("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
         "hyperref" nil)

        ("" "attachfile" nil)

        ("" "xcolor" t)

        ("" "setspace" nil)

        ("" "tikz" t)

        ))

(require 'ox-latex)
;; This is for when you don't want any default packages, and you want
;; to declare them all yourself.
(add-to-list 'org-latex-classes
             '("tese"      ;class-name
               "\\documentclass{gpsabntex}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]" ;;header-string
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[presentation]{beamer}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


(add-to-list 'org-latex-classes
             '("memorial"
               "\\documentclass[12pt,a4paper,oneside]{memoir}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{ebgaramond}
\\usepackage[protrusion=true, expansion=true, final]{microtype}
\\usepackage[scaled]{beramono}
\\usepackage[scaled]{helvet}
\\usepackage[brazilian]{babel}
\\usepackage{url}
\\usepackage{ifthen}
\\usepackage{etaremune}
\\usepackage{csquotes}
\\usepackage{caption}
\\usepackage{float}
\\usepackage{xcolor}
\\usepackage{soul}
\\setlrmarginsandblock{2.5cm}{2.5cm}{*}
\\setulmarginsandblock{2.5cm}{2.5cm}{*}
\\checkandfixthelayout
\\setlength{\\parindent}{1.2em}
\\setlength{\\parskip}{0.5em}
\\linespread{1.12}
\\sethlcolor{yellow!20}
\\aliaspagestyle{chapter}{plain}
\\usepackage[colorlinks=true,linkcolor=black,urlcolor=blue,citecolor=blue,bookmarks=true,bookmarksopen=true,bookmarksnumbered=true]{hyperref}
\\usepackage{bookmark}
\\renewcommand*{\\clearforchapter}{}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(add-to-list 'org-latex-classes
             '("memorial-section"
               "\\documentclass[12pt,a4paper,oneside]{memoir}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{ebgaramond}
\\usepackage[protrusion=true, expansion=true, final]{microtype}
\\usepackage[scaled]{beramono}
\\usepackage[scaled]{helvet}
\\usepackage[brazilian]{babel}
\\usepackage{url}
\\usepackage{ifthen}
\\usepackage{etaremune}
\\usepackage{csquotes}
\\usepackage{caption}
\\usepackage{float}
\\usepackage{xcolor}
\\usepackage{soul}
\\setlrmarginsandblock{2.5cm}{2.5cm}{*}
\\setulmarginsandblock{2.5cm}{2.5cm}{*}
\\checkandfixthelayout
\\setlength{\\parindent}{1.2em}
\\setlength{\\parskip}{0.5em}
\\linespread{1.12}
\\sethlcolor{yellow!20}
\\aliaspagestyle{chapter}{plain}
\\usepackage[colorlinks=true,linkcolor=black,urlcolor=blue,citecolor=blue,bookmarks=true,bookmarksopen=true,bookmarksnumbered=true]{hyperref}
\\usepackage{bookmark}
\\renewcommand*{\\clearforchapter}{}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq pdf-annot-activate-created-annotations t
      pdf-view-display-size 'fit-width
      pdf-view-resize-factor 1.1)

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

(setq enable-remote-dir-locals t)

(use-package! denote
  :defer t
  :config
  (setq
   denote-save-buffers nil
   denote-sort-keywords t
   denote-prompts '(title keywords template)
   denote-date-prompt-use-org-read-date t
   )
  )

(use-package! denote-sequence
  :after denote
  )

(setq! denote-directory (expand-file-name "~/Org/"))


(use-package! citar-denote
  :defer t
  :init
  (citar-denote-mode)
  )
(setq! denote-templates
       '((biblio . "

- DOI/URL: %^{doi-url}
- Type: %^{=type=}
- Journal Title: %^{journaltitle}
- Abbrev: %^{shortjournal}

* FISH-5SS

** 5SS

** Background and motivation

** Supporting Ideas and hypothesis

** Purpose, Relevance, and Contribution

** Methodology

** Results

** Interesting findings and not categorized stuff

** Critics

** Abstract

#+BEGIN_ABSTRACT
%^{abstract}
#+END_ABSTRACT

* Specific notes
* Annotations (zotero)
* Additional Backlinks

* References")
         (plain . ""))
       citar-denote-template 'biblio)

(setq citar-denote-use-bib-keywords nil)
(setq citar-denote-open-attachment nil)

(use-package! denote-org
  :defer t
  :after org
  )

(setq org-agenda-time-grid
      '((daily today require-timed)
        (630 700 730 800 830 900 930 1000 1030 1100 1130
             1200 1230 1300 1330 1400 1430 1500 1530
             1600 1630 1700 1730 1800 1830 1900 1930 2000
             2030 2100 2130 2200 2230
             )
        "......" "----------------"))

(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@server" . ?S)


        ;; Institutions
        ("@hwr" . ?h)
        ("@santanna" . ?s)
        ("@unicamp" . ?u)
        ("@ysi" . ?y)
        ("@univesp" . ?U)
        ("@PED" . ?P)
        ("@MADE" . ?M)

        ;; Activities
        ("@bureaucracy" . ?b)
        ("@planning" . ?n)
        ("@coding" . ?c)
        ("@tests" . ?t)
        ("@reading" . ?r)
        ("@studing" . ?g)
        ("@concurso" . ?o)
        ("@writing" . ?w)
        ("@Review" . ?v)
        ("@dissertation" . ?d)
        ("@paper" . ?p)
        ("@email" . ?e)
        ("@meetings" . ?m)
        ("@personal" . ?l)
        ("@free" . ?f)
        ))

(setq org-agenda-files '("~/Dropbox/GTD/"))

;; This affects tasks blocked by subtasks
(after! org-agenda
  (set-face-attribute 'org-agenda-dimmed-todo-face nil
                      :foreground "#357ABD"
                      :weight 'normal
                      :slant 'italic))

(use-package! org-super-agenda
 :after org-agenda
 :init
 (setq org-agenda-include-deadlines t
       ;;       org-agenda-show-future-repeats t
       ;;       ;; org-agenda-repeating-timestamp-show-all t
       org-agenda-skip-scheduled-if-deadline-is-shown t
       ;;       org-agenda-compact-blocks t
       ;;       ;; org-agenda-show-all-dates t
       org-agenda-start-day nil
       org-agenda-skip-additional-timestamps-same-entry t
       org-agenda-span 'week
       ;; org-agenda-span 3
       ;;       org-deadline-warning-days 1
       org-agenda-start-on-weekday nil)
 :config
 (org-super-agenda-mode)
 )


(setq org-agenda-custom-commands
     '(
       ("o" "Super view"
        (

         (alltodo "" ((org-agenda-overriding-header "")
                     (org-super-agenda-groups
                      '(
                        (:name "Overdue"
                         :and (:deadline past :todo ("TODO" "NEXT" "WAIT"))
                         :and (:scheduled past :todo ("TODO" "NEXT" "WAIT")))
                        (:discard (:anything t)
                                  )))))
        (agenda "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '(
                       (
                        :name "Already Done"
                        :todo "DONE"
                        :log t
                        :order 2
                        )
                       (:name "Due Later"
                        :and (:deadline future :todo ("TODO" "NEXT" "WAIT"))
                        :order 11)
                       (:name "Today"
                        :time-grid t
                        :scheduled today
                        :deadline today
                        :order 1)
                       ))))
        ;; FIXME Add untaggeds
        (alltodo "" ((org-agenda-overriding-header "All TODOs")
                     (org-super-agenda-groups
                      '(
                        (:name "Scheduled Soon"
                         :scheduled future
                         :order 1)
                        (:name "Important"
                         :discard (:tag "Archived")
                         :and (:priority "A" :deadline t)
                         :order 2)
                        (:name "Meetings"
                         :category "Meetings"
                         :discard (:not (:todo ("TODO" "WAITING" "NEXT" )))
                         :order 3)
                        (:name "Concurso"
                         :category "Concurso"
                         :order 3
                         )
                        (:name "MADE"
                         :category "MADE"
                         :order 3
                         )
                        (:name "Reading list"
                         :category "Reading list"
                         :order 4
                         )
                        (:name "Bureaucracy"
                         :category "Bureaucracy"
                         :order 5
                         )
                        (:name "PhD"
                         :category "PhD"
                         :order 5
                         )
                        (:name "Conferences and Workshops"
                         :category "Conferences"
                         :order 5)
                        (:name "Active papers"
                         :category "Paper-related"
                         :property ("ORG_GTD"  "Project")
                         )
                        (:name "Ticklers"
                         :property ("ORG_GTD"  "Tickler")
                         :order 5)
                        (:name "PED"
                         :category "PED"
                         :order 6
                         )
                        (:name "Paper reviews"
                         :category "Paper reviews"
                         :order 8)
                        (:name "YSI Related"
                         :category "YSI"
                         :order 8
                         )
                        (:name "Habits"
                         :property ("ORG_GTD"  "Habit")
                         :order 12)
                        (:name "Home/Chores"
                         :category "Home/Chores"
                         :order 12)
                        (:name "Computer related"
                         :category "Computer-related"
                         :order 13)
                        (:name "Emacs related"
                         :category "Emacs-related"
                         :order 13)
                        (:name "Archive"
                         :tag "Archived"
                         :order 100
                         )
                        (:name "Someday"
                         :property ("ORG_GTD"  "Someday")
                         :order 100)
                        )
                      )
                     )
                 )
        )
       )
      )
    )
(map! :desc "Next line"
     :map org-super-agenda-header-map
     "j" 'org-agenda-next-line)

(map! :desc "Next line"
     :map org-super-agenda-header-map
     "k" 'org-agenda-previous-line)

;; Custom styles for dates in agenda
(custom-set-faces!
  '(org-agenda-date :inherit outline-1 :height 1.15)
  '(org-agenda-date-today :inherit outline-2 :height 1.15)
  '(org-agenda-date-weekend :inherit outline-1 :height 1.15)
  ;; '(org-agenda-date-weekend-today :inherit outline-2 :height 1.15)
  '(org-super-agenda-header :inherit custom-button :weight bold :height 1.05)
  `(link :foreground "orange" :underline nil :background ,(nth 1 (nth 7 doom-themes--colors))))



(setq org-agenda-prefix-format '(
                                 (agenda . " %?-2i %t ")
                                 (todo . "[%e]  %i %-12:c ")
                                 (alltodo . "[%e]  %i %-12:c [%e] ")
                                 (tags . "[%e]  %i %-12:c")
                                 (search . "[%e]  %i %-12:c")))
(setq org-agenda-category-icon-alist
      `(("Teaching" ,(list (nerd-icons-faicon "nf-fa-graduation_cap" :height 0.8)) nil nil :ascent center)
        ("Home" ,(list (nerd-icons-faicon "nf-fa-home" :v-adjust 0.005)) nil nil :ascent center)
        ("inbox" ,(list (nerd-icons-faicon "nf-fa-edit" :height 0.9)) nil nil :ascent center)
        ("PhD" ,(list (nerd-icons-faicon "nf-fa-pen" :height 0.9)) nil nil :ascent center)
        ("paper" ,(list (nerd-icons-faicon "nf-fa-newspaper" :height 0.9)) nil nil :ascent center)
        ("MADE" ,(list (nerd-icons-faicon "nf-fa-dollar" :height 0.9)) nil nil :ascent center)
        ("YSI" ,(list (nerd-icons-faicon "nf-fa-dollar" :height 0.9)) nil nil :ascent center)
        ("package" ,(list (nerd-icons-faicon "nf-fa-box" :height 0.9)) nil nil :ascent center)
        ("Events" ,(list (nerd-icons-faicon "nf-fa-plane_departure" :height 0.9)) nil nil :ascent center)
        ("Computer-related" ,(list (nerd-icons-faicon "nf-fa-computer_mouse" :height 0.9)) nil nil :ascent center)
        ("Github" ,(list (nerd-icons-faicon "nf-fa-github" :height 0.9)) nil nil :ascent center)
        ("Emacs" ,(list (nerd-icons-faicon "nf-fa-computer_mouse" :height 0.9)) nil nil :ascent center)
        ("Docs" ,(list (nerd-icons-faicon "nf-fa-wpforms" :height 0.9)) nil nil :ascent center)
        ("Meeting" ,(list (nerd-icons-faicon "nf-fa-message" :height 0.9)) nil nil :ascent center)
        ("group" ,(list (nerd-icons-faicon "nf-fa-group" :height 0.9)) nil nil :ascent center)
        ("Review" ,(list (nerd-icons-faicon "nf-fa-book_open_reader" :height 0.9)) nil nil :ascent center)
        ("Concurso" ,(list (nerd-icons-faicon "nf-fa-university" :height 0.9)) nil nil :ascent center)
        ("Bureaucracy" ,(list (nerd-icons-faicon "nf-fa-building" :height 0.9)) nil nil :ascent center)
        ("Email" ,(list (nerd-icons-faicon "nf-fa-mail_reply" :height 0.9)) nil nil :ascent center)
        ("Literature update" ,(list (nerd-icons-faicon "nf-fa-rss" :height 0.9)) nil nil :ascent center)
        ("Supervisions" ,(list (nerd-icons-faicon "nf-fa-route" :height 0.9)) nil nil :ascent center)
        ))

(use-package! org-timeblock
  :after org-agenda
  )

(setq
 org-global-properties (quote ((
                                "Effort_ALL" .
                                "0:05 0:10 0:20 0:30 0:45 1:00 1:30 2:00 2:30 3:00 4:00 5:00 6:00 7:00 8:00")))
 )

(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

;; (setq shell-file-name (executable-find "bash"))
;; (setq-default vterm-shell "/usr/bin/fish")
;; (setq-default explicit-shell-file-name "/usr/bin/fish")
;; (setq-default explicit-shell-file-name shell-file-name
;;               shell-file-name (executable-find "bash"))
(setq shell-file-name "/usr/bin/bash")
(setq-default explicit-shell-file-name "/usr/bin/bash")
(setq-default vterm-shell "/usr/bin/bash")

(use-package! flymake-vale
  :defer t
  :config
  (add-hook 'latex-mode-hook #'flymake-vale-load)
  (add-hook 'org-mode-hook #'flymake-vale-load)
  )

(use-package! flycheck-grammarly
  :defer-incrementally flycheck)
(after! flycheck-grammarly
 (setq flycheck-grammarly-check-time 1.0)
 (flycheck-add-mode 'grammarly 'LaTeX-mode)
 (flycheck-grammarly-setup)
(grammarly-load-from-authinfo "gpetrinidasilveira@gmail.com"))


;; (use-package! eglot-grammarly
;;   :defer t  ; defer package loading
;;   :hook ((text-mode org-mode). (lambda ()
;;                                       (require 'eglot-grammarly)
;;                                       (eglot-ensure))))

(setq rmh-elfeed-org-files '("~/Dropbox/Elfeed.org"))

(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed

  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@4-month-ago +unread"
        ;; elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        ;; elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)
  )

(use-package! org-alert
  :after org
  :config
  (setq alert-default-style 'libnotify
        org-alert-interval 300
        alert-fade-time 15
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 30
        org-alert-time-match-string  "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")
  (org-alert-enable)
  )

(use-package! tempel
  :defer t
  :bind (("M-[" . tempel-complete) ;; Alternative tempel-expand
         ("M-]" . tempel-insert))
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-abbrev-mode)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (global-tempel-abbrev-mode))

(use-package! tempel-collection
  :after tempel)

(use-package! eglot-tempel
    :after eglot)

(use-package! org-gtd
  :after org
  :init
  ;; Suppress upgrade warnings (must be set before package loads)
  (setq org-gtd-update-ack "4.0.7")
  (setq org-gtd-directory "~/Dropbox/GTD/")
  :custom
  (org-gtd-keyword-mapping '((todo . "TODO")
                             (next . "NEXT")
                             (wait . "WAIT")
                             (done . "DONE")
                             (canceled . "KILL")))
  ;; Enable per-type refile prompting (recommended)
  ;; Without this, all items auto-refile to first target without prompting
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-gtd-refile-to-any-target nil)
  :config
  (org-edna-mode)
  ;; Doom-style leader key bindings
  ;; Clarify map binding
  (map! :localleader
        (:prefix ("D" . "org-gtd")
         :desc "Capture"        "c"  #'org-gtd-capture
         ;; :desc "Engage"         "e"  #'org-gtd-engage
         :desc "Command center"  "a"  #'org-gtd-command-center
         :desc "Process inbox"  "p"  #'org-gtd-process-inbox
         :desc "Show all next"  "n"  #'org-gtd-show-all-next
         :desc "Clarify item"   "k"  #'org-gtd-clarify-item
         :desc "Stuck projects" "s"  #'org-gtd-reflect-stuck-projects
         :desc "Set area of focus" "f"  #'org-gtd-area-of-focus-set-on-item-at-point))
  (map! :map org-gtd-clarify-mode-map
        :desc "Organize this item" "C-c c" #'org-gtd-organize)
  ;; Quick task actions in agenda view
  (map! :map org-agenda-mode-map
        :desc "GTD quick actions" "C-c ." #'org-gtd-agenda-transient)
  )


(setq org-gtd-areas-of-focus
      '(
        "Teaching"
        "Home/Chores"
        "Health"
        "Learning"
        "Reading list"
        "Literature update"
        "Planning"
        "Appointments"
        "PhD"
        "Paper-related"
        "MADE"
        "Package"
        "Conferences"
        "Events"
        "Computer-related"
        "Github"
        "Emacs-related"
        "Meetings"
        "Groups"
        "YSI"
        "Paper reviews"
        "Concurso"
        "Bureaucracy"
        "E-mail"
        "Uncategorized"
        "Supervisions"
        ))

(setq org-gtd-organize-hooks '(org-gtd-set-area-of-focus
                               org-set-tags-command
                               ))

(defun my-gtd-add-effort ()
  "Prompt for effort estimation when organizing single actions and projects."
  (when (org-gtd-organize-type-member-p '(single-action project-heading calendar delegated habit))
    (call-interactively #'org-set-effort)))

(add-to-list 'org-gtd-organize-hooks 'my-gtd-add-effort)

(defun my-gtd-add-priority ()
  "Prompt for priority when organizing single actions."
  (when (org-gtd-organize-type-member-p '(single-action project-heading calendar delegated habit))
    (call-interactively #'org-priority)))

(add-to-list 'org-gtd-organize-hooks 'my-gtd-add-priority)

(setq org-gtd-graph-render-mode 'ascii)

(defun my-gtd-review ()
  "Show planning views"
  (interactive)
  (org-gtd-view-show
   '(

     ((name . "󰃹 Overdue")
      (type . next-action)
      (scheduled . past)
      )

     ((name . "󰏺 Missed events")
      (type . calendar)
      (when . past)
      )

     ((name . "󰃹 Missed check-in")
      (type . tickler)
      (when . past)
      )

     ((name . "󰃭 Due today")
      (type . calendar)
      (when . today))

     ((name . "󰕪 Today's schedule")
      (block-type . calendar-day))

     ((name . "󰃶 Scheduled for today")
      (type . next-action)
      (scheduled . today)
      )

     ((name . "󰢌 Tickler items ready for today")
      (type . tickler)
      (when . today))

     ((name . " Delegation check-ins")
      (type . delegated)
      (when . today))


     ((name . " High priority focused Work for today")
      (type . next-action)
      (priority . A)
      (scheduled . today)
      (effort . (> "0:30")))

     ((name . "󰒭 All actions ready to be executed")
      (type . next-action)
      )

     ((name . "󱙬 Next time-dependent events")
      (type . calendar)
      (when . future))



     ((name . " Low priority")
      (type . next-action)
      (priority . (B C))
      (effort . (> "0:30")))

     ((name . " Easy picks")
      (effort . (between "0:05" "0:15"))
      (type . next-action)
      )


     ((name . " Completed projects")
      (type . completed-project)
      )

     ((name . " Stuck projects")
      (type . stuck-project)
      )

     ((name . " Tickler projects")
      (type . incubated-project)
      )

     ((name . "󱉓 MADE")
      (area-of-focus . "MADE")
      (type . next-action)
      )


     ((name . " Supervisions")
      (area-of-focus . "Supervisions")
      (type . next-action)
      )

     ((name . " Home/Chores")
      (area-of-focus . "Home/Chores")
      (type . next-action)
      )

     ((name . "󱐮 Health")
      (area-of-focus . "Health")
      (type . next-action)
      )

     ((name . " Reading list")
      (area-of-focus . "Reading list")
      (type . next-action)
      )

     ((name . " Literature update")
      (area-of-focus . "Literature update")
      (type . next-action)
      )

     ((name . " Planning")
      (area-of-focus . " Planning")
      (type . next-action)
      )

     ((name . " Appointments")
      (area-of-focus . "Appointments")
      (type . next-action)
      )

     ((name . " Paper-related")
      (area-of-focus . "Paper-related")
      (type . next-action)
      )

     ((name . " Package")
      (area-of-focus . "Package")
      (type . next-action)
      )

     ((name . " Conferences")
      (area-of-focus . "Conferences")
      (type . next-action)
      )

     ((name . "󱁖 Events")
      (area-of-focus . "Events")
      (type . next-action)
      )

     ((name . " Computer-related")
      (area-of-focus . "Computer-related")
      (type . next-action)
      )

     ((name . " Github")
      (area-of-focus . "Github")
      (type . next-action)
      )

     ((name . " Emacs-related")
      (area-of-focus . "Emacs-related")
      (type . next-action)
      )

     ((name . " Meetings")
      (area-of-focus . "Meetings")
      (type . calendar)
      (when . future)
      )

     ((name . " Groups")
      (area-of-focus . "Groups")
      (type . next-action)
      )

     ((name . " YSI")
      (area-of-focus . "YSI")
      (type . next-action)
      )

     ((name . " Paper reviews")
      (area-of-focus . "Paper reviews")
      (type . next-action)
      )

     ((name . " Concurso")
      (area-of-focus . "Concurso")
      (type . next-action)
      )

     ((name . " Bureaucracy")
      (area-of-focus . "Bureaucracy")
      (type . next-action)
      )

     ((name . " E-mail")
      (area-of-focus . "E-mail")
      (type . next-action)
      )

     ((name . " Unestimated")
      (type . next-action)
      (effort . nil)
      )

     ((name . " PhD")
      (area-of-focus . "PhD")
      (type . next-action)
      )

     ((name . " Teaching")
      (area-of-focus . "Teaching")
      (type . next-action)
      )

     ((name . " Learning")
      (area-of-focus . "Learning")
      (type . next-action)
      )

     ((name . " Paper Ideas")
      (type . someday)
      (area-of-focus . "Paper-related")
      )

     ((name . "󱫢 When idle")
      (type . next-action)
      (tags . ("@free"))
      )


     )
   )
  )

(map! :localleader
      (:prefix "D"
       :desc "My custom Engage view"
       "e" #'my-gtd-review))


(setq org-gtd-capture-templates
      `(
        ("i" "Inbox"
         entry  (file ,#'org-gtd-inbox-path)
         "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:ORG_GTD_CAPTURED_AT: %U\n:END:\n  %i"
         :kill-buffer t)
        ("l" "Inbox with link"
         entry  (file ,#'org-gtd-inbox-path)
         "* %? [[%^{Link}][%^{Name}]]\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:ORG_GTD_CAPTURED_AT: %U\n:END:\n  %i"
         :kill-buffer t)
        ("a" "Add to reading list"
         entry  (file ,#'org-gtd-inbox-path)
         "* Read [[%^{Link}][%^{Name}]]\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:ORG_GTD_CAPTURED_AT: %U\n:DOI: %^{DOI}\n:Published: %^{Published?}\n:END:
          \n- Reason: %^{Reason}
          \n- Possible group: %^{Group}"
         :kill-buffer t)
        )
      )


;; (defun my/org-capture-add-ids-to-subtree ()
;;   "Add UUID IDs to all headings in the captured subtree."
;;   (when (and (eq major-mode 'org-mode)
;;              (org-capture-get :key))
;;     (save-excursion
;;       (org-back-to-heading t)
;;       (org-map-entries
;;        (lambda ()
;;          (org-id-get-create))
;;        nil
;;        'tree))))

;; (add-hook 'org-capture-after-finalize-hook
;;           #'my/org-capture-add-ids-to-subtree)

(setq auth-sources '(password-store "~/.authinfo.gpg"))

(with-eval-after-load 'hl-todo
  (dolist (kw
           '(("REFACTOR"    font-lock-keyword-face bold)
             ("CLEANUP"     font-lock-doc-face bold)
             ("OPTIMIZE"    warning bold)

             ("ASSUMPTION"  font-lock-type-face bold)
             ("CALIBRATION" font-lock-type-face bold)
             ("VALIDATION"  success bold)
             ("CONSISTENCY" success bold)
             ("EDGECASE"    warning bold)

             ("HELP"        success bold)
             ("QUESTION"    warning bold)
             ("EXAMPLE"     font-lock-doc-face bold)

             ("REVERT"      error bold)
             ("BREAKING"    error bold)

             ("DELEGATE"    font-lock-constant-face bold)
             ("BLOCKED"     error bold)
             ("PRIORITY"    warning bold)

             ("DEBUG"       font-lock-constant-face bold)
             ("OUTPUT"      font-lock-type-face bold)
             ("LOGIC"       font-lock-keyword-face bold)

             ("FUTURE"      warning bold)
             ("LAST-REVIEW" font-lock-doc-face bold)
             ("REVIEW-DATE" font-lock-doc-face bold)))

    ;; adiciona apenas se ainda não existir
    (unless (assoc (car kw) hl-todo-keyword-faces)
      (add-to-list 'hl-todo-keyword-faces kw t))))
