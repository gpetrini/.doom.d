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
        ("@server" . ?S)


        ;; Institutions
        ("@hwr" . ?h)
        ("@santanna" . ?s)
        ("@unicamp" . ?u)
        ("@ysi" . ?y)
        ("@univesp" . ?U)
        ("@PED" . ?P)

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
        ("@DotCom" . ?D)
        ))
(setq org-agenda-files '(
                         "~/PhD/Writings/AB-SFC-SSM-Dot-Com/AB-SFC-SSM-Dot-Com.org"
                         "~/Documents/KS_vs_DSGE/KS_vs_DSGE.org"
                         "~/LSD/Work/PhD/mkks_irf/mkks_irf.org"
                         "~/Documents/KS-DA-Calibration/KS-DA-Calibration.org"
                         "~/Documents/Deflating_WIOD_Tables/TODOs.org"
                         "~/Documents/LSDCompare/README.org"
                         "~/Dropbox/GTD/YSI.org"
                         "~/Dropbox/GTD/Agenda.org"
                         "~/Dropbox/GTD/Inbox.org"
                         ))


(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-include-deadlines t
        ;;       org-agenda-show-future-repeats t
        ;;       ;; org-agenda-repeating-timestamp-show-all t
        ;;       org-agenda-skip-scheduled-if-deadline-is-shown nil
        ;;       org-agenda-compact-blocks t
        ;;       ;; org-agenda-show-all-dates t
        org-agenda-start-day nil
        org-agenda-span 'week
        ;;       ;; org-agenda-span 1
        ;;       org-deadline-warning-days 1
        org-agenda-start-on-weekday nil)
  :config
  (org-super-agenda-mode)
  )


(setq org-agenda-custom-commands
      '(
        ("o" "Super view"
         ((agenda "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (
                          :name "Already Done"
                          :todo "DONE"
                          :log t
                          :order 2
                          )
                         (:name "Overdue"
                          :and (:deadline past :todo ("TODO" "NEXT" "WAITING"))
                          :and (:scheduled past :todo ("TODO" "NEXT" "WAITING"))
                          :order 3)
                         (:name "Due Later"
                          :and (:deadline future :todo ("TODO" "NEXT" "WAITING"))
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
                           :order 10)
                          (:name "Inbox"
                           :tag "inbox"
                           :order 1)
                          (:name "Important"
                           :discard (:tag "Archived")
                           :and (:priority "A" :deadline t)
                           :order 2)
                          (:name "Meetings"
                           :tag "@meetings"
                           :discard (:not (:todo ("TODO" "WAITING" "NEXT" )))
                           :order 3)
                          (:name "Conferences and Workshops"
                           :tag "@Events"
                           :order 3)
                          (:name "Agent-Based Model chapter"
                           :tag "@DotCom"
                           :order 4
                           )
                          (:name "DSGE Comparison paper"
                           :tag "@DSGEComp"
                           :order 5
                           )
                          (:name "LSD Comparison package"
                           :tag "@LSDComp"
                           :order 5
                           )
                          (:name "PED"
                           :tag "@PED"
                           :order 6
                           )
                          (:name "Input Output Growth Decomposition"
                           :tag "IODefl"
                           :order 7
                           )
                          (:name "YSI Related"
                           :tag "@YSI"
                           :order 8
                           )
                          (:name "Univesp"
                           :tag "@univesp"
                           :order 9)
                          (:name "GEMAP"
                           :and (:tag "@Gemap" :tag "@Groups")
                           :order 11)
                          (:name "Chores"
                           :tag "chores"
                           :order 12)
                          (:name "Computer related"
                           :tag "@computer"
                           :order 13)
                          (:name "Archive"
                           :tag "Archived"
                           :order 100
                           )
                          (:name "Unimportant"
                           :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                           :order 100)
                          (:name "Waiting..."
                           :todo "WAITING"
                           :order 98)
                          )
                        )
                       )
                   )
          )
         )
        )
      )
(use-package! origami
  :after org-agenda
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
  '(org-agenda-date-weekend-today :inherit outline-2 :height 1.15)
  '(org-super-agenda-header :inherit custom-button :weight bold :height 1.05)
  `(link :foreground unspecified :underline nil :background ,(nth 1 (nth 7 doom-themes--colors)))
  '(org-link :foreground unspecified))

(setq org-agenda-prefix-format '(
                                 (agenda . "  %?-2i %t ")
                                 (todo . " %i %-12:c")
                                 (alltodo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-agenda-category-icon-alist
      `(("Teaching" ,(list (nerd-icons-faicon "nf-fa-graduation_cap" :height 0.8)) nil nil :ascent center)
        ("Home" ,(list (nerd-icons-faicon "nf-fa-home" :v-adjust 0.005)) nil nil :ascent center)
        ("inbox" ,(list (nerd-icons-faicon "nf-fa-edit" :height 0.9)) nil nil :ascent center)
        ("PhD" ,(list (nerd-icons-faicon "nf-fa-pen" :height 0.9)) nil nil :ascent center)
        ("paper" ,(list (nerd-icons-faicon "nf-fa-newspaper" :height 0.9)) nil nil :ascent center)
        ("Univesp" ,(list (nerd-icons-faicon "nf-fa-dollar" :height 0.9)) nil nil :ascent center)
        ("package" ,(list (nerd-icons-faicon "nf-fa-box" :height 0.9)) nil nil :ascent center)
        ("Events" ,(list (nerd-icons-faicon "nf-fa-plane_departure" :height 0.9)) nil nil :ascent center)
        ("Computer" ,(list (nerd-icons-faicon "nf-fa-computer_mouse" :height 0.9)) nil nil :ascent center)
        ("Docs" ,(list (nerd-icons-faicon "nf-fa-wpforms" :height 0.9)) nil nil :ascent center)
        ("Meeting" ,(list (nerd-icons-faicon "nf-fa-message" :height 0.9)) nil nil :ascent center)
        ("group" ,(list (nerd-icons-faicon "nf-fa-group" :height 0.9)) nil nil :ascent center)
))

(use-package! org-timeblock
  :after org-agenda
  )

(use-package! org-analyzer
  :after org-agenda
    )
(setq org-analyzer-wrapper-command "org-analyzer")
(setq org-analyzer-jar-file-name "/opt/org-analyzer.jar")
(setq org-analyzer-java-program "/opt/org-analyzer") ;; Is not actually java, buta  wrapper shell script

(defun org-analyzer-start-process (org-dir)
  "Start the org analyzer process .
Argument ORG-DIR is where the org-files are located."
  (org-analyzer-cleanup-process-state)
  (unless (file-exists-p org-dir)
    (warn "org-analyzer was started with org-directory set to
  \"%s\"\nbut this directory does not exist.
Please set the variable `org-directory' to the location where you keep your org files."
           org-directory))
    (let* ((name (format " *org-analyzer [org-dir:%s]*" org-dir))
           (proc-buffer (generate-new-buffer name))
           (proc nil))
      (setq org-analyzer-process-buffer proc-buffer)
      (with-current-buffer proc-buffer
        (setq default-directory (if (file-exists-p org-dir)
                                    org-dir default-directory)
              proc (condition-case err
                       (let ((process-connection-type nil)
                             (process-environment process-environment))
                         (start-process name
                                        (current-buffer)
                                        org-analyzer-wrapper-command
                                        "--port"
                                        (format "%d" org-analyzer-http-port)
                                        "--started-from-emacs"
                      (if (file-exists-p org-dir) org-dir "")))
                     (error
                      (concat "Can't start org-analyzer (%s: %s)"
                (car err) (cadr err)))))
        (set-process-query-on-exit-flag proc nil)
        (set-process-filter proc #'org-analyzer-process-filter))
      proc-buffer))

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

(setq shell-file-name (executable-find
      "bash"))
(setq-default vterm-shell
      "/usr/bin/fish")
(setq-default explicit-shell-file-name
      "/usr/bin/fish")
