(setq user-full-name "Gabriel Petrini"
      user-mail-address "gpetrinidasilveira@gmail.com")

;; (setq package-native-compile t)
(setq-default
    delete-by-moving-to-trash t         ; Delete files to trash
    tab-width 4                         ; Set width for tabs
    uniquify-buffer-name-style 'forward ; Uniquify buffer names
    window-combination-resize t         ; take new window space from all other windows (not just current)
    x-stretch-cursor t
    load-prefer-newer t                 ; Native-comp related
 )                                      ; Stretch cursor to the glyph width

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
    org-roam-directory (expand-file-name "notes/" org-directory)
    org-agenda-files '(expand-file-name "agenda.org" org-directory)
    org-notes-directory org-roam-directory
    gtd-directory (expand-file-name "gtd/" org-directory)
    inbox-file-path (expand-file-name "inbox.org" gtd-directory)
    notes-directory (expand-file-name "notes" org-directory)
    pdfs-directory  (expand-file-name "PDFs/" storage-directory)
    refs-files '((expand-file-name "zotero_refs.bib" org-directory))
    main-ref-file (expand-file-name "zotero_refs.bib" org-directory)
    org-roam-v2-ack t
 )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "biber %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

;; (setq doom-font (font-spec :family "Yanone Kaffeesatz" :size 30))
;; (setq  doom-font (font-spec :family "Fira Mono" :size 20))
;; (setq  doom-font (font-spec :family "Roboto Mono" :size 20))
;; (setq doom-theme 'doom-material)
(setq doom-theme 'doom-dracula)
;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)
(setq org-support-shift-select t)
(after! org
  (setq org-image-actual-width '(300))
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  )
(setq evil-normal-state-cursor '(box "orange")
      evil-insert-state-cursor '(bar "orange")
      evil-visual-state-cursor '(hollow "orange"))
(setq org-export-headline-levels 5) ; I like nesting

(set-face-foreground 'vertical-border (doom-color 'red))
(setq window-divider-default-bottom-width 4  ; default is 1
      window-divider-default-right-width 4)  ; default is 1

(custom-set-faces! '(window-divider :foreground "grey"))

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-ellipsis " ▼ "
        org-log-done 'time
        org-enable-roam-support t
        org-src-window-setup 'other-window ;; FIXME
        org-startup-folded 'overview
        org-hide-emphasis-markers t))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(add-hook! org-mode :append #'org-appear-mode)


(setq
 org-cite-csl-styles-dir "~/Zotero/styles"
 ;; org-cite-global-bibliography '(main-ref-file)
      )

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "output replace")
        (:exports . "results")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        ))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))


(appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "◼"
            :checkedbox    "☑"
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         "𝙏"
            :subtitle      "𝙩"
            :author        "𝘼"
            :date          "𝘿"
            :property      "☸"
            :options       "⌥"
            :latex_class   "🄲"
            :latex_header  "⇥"
            :beamer_header "↠"
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       "🠶"
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "⚙"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
(set-ligatures! 'org-mode
  :merge t
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_latex:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]")
(plist-put +ligatures-extra-symbols :name "⁍")

(defvar org-latex-extra-special-string-regexps
  '(("->" . "\\\\textrightarrow{}")
    ("<-" . "\\\\textleftarrow{}")))

(defun org-latex-convert-extra-special-strings (string)
  "Convert special characters in STRING to LaTeX."
  (dolist (a org-latex-extra-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defadvice! org-latex-plain-text-extra-special-a (orig-fn text info)
  "Make `org-latex-plain-text' handle some extra special strings."
  :around #'org-latex-plain-text
  (let ((output (funcall orig-fn text info)))
    (when (plist-get info :with-special-strings)
      (setq output (org-latex-convert-extra-special-strings output)))
    output))

(use-package! org-web-tools
  :defer t
)

(use-package! org-super-agenda
  :commands org-super-agenda-mode)
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-todo-ignore-deadlines t
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
            ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '(
                           (:auto-group t)
                           (:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)
                          (:name "Due Today"
                           :scheduled t
                           :deadline today
                           :todo "TODO"
                           :order 2)
                          (:name "Due Soon"
                           :scheduled future
                           :todo "TODO"
                           :order 2)
                          (:name "Overdue"
                           :deadline past
                           :todo "TODO"
                           :face error
                           :order 1)
                           )))
                     )
                (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                           (:auto-group t)
                          (:name "Lectures"
                           :tag ("@Teaching" "@Lectures")
                           :order 8)
                          (:name "Advisoring"
                           :tag "@Orientations"
                           :order 8)
                          (:name "Meetings"
                           :tag "@Meeting"
                           :order 5)
                          (:name "Current Research"
                           :tag "@Article"
                           :order 6)
                          (:name "To read"
                           :file-path "readings"
                           :order 8
                           )
                          (:name "To writing"
                           :todo ("STRT")
                           :order 4)
                          (:name "Waiting"
                           :todo ("WAITING" "WAIT" "MAYBE")
                           :order 20)
                          (:name "Research groups"
                           :tag ("@Group")
                           :order 10)
                          (:name "University"
                           :tag ("@UNICAMP")
                           :order 10)
                          (:name "Emacs"
                           :tag ("@Emacs")
                           :order 80)
                          (:name "Trivial"
                           :tag ("@free")
                           :order 90)
                          ))))))))

(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))

(use-package! graphviz-dot-mode
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(use-package! elfeed-org
  :defer t
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "~/Org/rss/elfeed.org"))))

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

(defun gps/elfeed-load-db-and-open ()
  "Load the elfeed db from disk before updating."
  (interactive)
  (elfeed)
  (elfeed-goodies/setup)
  (elfeed-db-load)
  (elfeed-search-update--force)
  (elfeed-update))

;;write to disk when quiting
(defun gps/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun gps/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "en_US"))
(setq ispell-program-name (executable-find "aspell")
      ispell-dictionary "en_US")
(setq flyspell-correct-popup t)
(setq langtool-language-tool-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool.jar")
(setq langtool-language-tool-server-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool-server.jar")

;; (use-package! lsp-grammarly
;;   :defer t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp))))  ; or lsp-deferred

;; (use-package! flycheck-languagetool
;;   :defer t
;;   :hook (text-mode . (lambda ()
;;                        (require 'flycheck-languagetool)))
;;   :init
;;   (setq flycheck-languagetool-server-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool-server.jar"))

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/usr/bin/zsh")
  (exec-path-from-shell-initialize))

(use-package! dap-mode
  :defer t
  :config
  (require 'dap-node)
  )

(setq dap-auto-configure-features '(sessions locals controls tooltip))

;; (when (functionp 'module-load)
;; associated jupyter-stata with stata (fixes fontification if using pygmentize for html export)
;;   (add-to-list 'org-src-lang-modes '("jupyter-stata" . stata))
;;   (add-to-list 'org-src-lang-modes '("Jupyter-Stata" . stata))
;; you **may** need this for latex output syntax highlighting
;; (add-to-list 'org-latex-minted-langs '(stata "stata"))
(setq inferior-STA-program-name "/usr/local/bin/jupyter-console")

(after! ess
  (setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-below-selected)
         (side . left)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil)))
      )
  (setq ess-style 'RStudio
      ;; auto-width
      ess-auto-width 'window
      ;; let lsp manage lintr
      ess-use-flymake nil
      ;; Stop R repl eval from blocking emacs.
      ess-eval-visibly 'nowait
      ess-use-eldoc nil
      ess-use-company nil
      )

  (setq ess-r--no-company-meta t)

  (setq ess-ask-for-ess-directory nil
      ;; ess-startup-directory t
      ess-local-process-name "R"
      ansi-color-for-comint-mode 'filter
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-move-point-for-output t)

;; ESS buffers should not be cleaned up automatically
  (add-hook 'inferior-ess-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Open ESS R window to the left iso bottom.
  ;; (set-popup-rule! "^\\*R.*\\*$" :side 'left :size 0.38 :select nil :ttl nil :quit nil :modeline t)
)

;; ===========================================================
;; IDE Functions
;; ===========================================================

;; Bring up empty R script and R console for quick calculations
(defun ess-tide-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))

(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:constants . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op% . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))

;; Fix Warning "readline" message
;; (require 'dap-python)
(set-popup-rule! "^\\*Python*"  :side 'bottom :size .30) ;; Python console to the bottom ;; FIXME

;; Disable native completion

(after! python

  (setq python-shell-completion-native-enable nil)
  (set-company-backend! 'python-mode 'elpy-company-backend)
  (setq python-shell-interpreter "/usr/bin/python3"
        org-babel-python-command "/usr/bin/python3")
  )
(after! elpy
  (set-company-backend! 'elpy-mode
    '(elpy-company-backend :with company-files company-yasnippet)))

;; (add-hook 'python-mode-hook 'eglot-ensure)

(after! python
  (set-company-backend! 'python-mode 'elpy-company-backend))
(after! company
  (setq company-idle-delay 0
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-show-numbers t
        company-minimum-prefix-length 3)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(set-company-backend! 'org-mode nil)
(use-package! company-tabnine
  :defer t
  )
(after! company
  (add-to-list 'company-backends 'company-tabnine))

;; In case we get a wrong workspace root, we can delete it with lsp-workspace-folders-remove
(after! lsp-mode
  (setq lsp-auto-guess-root nil))
(set-popup-rule! "^\\*lsp-help" :side 'right :size .50 :select t :vslot 1)

;; Disable lsp flycheck checker and use flake8
(after! lsp-mode
  (setq lsp-diagnostic-package :none))

(after! flycheck
  (add-hook 'pyhon-mode-local-vars-hook
            (lambda ()
              (when (flycheck-may-enable-checker 'python-flake8)
                (flycheck-select-checker 'python-flake8)))))
;; (setq flycheck-disabled-checkers 'lsp)

(after! lsp-mode
  (setq lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;; lsp-enable-on-type-formatting nil
        ;; lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil))

(after! magit
  ;; (magit-wip-mode)
  (setq magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit
        magit-inhibit-save-previous-winconf t
        magit-log-arguments '("--graph" "--decorate" "--color")
        ;; magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 120))

(use-package! lsp-bridge
  :defer t
  )

(use-package! wolfram-mode
  :defer t
  :config
  (setq mathematica-command-line "wolframscript -script")
  ;; (add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
  )
(after! 'org
            (org-babel-do-load-languages 'org-babel-load-languages
                                         (append org-babel-load-languages
                                                 '((mathematica . t))
                                                 ))
            )
(load! "./misc/ob-mathematica.el")
(add-to-list 'org-src-lang-modes '("mathematica" . wolfram))

(load! "scimax-org-latex.el")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(setq org-latex-prefer-user-labels t)
(setq org-latex-caption-above nil)

(use-package! cdlatex
    :after (:any org-mode LaTeX-mode)
    :hook
    ((LaTeX-mode . turn-on-cdlatex)
     (org-mode . turn-on-org-cdlatex)))

(use-package! company-math
    :after (:any org-mode TeX-mode)
    :config
    (set-company-backend! 'org-mode 'company-math-symbols-latex)
    (set-company-backend! 'TeX-mode 'company-math-symbols-latex)
    (set-company-backend! 'org-mode 'company-latex-commands)
    (set-company-backend! 'TeX-mode 'company-latex-commands)
    (setq company-tooltip-align-annotations t)
    (setq company-math-allow-latex-symbols-in-faces t))

(add-to-list
 'org-latex-classes
 '(("tufte-book"
    "\\documentclass[a4paper, sfsidenotes, openany, justified]{tufte-book}"
    ("\\part{%s}" . "\\part*{%s}")
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("utf8" . "utf8x")
    ("\\subsection{%s}" . "\\subsection*{%s}"))))

(setq reftex-default-bibliography refs-files)

(setq +latex-viewers '(evince pdf-tools okular))

(setq org-highlight-latex-and-related '(native script entities))

(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(setq pdf-annot-activate-created-annotations t
      pdf-view-display-size 'fit-width
      pdf-view-resize-factor 1.1)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path (list org-directory)
   org-noter-notes-window-location 'horizontal-split
   bibtex-completion-pdf-field "file"
   )
  ;; (require 'org-noter-pdftools)
  )

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
      bibtex-completion-notes-path "~/Org/notes" ;; FIXME generalize
      )


(setq org-roam-capture-templates
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
 citar-notes-paths '("~/Org/notes/")
 )

(use-package! citeproc-el :defer t)

(after! oc
  (defun org-ref-to-org-cite ()
    "Attempt to convert org-ref citations to org-cite syntax."
    (interactive)
    (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                               ("nocite" . "/n")
                               ("citep" . "") ("citep*" . "//f")
                               ("textcite" . "/t")
                               ("parencite" . "") ("Parencite" . "//c")
                               ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                               ("citeyear" . "/na/b")
                               ("Citep" . "//c") ("Citealp" . "//bc")
                               ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                               ("autocite" . "") ("Autocite" . "//c")
                               ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                               ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
           (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                            ":" (group (+ (not (any "\n     ,.)]}")))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cite-regexp nil t)
          (message (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2)))
          (replace-match (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2))))))))

(setq citar-file-open-note-function 'orb-bibtex-actions-edit-note)
(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (preview . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (note . "
#+OPTIONS: num:nil ^:{} toc:nil
#+TITLE: ${author editor}: ${title} - (${date year issued:4}, ${journal shortjournal})
#+hugo_base_dir: ~/BrainDump/
#+hugo_section: notes
#+hugo_categories: ${journal shortjournal}
#+FILETAGS: ${keywords}
#+BIBLIOGRAPHY: ~/Org/zotero_refs.bib
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
\n* References")))

(setq citar-symbols
      `((file ,(all-the-icons-octicon "file-pdf" :face 'all-the-icons-red :v-adjust -0.1) . " ")
        (note ,(all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-blue :v-adjust 0.01) . " ")))
(setq citar-symbol-separator "  ")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :hook (org-roam . org-roam-ui-mode)
    :config
)

(setq org-capture-templates '(
                              ("t" "Todo [inbox]" entry
                               (file+headline inbox-file-path "Tasks inbox")
                               "* TODO %i%?")
                              ("w" "Writing inbox" entry
                               (file+headline inbox-file-path "Writing inbox")
                               "* TODO %i%?")
                              ("f" "Fleeting notes" entry
                               (file+headline inbox-file-path "Fleeting notes")
                               "* WAIT %i%?")
                              ("r" "Readings inbox" entry
                               (file+headline inbox-file-path "Reading Inbox")
                               "* %(org-cliplink-capture) %^g" :prepend t)
                              ("n" "News inbox" entry
                               (file+headline inbox-file-path "News Inbox")
                               "* %t %(org-cliplink-capture) %^g" :prepend t)
                              ))

(setq org-refile-targets '((expand-file-name "projects.org"  gtd-directory :maxlevel . 3)
                           (expand-file-name "reading.org" gtd-directory   :maxlevel . 4)
                           (expand-file-name "someday.org" gtd-directory :level . 1)))

(setq org-roam-dailies-directory "~/Org/journal/")
  (setq org-roam-dailies-capture-templates
           '(("D" "Daily Report" plain (file "~/Org/journal/Template.org")
            :if-new  (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
            )
           ))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(setq +zen-text-scale 0.5)



(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defvar-local +zen--original-solaire-mode-p nil)
  (defvar-local +zen--original-org-pretty-table-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-solaire-mode-p solaire-mode)
            (solaire-mode -1)
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1))
        (when +zen--original-solaire-mode-p (solaire-mode 1)))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-superstar-headline-bullets-list
            'org-superstar-remove-leading-stars)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-prose-org-h ()
              "Reformat the current Org buffer appearance for prose."
              (when (eq major-mode 'org-mode)
                (setq display-line-numbers nil
                      visual-fill-column-width 60
                      org-adapt-indentation nil)
                (when (featurep 'org-superstar)
                  (setq-local org-superstar-headline-bullets-list '("🙘" "🙙" "🙚" "🙛")
                              ;; org-superstar-headline-bullets-list '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                              org-superstar-remove-leading-stars t)
                  (org-superstar-restart))
                (setq
                 +zen--original-org-indent-mode-p org-indent-mode
                 +zen--original-org-pretty-table-mode-p (bound-and-true-p org-pretty-table-mode))
                (org-indent-mode -1)
                (org-pretty-table-mode 1))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-nonprose-org-h ()
              "Reverse the effect of `+zen-prose-org'."
              (when (eq major-mode 'org-mode)
                (when (featurep 'org-superstar)
                  (org-superstar-restart))
                (when +zen--original-org-indent-mode-p (org-indent-mode 1))
                ;; (unless +zen--original-org-pretty-table-mode-p (org-pretty-table-mode -1))
                ))))

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(use-package! orgdiff :defer t)

(setq org-hugo-base-dir "~/BrainDump/")

(use-package! org-transclusion :defer t)

(defhydra thesis-nav (:hint nil :color blue)
"
Main Files:  _T_hesis _m_anuscript _i_deas _s_etup _o_pen PDF file externaly
  Chapters:  _0_ Introduction _1_ Ch^1 _2_ Ch^2 _3_ Ch^3 _C_ Conclusion _A_ Appendix
   Actions:  _c_ Async compile file _t_angle _r_un
"
      ("T" (find-file "~/PhD/Writings/thesis.org"))
      ("m" (find-file "~/PhD/Writings/manuscript.org"))
      ("i" (find-file "~/Org/notes/phd_dissertation_discussions.org"))
      ("s" (find-file "~/PhD/Writings/thesis.setup"))
      ("o" (shell-command "open ~/PhD/Writings/thesis.pdf"))
      ("0" (find-file "~/PhD/Writings/Introduction/Introduction.org"))
      ("1" (find-file "~/PhD/Writings/Super_ABM/Research_Paper.org"))
      ("2" (find-file "~/PhD/Writings/Househing_ABM/Research_Paper.org"))
      ("3" (find-file "~/PhD/Writings/Spatial_Housing_ABM/Research_Paper.org"))
      ("C" (find-file "~/PhD/Writings/Conclusion/Conclusion.org"))
      ("A" (find-file "~/PhD/Writings/Appendix/Appendix.org"))
      ("c" (org-latex-export-to-pdf :async t))
      ("t" (org-babel-tangle))
      ("r" (org-babel-execute-buffer))
      ;; ("b" ()) ;; for biber
      ;; ("l" ()) ;; for latex
      ;; ("N" (shell-command "") :exit t) ;; For LSD no window
      ;; ("p" (shell-command "./purpure/purpurea.sh") :exit t) ;; For purpurea
      ("q" nil)
  )
(map! :leader
      :desc "Thesis Navegation"
      "H t" #'thesis-nav/body)

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))


(map! :leader
      :desc "Dired Navegation"
      "H d" #'hydra-dired/body)

(defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
     :post (quit-windows-on "*Flycheck errors*")
     :hint nil)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "Next")
  ("k" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))


(map! :leader
      :desc "Flyckeck"
      "H f" #'hydra-flycheck/body)

(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))


(map! :leader
      :desc "Macro creating"
      "H m" #'hydra-macro/body)

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))


(map! :leader
      :desc "git gutter"
      "H g" #'hydra-git-gutter/body)

(use-package! smerge-mode
  :defer t
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package! litex-mode
  :commands litex-mode
  :hook text-mode)
(setq litex-keep-sexp-in-buffer t)

(defun my/rdrview-get (url callback)
  "Get the rdrview representation of URL.

Call CALLBACK with the output."
  (let* ((buffer (generate-new-buffer "rdrview"))
         (proc (start-process "rdrview" buffer "rdrview"
                              url "-T" "title,sitename,body"
                              "-H")))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (progn
                  (funcall callback
                           (with-current-buffer (process-buffer process)
                             (buffer-string)))
                  (kill-buffer (process-buffer process))) )
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (kill-buffer (process-buffer process))
                  (user-error "Error in rdrview: %s" err)))))))
    proc))

(defun my/rdrview-parse (dom-string)
  (let ((dom (with-temp-buffer
               (insert dom-string)
               (libxml-parse-html-region (point-min) (point-max)))))
    (let (title sitename content (i 0))
      (dolist (child (dom-children (car (dom-by-id dom "readability-page-1"))))
        (when (listp child)
          (cond
           ((eq (car child) 'h1)
            (setq title (dom-text child)))
           ((eq (car child) 'h2)
            (setq sitename (dom-text child)))
           ((eq (car child) 'div)
            (setq content child)))))
      (while (and
              (not (dom-by-tag content 'h1))
              (dom-search
               content
               (lambda (el)
                 (when (listp el)
                   (pcase (car el)
                     ('h2 (setf (car el) 'h1))
                     ('h3 (setf (car el) 'h2))
                     ('h4 (setf (car el) 'h3))
                     ('h5 (setf (car el) 'h4))
                     ('h6 (setf (car el) 'h5))))))))
      `((title . ,title)
        (sitename . ,sitename)
        (content . ,(with-temp-buffer
                      (dom-print content)
                      (buffer-string)))))))

(defvar-local my/elfeed-show-rdrview-html nil)

(defun my/rdrview-elfeed-show ()
  (interactive)
  (unless elfeed-show-entry
    (user-error "No elfeed entry in this buffer!"))
  (my/rdrview-get
   (elfeed-entry-link elfeed-show-entry)
   (lambda (result)
     (let* ((data (my/rdrview-parse result))
            (inhibit-read-only t)
            (title (elfeed-entry-title elfeed-show-entry))
            (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
            (authors (elfeed-meta elfeed-show-entry :authors))
            (link (elfeed-entry-link elfeed-show-entry))
            (tags (elfeed-entry-tags elfeed-show-entry))
            (tagsstr (mapconcat #'symbol-name tags ", "))
            (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
            (content (alist-get 'content data))
            (feed (elfeed-entry-feed elfeed-show-entry))
            (feed-title (elfeed-feed-title feed))
            (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
       (erase-buffer)
       (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                       (propertize title 'face 'message-header-subject)))
       (when elfeed-show-entry-author
         (dolist (author authors)
           (let ((formatted (elfeed--show-format-author author)))
             (insert
              (format (propertize "Author: %s\n" 'face 'message-header-name)
                      (propertize formatted 'face 'message-header-to))))))
       (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                       (propertize nicedate 'face 'message-header-other)))
       (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
                       (propertize feed-title 'face 'message-header-other)))
       (when tags
         (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                         (propertize tagsstr 'face 'message-header-other))))
       (insert (propertize "Link: " 'face 'message-header-name))
       (elfeed-insert-link link link)
       (insert "\n")
       (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
                do (insert (propertize "Enclosure: " 'face 'message-header-name))
                do (elfeed-insert-link (car enclosure))
                do (insert "\n"))
       (insert "\n")
       (if content
           (elfeed-insert-html content base)
         (insert (propertize "(empty)\n" 'face 'italic)))
       (setq-local my/elfeed-show-rdrview-html content)
       (goto-char (point-min))))))

(setq my/rdrview-template (expand-file-name "~/.doom.d/misc/latex_template_elfeed.tex"))

(cl-defun my/rdrview-render (content type variables callback
                                     &key file-name overwrite)
  "Render CONTENT with pandoc.

TYPE is a file extension as supported by pandoc, for instance,
html or txt.  VARIABLES is an alist that is fed into the
template.  After the rendering is complete successfully, CALLBACK
is called with the resulting PDF.

FILE-NAME is a path to the resulting PDF. If nil it's generated
randomly.

If a file with the given FILE-NAME already exists, the function will
invoke CALLBACK straight away without doing the rendering, unless
OVERWRITE is non-nil."
  (unless file-name
    (setq file-name (format "/tmp/%d.pdf" (random 100000000))))
  (let (params
        (temp-file-name (format "/tmp/%d.%s" (random 100000000) type)))
    (cl-loop for (key . value) in variables
             when value
             do (progn
                  (push "--variable" params)
                  (push (format "%s=%s" key value) params)))
    (setq params (nreverse params))
    (if (and (file-exists-p file-name) (not overwrite))
        (funcall callback file-name)
      (with-temp-file temp-file-name
        (insert content))
      (let ((proc (apply #'start-process
                         "pandoc" (get-buffer-create "*Pandoc*") "pandoc"
                         temp-file-name "-o" file-name
                         "--pdf-engine=xelatex" "--template" my/rdrview-template
                         params)))
        (set-process-sentinel
         proc
         (lambda (process _msg)
           (let ((status (process-status process))
                 (code (process-exit-status process)))
             (cond ((and (eq status 'exit) (= code 0))
                    (progn
                      (message "Done!")
                      (funcall callback file-name)))
                   ((or (and (eq status 'exit) (> code 0))
                        (eq status 'signal))
                    (user-error "Error in pandoc. Check the *Pandoc* buffer")))))))))
)

(setq my/elfeed-pdf-dir (expand-file-name "~/.elfeed/pdf/"))

(defun my/elfeed-open-pdf (entry overwrite)
  "Open the current elfeed ENTRY with a pdf viewer.

If OVERWRITE is non-nil, do the rendering even if the resulting
PDF already exists."
  (interactive (list elfeed-show-entry current-prefix-arg))
  (let ((authors (mapcar (lambda (m) (plist-get m :name)) (elfeed-meta entry :authors)))
        (feed-title (elfeed-feed-title (elfeed-entry-feed entry)))
        (tags (mapconcat #'symbol-name (elfeed-entry-tags entry) ", "))
        (date (format-time-string "%a, %e %b %Y"
                                  (seconds-to-time (elfeed-entry-date entry))))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (file-name (concat my/elfeed-pdf-dir
                           (elfeed-ref-id (elfeed-entry-content entry))
                           ".pdf"))
        (main-language "english")
        (other-language "portuguese"))
    (unless content
      (user-error "No content!"))
    (setq subtitle
          (cond
           ((seq-empty-p authors) feed-title)
           ((and (not (seq-empty-p (car authors)))
                 (string-match-p (regexp-quote (car authors)) feed-title)) feed-title)
           (t (concat (string-join authors ", ") "\\\\" feed-title))))
    (when (member 'ru (elfeed-entry-tags entry))
      (setq main-language "portuguese")
      (setq other-language "english"))
    (my/rdrview-render
     (if (bound-and-true-p my/elfeed-show-rdrview-html)
         my/elfeed-show-rdrview-html
       content)
     (elfeed-entry-content-type entry)
     `((title . ,(elfeed-entry-title entry))
       (subtitle . ,subtitle)
       (date . ,date)
       (tags . ,tags)
       (main-lang . ,main-language)
       (other-lang . ,other-language))
     (lambda (file-name)
       (start-process "xdg-open" nil "xdg-open" file-name))
     :file-name file-name
     :overwrite current-prefix-arg)))

(defun my/get-languages (url)
  (let ((main-lang "english")
        (other-lang "portuguese"))
    (when (string-match-p (rx ".br") url)
      (setq main-lang "portuguese"
            other-lang "english"))
    (list main-lang other-lang)))

(defun my/rdrview-open (url overwrite)
  (interactive
   (let ((url (read-from-minibuffer
               "URL: "
               (if (bound-and-true-p elfeed-show-entry)
                   (elfeed-entry-link elfeed-show-entry)))))
     (when (string-empty-p url)
       (user-error "URL is empty"))
     (list url current-prefix-arg)))
  (my/rdrview-get
   url
   (lambda (res)
     (let ((data (my/rdrview-parse res))
           (langs (my/get-languages url)))
       (my/rdrview-render
        (alist-get 'content data)
        'html
        `((title . ,(alist-get 'title data))
          (subtitle . ,(alist-get 'sitename data))
          (main-lang . ,(nth 0 langs))
          (other-lang . ,(nth 1 langs)))
        (lambda (file-name)
          (start-process "xdg-open" nil "xdg-open" file-name)))))))

(use-package! nov)
(use-package! nov-xwidget
  :defer t
  :after nov
  :config
  (map! :map xwidget-webkit-mode-map
        :n "]" 'nov-xwidget-next-document
        :n "[" 'nov-xwidget-previous-document
        :n "gt" 'nov-xwidget-goto-toc)
  (map! :map nov-mode-map
        :n "gv" 'nov-xwidget-view))

(use-package! org-pandoc-import :after org)
