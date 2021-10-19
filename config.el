(setq user-full-name "Gabriel Petrini"
      user-mail-address "gpetrinidasilveira@gmail.com")

;; (setq package-native-compile t)
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                                         ; Set width for tabs
 uniquify-buffer-name-style 'forward      ; Uniquify buffer names
 window-combination-resize t                    ; take new window space from all other windows (not just current)
 x-stretch-cursor t
 load-prefer-newer t                             ;; Native-comp related
 )                                           ; Stretch cursor to the glyph width

(setq evil-want-fine-undo t                             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                                    ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t      ; When there are lots of glyphs, keep them in memory
      ;; undo-limit 80000000                          ; Raise undo-limit to 80Mb
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq
 storage-directory "/HDD/" ;; Where to some main emacs-related file
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

(map! :leader
      (:prefix ("-" . "open file")
       :desc "Edit readings list" "r" #'(lambda () (interactive) (find-file (expand-file-name "readings.org" gtd-directory)))
       :desc "Edit projects file" "p" #'(lambda () (interactive) (find-file  (expand-file-name "projects.org" gtd-directory)))
       :desc "Edit inbox tasks" "i" #'(lambda () (interactive) (find-file (expand-file-name "inbox.org" gtd-directory)))
       :desc "Edit someday tasks" "s" #'(lambda () (interactive) (find-file (expand-file-name "someday.org" gtd-directory)))
       ))

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq doom-modeline-major-mode-icon t)
;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)
;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)
;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)
;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)
;; Whether display the environment version.
(setq doom-modeline-env-version t)

(setq! +biblio-pdf-library-dir pdfs-directory)

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
;; (setq doom-theme 'doom-henna)
(after! ox
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)
(setq org-support-shift-select t)
(after! org
  (setq org-image-actual-width '(300)))
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
        org-src-window-setup 'other-frame
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


(setq org-cite-csl-styles-dir "~/Zotero/styles")

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "output replace")
        (:exports . "results")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        ))

(after! org
  (setq org-html-checkbox-type 'unicode
        org-html-checkbox-types
        '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                   (off . "<span class=\"task-todo\">&#x2610;</span>")
                   (trans . "<span class=\"task-in-progress\">[-]</span>")))))

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

(use-package! graphviz-dot-mode
  :defer t
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(use-package! elfeed-org
  :defer t
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "rss/elfeed.org" org-directory))))

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

(use-package! flycheck-languagetool
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'flycheck-languagetool)))
  :init
  (setq flycheck-languagetool-server-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool-server.jar"))

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/usr/bin/zsh")
  (exec-path-from-shell-initialize))

;; (when (functionp 'module-load)
;; associated jupyter-stata with stata (fixes fontification if using pygmentize for html export)
;;   (add-to-list 'org-src-lang-modes '("jupyter-stata" . stata))
;;   (add-to-list 'org-src-lang-modes '("Jupyter-Stata" . stata))
;; you **may** need this for latex output syntax highlighting
;; (add-to-list 'org-latex-minted-langs '(stata "stata"))
(setq inferior-STA-program-name "/usr/local/bin/jupyter-console")

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

(setq ess-ask-for-ess-directory t
      ess-local-process-name "R"
      ansi-color-for-comint-mode 'filter
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-move-point-for-output t)


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
(set-popup-rule! "^\\*Python*"  :side 'bottom :size .30) ;; Python console to the bottom

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

(load! "scimax-org-latex.el")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(setq org-latex-prefer-user-labels t)

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

(citeproc-org-setup)

(setq reftex-default-bibliography refs-files)

(setq +latex-viewers '(evince pdf-tools okular))

(setq org-highlight-latex-and-related '(native script entities))

(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(setq pdf-annot-activate-created-annotations t
      pdf-view-display-size 'fit-width
      pdf-view-resize-factor 1.1)

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

(setq org-attach-use-inheritance nil)
(require 'org-id)
(setq org-id-track-globally t)
(setq org-roam-completion-everywhere t)

(setq bibtex-completion-bibliography (expand-file-name "zotero_refs.bib" org-directory))
(setq bibtex-completion-library-path pdfs-directory)


(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n
#+SETUPFILE: ../themes/comfy_inline/comfy_inline.theme\n
#+OPTIONS: num:nil ^:{} toc:nil\n
#+INCLUDE: '../header.org'
\n")
         :unnarrowed t)
        ("r" "Bibliographic note" plain
         :if-new (file+head "%<%Y-%m-%d>_${citekey}.org"
                            ":PROPERTIES:
:ID: %<%Y%m%dT%H%M%S>
:CAPTURED: [%<%Y-%m-%d %H:%M:%S>]
:END:
#+TITLE: ${citekey}: ${title} - (${year}, ${journal})
Time-stamp: %<%Y-%m-%d>
#+SETUPFILE: ../themes/comfy_inline/comfy_inline.theme
#+OPTIONS: num:nil ^:{} toc:nil
#+INCLUDE: ../header.org
\n* Backlinks

- keywords :: %^{keywords}

%?

\n* FISH-5SS

|---------------------------------------------+-----|
| <40>                                        |<50> |
| *Background*                                  |     |
| *Supporting Ideas*                            |     |
| *Purpose*                                     |     |
| *Originality/value (Contribution)*            |     |
| *Relevance*                                   |     |
| *Design/methodology/approach*                 |     |
| *Results*                                     |     |
| *(Interesting) Findings*                      |     |
| *Research limitations/implications (Critics)* |     |
| *Uncategorized stuff*                         |     |
| *5SS*                                         |     |
|---------------------------------------------+-----|


\n* Specifics comments

\n* %^{citekey} Highlights
:PROPERTIES:
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}
:NOTER_PAGE:
:END:\n"

                            )
         :immediate-finish t
         :unnarrowed t
         :type org-roam-bibtex
         :jump-to-captured t ))
      )

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (setq orb-preformat-keywords
    '("citekey" "title" "url" "author-or-editor" "keywords" "file" "year")
    orb-process-file-keyword t
    orb-file-field-extensions '("pdf")
  ))

(setq
 bibtex-actions-bibliography (expand-file-name "zotero_refs.bib" org-directory)
 bibtex-actions-at-point-function 'embark-act
 bibtex-actions-file-open-note-function 'orb-bibtex-actions-edit-note
 )
;; Set bibliography paths so they are the same.
(defvar my/bibs refs-files)

(use-package! oc-bibtex-actions
  :bind (("C-c ]" . org-cite-insert)
         ("M-o" . org-open-at-point)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :after (embark oc)
  :config
  (setq bibtex-actions-bibliography my/bibs
        org-cite-global-bibliography my/bibs
        org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'oc-bibtex-actions))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :hook (org-roam . org-roam-ui-mode)
    :config
)

(setq deft-directory notes-directory
      deft-recursive t
      deft-use-filter-string-for-filename t
      deft-default-extension "org"
      )

(setq org-journal-file-type 'weekly
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-file-header "#+TITLE: Weekly Journal\n#+STARTUP: folded"
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
                               "* %t %(org-cliplink-capture) %^g" :prepend t)
                              ("n" "News inbox" entry
                               (file+headline inbox-file-path "News Inbox")
                               "* %t %(org-cliplink-capture) %^g" :prepend t)
                              ))

(setq org-refile-targets '((expand-file-name "projects.org"  gtd-directory :maxlevel . 3)
                           (expand-file-name "reading.org" gtd-directory   :maxlevel . 4)
                           (expand-file-name "someday.org" gtd-directory :level . 1)))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package! org-pandoc-import :after org)

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

(use-package! company-org-block
  :defer t
  :after org
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package! org-transclusion
  :defer
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(defun export-transcluded ()
  (interactive)
  (setq inhibit-read-only t)
  (org-export-dispatch)
  (setq inhibit-read-only nil))

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

(use-package! ox-publish)

(setq org-publish-use-timestamps-flag nil) ;; What is this?
(setq org-export-with-broken-links t)
(setq org-publish-project-alist
      '(("MyOrg"
         :base-directory org-directory
         :base-extension "org"
         :publishing-directory  (expand-file-name "docs/" org-directory)
         :recursive t
         :exclude "./org-html-themes/.*\\|./themes/*"
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t)
        ("org-static"
         :base-directory  (expand-file-name "website/" org-directory)
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "./public_html/"
         :recursive t
         :exclude "./org-html-themes/.*\\|./themes/*"
         :publishing-function org-publish-attachment)
        ))

(use-package! orgdiff :defer t)

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
