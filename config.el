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
(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.
(setq org-roam-directory "/HDD/Org/notes/")
(setq org-notes-directory "/HDD/Org/notes/")
(setq gtd-directory "/HDD/Org/gtd/")
(setq org-roam-v2-ack t)
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432))  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb

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

(setq! +biblio-pdf-library-dir "/HDD/PDFs/")

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

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-agenda-files '("/HDD/Org/agenda.org")
        org-ellipsis " ▼ "
        org-log-done 'time
        org-enable-roam-support t
        org-directory "/HDD/Org/"
        notes-directory "/HDD/Org/notes"
        pdfs-directory "/HDD/PDFs/"
        refs-files '("/HDD/Org/zotero_refs.bib")
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

;; IMO, modern editors have trained a bad habit into us all: a burning
;; need for completion all the time -- as we type, as we breathe, as we
;; pray to the ancient ones -- but how often do you *really* need that
;; information? I say rarely. So opt for manual completion:
(require 'company)
(setq company-idle-delay nil ;; https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83/3
      company-minimum-prefix-length 3)

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
  (setq rmh-elfeed-org-files (list "/HDD/Org/rss/elfeed.org")))

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
(setq langtool-language-tool-jar "/opt/LanguageTool-stable/LanguageTool-5.2/languagetool.jar")
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

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
;; (use-package! company-tabnine
;;   :defer t
;;   )
;; (after! company
;;   (add-to-list 'company-backends 'company-tabnine))

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

;; (load! "dynare.el")

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

(setq reftex-default-bibliography "/HDD/Org/zotero_refs.bib")

(setq +latex-viewers '(evince pdf-tools zathura okular skim sumatrapdf))

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

(require 'org-roam)
(setq org-roam-directory (file-truename org-notes-directory))
(make-directory org-roam-directory 'parents)
(setq org-roam-verbose t)
(setq org-roam-db-location
      (concat org-roam-directory "/.database/org-roam.db"))
;; (setq +org-roam-open-buffer-on-find-file nil) ; deprecated for v2
;; (setq org-roam-db-update-idle-seconds 30) ; deprecated for v2
;; (setq org-roam-graph-viewer "qutebrowser") ; deprecated for v2

;; Redefining some part of the slug generator.
(cl-defmethod org-roam-node-slug ((node org-roam-node))
  "Return the slug of NODE."
  (let ((title (org-roam-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768    ; U+0300 COMBINING GRAVE ACCENT
                           769    ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                      ;; ("__*" . "_") ;; remove sequential underscores
                      ;; ("^_" . "")   ;; remove starting underscore
                      ;; ("_$" . "")   ;; remove ending underscore
                      ))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))

;; (setq org-roam-capture-templates
;;       `(("p" "Permanent Note" plain "%?"
;;          :if-new (file+head "${slug}.org"
;;                             "#+title: ${title}\n")
;;          :unnarrowed t)

;;         ("l" "Literature Note" plain "%?"
;;          :if-new (file+head "%<%Y%m%d%H%M%S>-${ref}.org"
;;                             "#+TITLE: ${author-abbre} (${year}, ${jornaltitle}): ${title}
;; ,#+OPTIONS: toc:nil num:nil\n
;; ,#+ROAM_KEY: ${ref}\n
;;   ,Time-stamp: %<%Y-%m-%d>\n
;;   ,- tags :: ${keywords}\n
;;   ,\n* Backlinks\n
;;   ,\n* FISH-5SS\n
;;   ,|---------------------------------------------+-----|\n
;;   ,| <40>                                        |<50> |\n
;;   ,| *Background*                                  |     |\n
;;   ,| *Supporting Ideas*                            |     |\n
;;   ,| *Purpose*                                     |     |\n
;;   ,| *Originality/value (Contribution)*            |     |\n
;;   ,| *Relevance*                                   |     |\n
;;   ,| *Design/methodology/approach*                 |     |\n
;;   ,| *Results*                                     |     |\n
;;   ,| *(Interesting) Findings*                      |     |\n
;;   ,| *Research limitations/implications (Critics)* |     |\n
;;   ,| *Uncategorized stuff*                         |     |\n
;;   ,| *5SS*                                         |     |\n
;;   ,|---------------------------------------------+-----|\n
;;                 ")
;;          :unnarrowed t)
;;         ))


;; (require 'org-roam-protocol) ; Deprecated for v2.

;; TODO Have not got it integrated with org protocol. Find out how.
;;
;; Recall that I used to use
;;
;;   `emacsclient "org-protocol://roam-ref?template=r&ref={INSERT-URL}&title={INSERT-TITLE}"`
;;
;; for quickly capturing a webpage.

(use-package! vulpea
  :after org-roam)

(defun vulpea-migrate-buffer ()
  "Migrate current buffer note to `org-roam' v2."
  ;; Create file level ID if it doesn't exist yet
  (org-with-point-at 1
    (org-id-get-create))

  ;; update title (just to make sure it's lowercase)
  (vulpea-buffer-title-set (vulpea-buffer-prop-get "title"))

  ;; move roam_key into properties drawer roam_ref
  (when-let* ((ref (vulpea-buffer-prop-get "roam_key")))
    (org-set-property "ROAM_REFS" ref)
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_key:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_alias into properties drawer roam_aliases
  (when-let* ((aliases (vulpea-buffer-prop-get-list "roam_alias")))
    (org-set-property "ROAM_ALIASES"
                      (combine-and-quote-strings aliases))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_tags into filetags
  (let* ((roam-tags (vulpea-buffer-prop-get-list "roam_tags"))
         (file-tags (vulpea-buffer-prop-get-list "filetags"))
         (path-tags (seq-filter
                     (lambda (x) (not (string-empty-p x)))
                     (split-string
                      (string-remove-prefix
                       org-roam-directory
                       (file-name-directory (buffer-file-name)))
                      "/")))
         (tags (seq-map
                (lambda (tag)
                  (setq tag (replace-regexp-in-string
                             ;; see `org-tag-re'
                             "[^[:alnum:]_@#%]"
                             "_"        ; use any valid char - _@#%
                             tag))
                  (if (or
                       (string-prefix-p "status" tag 'ignore-case)
                       (string-prefix-p "content" tag 'ignore-case)
                       (string-equal "Project" tag))
                      (setq tag (downcase tag)))
                  tag)
                (seq-uniq (append roam-tags file-tags path-tags)))))
    (when tags
      (apply #'vulpea-buffer-tags-set tags)
      (let ((case-fold-search t))
        (org-with-point-at 1
          (while (re-search-forward "^#\\+roam_tags:" (point-max) t)
            (beginning-of-line)
            (kill-line 1))))))

  (save-buffer))

(defun vulpea-migrate-db ()
  "Migrate all notes."
  (dolist (f (org-roam--list-all-files))
    (with-current-buffer (find-file f)
      (message "migrating %s" f)
      (vulpea-migrate-buffer)))

  ;; Step 2: Build cache
  ;; (org-roam-db-sync 'force)
  )

(setq org-attach-use-inheritance nil)
(require 'org-id)
(setq org-id-track-globally t)
(setq org-roam-completion-everywhere t)


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

"\n#+SETUPFILE: ../themes/comfy_inline/comfy_inline.theme\n
#+OPTIONS: num:nil ^:{} toc:nil\n
#+INCLUDE: '../header.org'
         \n\n* Backlinks\n

%?

\n* FISH-5SS
\n
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
\n* Specifics comments\n"
         :if-new (file+head "%<%Y-%m-%d>_${citekey}.org"
                            ":PROPERTIES:
:ID: %<%Y%m%dT%H%M%S>
:CAPTURED: [%<%Y-%m-%d %H:%M:%S>]
:END:
#+TITLE: ${citekey}: ${title} - (${year}, ${journal})
Time-stamp: %<%Y-%m-%d>
#+OPTIONS: toc:nil num:nil"

                            )
         :immediate-finish t
         :unnarrowed t
         :type org-roam-bibtex
         :jump-to-captured t ))
      )


;; (use-package! org-roam
;;   :defer t
;;   :init
;;   (map! :leader
;;         :prefix "n"
;;         :desc "org-roam" "l" #'org-roam-buffer-toggle
;;         :desc "org-roam-node-insert" "i" #'org-roam-node-insert
;;         :desc "org-roam-node-find" "f" #'org-roam-node-find
;;         :desc "org-roam-ref-find" "r" #'org-roam-ref-find
;;         :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;         :desc "org-roam-capture" "c" #'org-roam-capture
;;         :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
;;   (setq
;;         org-roam-directory (file-truename org-notes-directory)
;;         org-roam-db-gc-threshold most-positive-fixnum
;;         org-id-link-to-org-use-id t)
;;   (add-to-list 'display-buffer-alist
;;                '(("\\*org-roam\\*"
;;                   (display-buffer-in-direction)
;;                   (direction . right)
;;                   (window-width . 0.33)
;;                   (window-height . fit-window-to-buffer))))
;;   :config
;;   (setq org-roam-mode-sections
;;         (list #'org-roam-backlinks-insert-section
;;               #'org-roam-reflinks-insert-section
;;               ;; #'org-roam-unlinked-references-insert-section
;;               ))
;;   (org-roam-setup)
;;   (require 'org-roam-protocol)
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain
;;            "%?"
;;            :if-new (file+head "${slug}.org"
;;                               "#+title: ${title}\n")
;;            :immediate-finish t
;;            :unnarrowed t)))
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain
;;            "%?"
;;            :if-new (file+head "${slug}.org"
;;                               "#+title: ${title}\n")
;;            :unnarrowed t)))
;; )

;;   ;; (set-company-backend! 'org-mode '(company-capf))

;; (add-to-list 'display-buffer-alist
;;                '("\\*org-roam\\*"
;;                   (display-buffer-in-direction)
;;                   (direction . right)
;;                   (window-width . 0.33)
;;                   (window-height . fit-window-to-buffer)))


;; (defun org-hide-properties ()
;;   "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward
;;             "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
;;       (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
;;         (overlay-put ov_this 'display "")
;;         (overlay-put ov_this 'hidden-prop-drawer t))))
;;   (put 'org-toggle-properties-hide-state 'state 'hidden))

;; (defun org-show-properties ()
;;   "Show all org-mode property drawers hidden by org-hide-properties."
;;   (interactive)
;;   (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
;;   (put 'org-toggle-properties-hide-state 'state 'shown))

;; (defun org-toggle-properties ()
;;   "Toggle visibility of property drawers."
;;   (interactive)
;;   (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
;;       (org-show-properties)
;;     (org-hide-properties)))

;; (cl-defmethod org-roam-node-directories ((node org-roam-node))
;;   (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;       (format "(%s)" (car (f-split dirs)))
;;     ""))

;; (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
;;   (let* ((count (caar (org-roam-db-query
;;                        [:select (funcall count source)
;;                                 :from links
;;                                 :where (= dest $s1)
;;                                 :and (= type "id")]
;;                        (org-roam-node-id node)))))
;;     (format "[%d]" count)))

;; (setq org-roam-node-display-template "${directories:10} ${tags:10} ${title:100} ${backlinkscount:6}")

(use-package! org-ref
  :after org
  ;; :hook (org-mode . org-ref)
  :config
  (setq org-ref-default-bibliography refs-files)
  (setq bibtex-completion-bibliography refs-files)
  (setq bibtex-completion-library-path pdfs-directory)
  (setq
    org-ref-notes-function 'orb-edit-note
    org-ref-completion-library 'org-ref-helm-bibtex
    org-ref-notes-directory org-notes-directory
    org-ref-default-citation-link "parencite"
    org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)

  (setq bibtex-completion-pdf-extension '(".pdf" ".djvu")
    bibtex-completion-pdf-field "file"))

(setq warning-minimum-log-level :error) ;; https://github.com/tmalsburg/helm-bibtex/issues/280
(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
    '(("citekey" . "=key=") "title" "url" "author-or-editor" "keywords" "file" "year")
    orb-process-file-keyword t
    orb-file-field-extensions '("pdf")
    orb-note-actions-interface 'ivy
    orb-insert-interface 'ivy-bibtex))

(after! org-roam
  (org-roam-bibtex-mode))

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
                               (file+headline "/HDD/Org/gtd/inbox.org" "Tasks inbox") ;; FIXME Generalize path
                               "* TODO %i%?")
                              ("w" "Writing inbox" entry
                               (file+headline "/HDD/Org/gtd/inbox.org" "Writing inbox")
                               "* TODO %i%?")
                              ("f" "Fleeting notes" entry
                               (file+headline "/HDD/Org/gtd/inbox.org" "Fleeting notes")
                               "* WAIT %i%?")
                              ("r" "Readings inbox" entry
                               (file+headline "/HDD/Org/gtd/inbox.org" "Reading Inbox")
                               "* %t %(org-cliplink-capture) %^g" :prepend t)
                              ("n" "News inbox" entry
                               (file+headline "/HDD/Org/gtd/inbox.org" "News Inbox")
                               "* %t %(org-cliplink-capture) %^g" :prepend t)
                              ))

(setq org-refile-targets '(("/HDD/Org/gtd/projects.org" :maxlevel . 3) ;; FIXME Generalize path
                           ("/HDD/Org/gtd/readings.org" :maxlevel . 4)
                           ("/HDD/Org/gtd/someday.org" :level . 1)))

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

(use-package! ebib :after (org latex))

(use-package! ox-publish)

(setq org-publish-use-timestamps-flag nil) ;; What is this?
(setq org-export-with-broken-links t)
(setq org-publish-project-alist
      '(("MyOrg"
         :base-directory "/HDD/Org/"
         :base-extension "org"
         :publishing-directory "/HDD/Org/docs/"
         :recursive t
         :exclude "./org-html-themes/.*\\|./themes/*"
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t)
        ("org-static"
         :base-directory "/HDD/Org/website/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "./public_html/"
         :recursive t
         :exclude "./org-html-themes/.*\\|./themes/*"
         :publishing-function org-publish-attachment)
        ))

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
