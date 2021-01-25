(setq user-full-name "Gabriel Petrini"
      user-mail-address "gpetrinidasilveira@gmail.com")

(setq-default
    delete-by-moving-to-trash t                      ; Delete files to trash
    tab-width 4                                                         ; Set width for tabs
    uniquify-buffer-name-style 'forward      ; Uniquify buffer names
    window-combination-resize t                    ; take new window space from all other windows (not just current)
    x-stretch-cursor t
 )                                           ; Stretch cursor to the glyph width

(setq undo-limit 80000000                          ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                                    ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t      ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.
(setq org-roam-directory "/HDD/Org/notes/")

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                           ; On laptops it's nice to know how much power you have

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq! +biblio-pdf-library-dir "/HDD/PDFs/")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq doom-font (font-spec :family "Yanone Kaffeesatz" :size 30))
(setq  doom-font (font-spec :family "monospace" :size 20 :weight 'semi-light))
(setq doom-theme 'doom-one)
(cua-mode +1)
(after! ox
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
(setq display-line-numbers-type t)
(setq org-support-shift-select t)
(setq org-image-actual-width '(300))

(setq org-src-window-setup 'current-window)
(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-agenda-files '("/HDD/Org/agenda.org")
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t))

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "output replace")
        (:exports . "results")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        ))

(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :defer t
  :mode ("\\.dot\\'" "\\.gz\\'"))

(use-package! elfeed-org
  :after org
  :defer t
  :config
  (setq rmh-elfeed-org-files (list "~/Dropbox/Emacs/elfeed.org")))

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
(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US")
(setq flyspell-correct-popup t)
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

(setq ess-use-flymake nil)
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

(setq python-shell-interpreter "/usr/bin/python3")
(setq org-babel-python-command "/usr/bin/python3")
;; Fix Warning "readline" message
(setq python-shell-completion-native-enable nil)
(setq flycheck-python-pylint-executable "pylint")

(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)
(setq company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
(add-hook 'after-init-hook 'global-company-mode)
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(after! company-box
  (setq company-box-max-candidates 5))
;; (use-package! company-prescient
;;   :defer t
;;   :after company
;;   :hook (company-mode . company-prescient-mode))


(after! company
  (setq company-tooltip-limit 5
        company-tooltip-minimum-width 80
        company-tooltip-minimum 5
        company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

;; (load! "dynare.el")

(load! "scimax-org-latex.el")

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-prefer-user-labels t)

(citeproc-org-setup)

(use-package! org-ref
  :defer t
  :after (org bibtex)
  :init
  (setq org-ref-default-bibliography '("/HDD/Org/all_my_refs.bib"))
  (setq bibtex-completion-bibliography org-ref-default-bibliography)
  :config
  (setq org-ref-pdf-directory "/HDD/PDFs/"
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex
        org-ref-default-bibliography (list "/HDD/Org/all_my_refs.bib")
        org-ref-notes-directory "/HDD/Org/notes/"
        org-ref-notes-function 'orb-edit-notes
        ))

(use-package! org-roam-bibtex
  :defer t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "%<%Y-%m-%d-%H-%M-%S>-${=key=}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: %<%Y-%m-%d>
- tags :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: %a\n  :END:
\n** FISH-5SS
\n
|---------------------------------------------+-----|
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
\n** Backlinks\n
\n* Specifics comments
"
           :unnarrowed t)))

  )

(use-package! org-roam-server
  ;; :defer t
  :after (org-roam server)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-arrows "to"
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (smartparens-global-mode -1)
    (org-roam-server-mode)
    (smartparens-global-mode +1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
    )
  )

;; (after! org-roam
;;  (smartparens-global-mode -1)
;;  (org-roam-server-mode +1)
;;  (smartparens-global-mode +1)
;;  )
