(setq user-full-name "Gabriel Petrini"
      user-mail-address "gpetrinidasilveira@gmail.com")

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                                         ; Set width for tabs
 uniquify-buffer-name-style 'forward      ; Uniquify buffer names
 window-combination-resize t                    ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                                           ; Stretch cursor to the glyph width

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

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

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

(setq  doom-font (font-spec :family "monospace" :size 20 :weight 'semi-light))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(cua-mode +1)
;;(setq org-support-shift-select t)
(after! ox
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
(setq display-line-numbers-type t)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(setq display-line-numbers-type 'relative)
(setq org-support-shift-select t)

(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)
(sublimity-mode 0)

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-agenda-files '("/HDD/Org/agenda.org")
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t))

(setq org-babel-default-header-args
      '((:session . "yes")
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

(setq python-shell-interpreter "/usr/bin/python3")
(setq org-babel-python-command "/usr/bin/python3")
;; Fix Warning "readline" message
(setq python-shell-completion-native-enable nil)
(setq flycheck-python-pylint-executable "pylint")

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(add-hook 'after-init-hook 'global-company-mode)
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(set-company-backend! '(julia-mode)
  '(:separate company-lsp
    company-tabnine
    company-files
    company-yasnippet
    ))

(set-company-backend! '(lisp-mode
                        sh-mode
                        python-mode
                        css-mode
                        org-mode
                        )
  '(:separate company-tabnine
    company-files
    company-yasnippet))

(setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))

(load! "dynare.el")

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("julia" "python" "ipython" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

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
        org-ref-completion-library 'org-ref-helm-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography (list "/HDD/Org/all_my_refs.bib")
        org-ref-note-title-format "* NOTES %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
        org-ref-notes-directory "/HDD/Org/notes/"
        org-ref-notes-function 'orb-edit-notes
        ))

(use-package! org-roam
  :defer t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/HDD/Org/notes/")
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))

(after! org-roam
  (setq org-roam-graph-node-extra-config
        '(("shape"      . "underline")
          ("style"      . "rounded,filled")
          ("fillcolor"  . "#EEEEEE")
          ("color"      . "#C9C9C9")
          ("fontcolor"  . "#111111")
          ("fontname"   . "Overpass")))

  (setq +org-roam-graph--html-template
        (replace-regexp-in-string "%\\([^s]\\)" "%%\\1"
                                  (f-read-text "~/.doom.d/misc/org-roam-template.html")))

  (defadvice! +org-roam-graph--build-html (&optional node-query callback)
    "Generate a graph showing the relations between nodes in NODE-QUERY. HTML style."
    :override #'org-roam-graph--build
    (unless (stringp org-roam-graph-executable)
      (user-error "`org-roam-graph-executable' is not a string"))
    (unless (executable-find org-roam-graph-executable)
      (user-error (concat "Cannot find executable %s to generate the graph.  "
                          "Please adjust `org-roam-graph-executable'")
                  org-roam-graph-executable))
    (let* ((node-query (or node-query
                           `[:select [file titles] :from titles
                             ,@(org-roam-graph--expand-matcher 'file t)]))
           (graph      (org-roam-graph--dot node-query))
           (temp-dot   (make-temp-file "graph." nil ".dot" graph))
           (temp-graph (make-temp-file "graph." nil ".svg"))
           (temp-html  (make-temp-file "graph." nil ".html")))
      (org-roam-message "building graph")
      (make-process
       :name "*org-roam-graph--build-process*"
       :buffer "*org-roam-graph--build-process*"
       :command `(,org-roam-graph-executable ,temp-dot "-Tsvg" "-o" ,temp-graph)
       :sentinel (progn
                   (lambda (process _event)
                     (when (= 0 (process-exit-status process))
                       (write-region (format +org-roam-graph--html-template (f-read-text temp-graph)) nil temp-html)
                       (when callback
                         (funcall callback temp-html)))))))))

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

(use-package org-roam-server
  :after (org-roam server)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))
(after! org-roam
  (unless (server-running-p)
    (smartparens-global-mode -1)
    (org-roam-server-mode 1)
    (smartparens-global-mode 1)))
