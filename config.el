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
;; (global-subword-mode 1)                           ; Iterate through CamelCase words
(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.
(setq org-roam-directory "/HDD/Org/notes/")

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
;; (setq  doom-font (font-spec :family "Fira Mono" :size 20))
(setq  doom-font (font-spec :family "Roboto Mono" :size 20))
(setq doom-theme 'doom-one)
(after! ox
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
(setq display-line-numbers-type t)
(setq org-support-shift-select t)
(setq org-image-actual-width '(300))

(use-package! info-colors
  :defer t
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(map! :map evil-window-map
      "SPC" #'rotate-layout
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(setq org-src-window-setup 'current-window)
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
        refs-directory "/HDD/Org/all_my_refs.bib"
        org-hide-emphasis-markers t))
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))
(add-hook! org-mode :append #'org-appear-mode)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "output replace")
        (:exports . "results")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        ))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
      '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                 (off . "<span class=\"task-todo\">&#x2610;</span>")
                 (trans . "<span class=\"task-in-progress\">[-]</span>"))))

(map! :after counsel :map org-mode-map
      "C-c l l h" #'counsel-org-link)
(after! counsel
  (setq counsel-outline-display-style 'title))

(after! org-id
  ;; Do not create ID if a CUSTOM_ID exists
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(defun zz/make-id-for-title (title)
  "Return an ID based on TITLE."
  (let* ((new-id (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase title))))
    new-id))

(defun zz/org-custom-id-create ()
  "Create and store CUSTOM_ID for current heading."
  (let* ((title (or (nth 4 (org-heading-components)) ""))
         (new-id (zz/make-id-for-title title)))
    (org-entry-put nil "CUSTOM_ID" new-id)
    (org-id-add-location new-id (buffer-file-name (buffer-base-buffer)))
    new-id))

(defun zz/org-custom-id-get-create (&optional where force)
  "Get or create CUSTOM_ID for heading at WHERE.

If FORCE is t, always recreate the property."
  (org-with-point-at where
    (let ((old-id (org-entry-get nil "CUSTOM_ID")))
      ;; If CUSTOM_ID exists and FORCE is false, return it
      (if (and (not force) old-id (stringp old-id))
          old-id
        ;; otherwise, create it
        (zz/org-custom-id-create)))))

;; Now override counsel-org-link-action
(after! counsel
  (defun counsel-org-link-action (x)
    "Insert a link to X.

X is expected to be a cons of the form (title . point), as passed
by `counsel-org-link'.

If X does not have a CUSTOM_ID, create it based on the headline
title."
    (let* ((id (zz/org-custom-id-get-create (cdr x))))
      (org-insert-link nil (concat "#" id) (car x)))))

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
(setq langtool-language-tool-jar "/HDD/Configuracoes/LanguageTool-stable/LanguageTool-5.2/languagetool.jar")
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/usr/bin/zsh")
  (exec-path-from-shell-initialize))

(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

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

(after! python
  (set-company-backend! 'python-mode 'elpy-company-backend))
;; (after! company
;;   (setq company-idle-delay 0.5
;;         company-tooltip-limit 10
;;         company-dabbrev-downcase nil
;;         company-show-numbers t
;;         company-minimum-prefix-length 3)
;;   (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
;; (setq-default history-length 1000)
;; (setq-default prescient-history-length 1000)

;; (set-company-backend!
;;   '(org-mode)
;;   '(:seperate
;;     company-ispell
;;     company-files
;;     company-yasnippet))

;; (use-package! company-tabnine
;;   :defer t
;;   )
;; (after! company
;;   (add-to-list 'company-backends 'company-tabnine))

;; (load! "dynare.el")

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

(setq +latex-viewers '(pdf-tools))
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
  )

(use-package! org-ref
  :after (org bibtex)
  :init
  (setq org-ref-default-bibliography refs-directory)
  (setq bibtex-completion-bibliography org-ref-default-bibliography)
  (setq bibtex-completion-library-path pdfs-directory)
  :config
  (setq org-ref-pdfs-directory pdfs-directory
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography (list refs-directory)
        org-ref-notes-directory org-directory
        org-ref-notes-function 'orb-edit-notes
        ))

(setq
 bibtex-completion-notes-path org-directory
 bibtex-completion-bibliography refs-directory
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "${author-abbre} (${year}, ${journaltitle}): ${title}\n"
  "#+OPTIONS: toc:nil num:nil\n"
  "#+ROAM_KEY: cite:${key}\n"
  "Time-stamp: %<%Y-%m-%d>\n"
  "- tags :: ${keywords}\n"
  "\n* Backlinks\n"
  "\n* FISH-5SS\n"
  "|---------------------------------------------+-----|\n"
  "| <40>                                          |<50> |\n"
  "| *Background*                                  |     |\n"
  "| *Supporting Ideas*                            |     |\n"
  "| *Purpose*                                     |     |\n"
  "| *Originality/value (Contribution)*            |     |\n"
  "| *Relevance*                                   |     |\n"
  "| *Design/methodology/approach*                 |     |\n"
  "| *Results*                                     |     |\n"
  "| *(Interesting) Findings*                      |     |\n"
  "| *Research limitations/implications (Critics)* |     |\n"
  "| *Uncategorized stuff*                         |     |\n"
  "| *5SS*                                         |     |\n"
  "|---------------------------------------------+-----|\n"
  "\n* Specifics comments\n :PROPERTIES:\n :Custom_ID: ${=key=}\n :AUTHOR: ${author-or-editor}\n :JOURNAL: ${journal}\n :YEAR: ${year}\n :DOI: ${doi}\n :URL: ${url}\n :END:\n"
  "\n* PDF Highlights\n:PROPERTIES:\n :NOTER_DOCUMENT: %(orb-process-file-field \"${key}\")\n :END:\n"
  ))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords" "journal" "year" "doi"))
  ;; (orb-process-file-keyword t)
  ;; (orb-file-field-extensions '("pdf" "epub" "html")
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "%<%Y-%m-%d-%H-%M-%S>-${=key=}"
           :head "#+TITLE: ${=key=}: ${title} (${year}, ${journal})
#+OPTIONS: toc:nil num:nil
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
Time-stamp: %<%Y-%m-%d>
- tags :: ${keywords}

\n* Backlinks\n

\n* FISH-5SS
\n
|---------------------------------------------+-----|
| <40>                                          |<50> |
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
\n* Specifics comments\n :PROPERTIES:\n :Custom_ID: ${=key=}\n :AUTHOR: ${author-or-editor}\n :JOURNAL: ${journal}\n :YEAR: ${year}\n :DOI: ${doi}\n :URL: ${url}\n :END:\n
\n* PDF Highlights\n:PROPERTIES:\n :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n :END:\n"
           :unnarrowed t))))

(setq deft-directory notes-directory
      deft-recursive t
      deft-use-filter-string-for-filename t
      deft-default-extension "org"
      )

;; (use-package! org-roam-protocol
;;   :after org-protocol)


(use-package! org-roam-server
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

(setq org-journal-file-type 'weekly
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-file-header "#+TITLE: Weekly Journal\n#+STARTUP: folded"
      )

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "/HDD/Org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")))

(setq org-refile-targets '(("/HDD/Org/gtd/projects.org" :maxlevel . 3)
                           ("/HDD/Org/gtd/someday.org" :level . 1)))

;; (add-to-list 'load-path "your/path/to/mu4e")
;; if you installed it using your package manager
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; if you built from source
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'smtpmail)
;; (add-hook 'message-send-hook 'org-mime-htmlize)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "gpetrinidasilveira@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(after! mu4e
  (setq user-mail-address "gpetrinidasilveira@gmail.com"
        user-full-name  "Gabriel Petrini"
        mu4e-maildir "/HDD/Mail"
        mu4e-root-maildir "/HDD/Mail"
        mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
        mu4e-update-interval  300
        org-mu4e-convert-to-html t
        ;; mu4e-html2text-command "html2text -utf8 -width 72"
        ;; mu4e-html2text-command "w3m -T text/html"
        mu4e-main-buffer-hide-personal-addresses t
        mu4e-view-show-images t
        mu4e-attachment-dir  "~/Downloads"
        mu4e-sent-folder "/gmail/Sent"
        mu4e-drafts-folder "/gmail/Drafts"
        mu4e-trash-folder "/gmail/Trash"
        mu4e-maildir-shortcuts
        '(("/gmail/Inbox"      . ?i)
          ("/gmail/Sent Items" . ?s)
          ("/gmail/Drafts"     . ?d)
          ("/gmail/Trash"      . ?t)))
  )

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(after! org-msg
  (setq org-msg-greeting-name-limit 3
        org-msg-default-alternatives '(text html)
        org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-recipient-names '(("gpetrinidasilveira@gmail.com" . "Gabriel Petrini"))
        org-msg-greeting-name-limit 4
        org-msg-convert-citation t
        org-msg-signature "
 Regards,

 #+begin_signature
 -- *Gabriel Petrini da Silvera*\n
PhD Student at University of Campinas - Brazil\n
 /Sent from Emacs with mu4e and org-msg/
 #+end_signature"))
