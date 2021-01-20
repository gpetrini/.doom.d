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

(use-package! ess
  :defer t
  :demand t
  :init
  (require 'ess-site)
  :config
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

  ;; insert pipes etc...
  (defun tide-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert " <- "))
  (defun tide-insert-pipe ()
    "Insert a %>% and newline"
    (interactive)
    (insert " %>%"))
  ;; set keybindings
  ;; insert pipe
  (define-key ess-r-mode-map (kbd "M-s-'") 'tide-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M-s-'") 'tide-insert-assign)
  ;; insert assign
  (define-key ess-r-mode-map (kbd "M-s-\"") 'tide-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-s-\"") 'tide-insert-pipe)
  )

;; ess-view
;; open a df in an external app
;; TODO need to find out how to display options vertically
(use-package! ess-view
  ;; :ensure t
  :after ess
  :diminish
  :config
  (setq ess-view--spreadsheet-program "open") ; open in system default on macos
  (setq ess-view-inspect-and-save-df t)
  ;; enable ess-view package to be triggered from the source doc
  ;; see: <https://github.com/GioBo/ess-view/issues/9>
  (defun ess-view-extract-R-process ()
    "Return the name of R running in current buffer."
    (let*
        ((proc (ess-get-process))         ; Modified from (proc (get-buffer-process (current-buffer)))
         (string-proc (prin1-to-string proc))
         (selected-proc (s-match "^#<process \\(R:?[0-9]*\\)>$" string-proc)))
      (nth 1 (-flatten selected-proc))
      )
    )
  :bind
  (
   ("C-c C-e C-v" . ess-view-inspect-df)
   ;; the below doesn't work on osx
   ;; see <https://github.com/GioBo/ess-view/issues/5>
   ;; ("C-x q" . ess-view-inspect-and-save-df)
   )
  )

(map! :leader
      :prefix "m"
      "cv"      #'ess-view-inspect-df
      "cc"       'ess-tide-insert-chunk
      "w"        'ess-eval-word
      )

(use-package! ess-view-data
  :load-path "./ess-view-data"
  :after ess
  :init
  (require 'ess-view-data))

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

(global-set-key (kbd "C-x 9") 'ess-tide-R-scratch)

;; Graphics device management ;;
(defun ess-tide-new-gdev ()
  "create a new graphics device"
  (interactive)
  (ess-eval-linewise "dev.new()"))

(defun ess-tide-cur-gdev ()
  "return current graphics device"
  (interactive)
  (ess-eval-linewise "dev.cur()"))

(defun ess-tide-list-all-gdev ()
  "list all graphics devices"
  (interactive)
  (ess-eval-linewise "dev.list()"))

(defun ess-tide-switch-to-gdev ()
  "Prompt for the number of the graphics device to make current"
  (interactive)
  (setq dev-num
        (read-from-minibuffer "Select R graphics device: "
                              nil nil t t "1"))
  (ess-eval-linewise
   (format "dev.set(%s)" dev-num)))

(defun ess-tide-switch-next-gdev ()
  "switch to next available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.next())"))

(defun ess-tide-switch-prev-gdev ()
  "switch to previous available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.prev())"))

(defun ess-tide-save-gdev-pdf ()
  "Save current graphics device as pdf"
  (interactive)
  (ess-eval-linewise "dev.copy2pdf()"))

(defun ess-tide-capture-gdev ()
  "Capture current graphics device as image"
  (interactive)
  (ess-eval-linewise "dev.capture()"))

;; Devtools
(defun ess-tide-devtools-setup ()
  "setup R package in current working directory"
  (interactive)
  (ess-eval-linewise "devtools::setup()"))

;; eval any word where the cursor is (objects, functions, etc)
(defun ess-eval-word ()
  (interactive)
  (let ((x (ess-edit-word-at-point)))
    (ess-eval-linewise (concat x)))
  )
;; key binding
(define-key ess-mode-map (kbd "C-c r") 'ess-eval-word)
;; (load! "+ess.el")

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

;; (require 'simple-httpd)
;; (setq httpd-root "/var/www")
;; (httpd-start)
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

(defun doom-buffer-has-long-lines-p ()
  (when comment-use-syntax
    (so-long-detected-long-line-p)))
(setq so-long-predicate #'doom-buffer-has-long-lines-p)
