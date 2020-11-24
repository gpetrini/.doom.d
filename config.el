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

(use-package! org-ref
  :after org
  :init
                                        ; code to run before loading org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  )
(setq org-ref-pdf-directory "/HDD/PDFs/")

(use-package! helm-bibtex
  :after org
  :init
  ; blah blah
  :config
  ;blah blah
  )

(setq bibtex-format-citation-functions
      '((org-mode . (lambda (x) (insert (concat
                                         "\\cite{"
                                         (mapconcat 'identity x ",")
                                         "}")) ""))))

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

(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)
(sublimity-mode 0)

(use-package! hungry-delete
  :config
  (add-hook! 'after-init-hook #'global-hungry-delete-mode)
  )

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "/HDD/Org/"
        org-agenda-files '("/HDD/Org/agenda.org")
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t))

(setq org-babel-default-header-args
      '((:session . "yes")
        (:results . "output")
        (:exports . "results")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        ))

(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(use-package! elfeed-org
  :after org
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

(use-package! origami
  :commands (origami-toggle-node origami-toggle-all-nodes)
  :hook (text-mode . origami-mode)
  :init
  :config
  (map! :leader
        :prefix "t"
        :desc "Origami-Toggle All Nodes" "O" #'origami-toggle-all-nodes
        :desc "Origami-Toggle Node" "o" #'origami-toggle-node))

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
