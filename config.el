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
;; (setq  doom-font (font-spec :family "Fira Mono" :size 20))
;; (setq  doom-font (font-spec :family "Roboto Mono" :size 20))
;; (setq doom-theme 'doom-material)
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
(after! centaur-tabs
  (setq centaur-tabs-set-bar 'right)
  (centaur-tabs-group-by-projectile-project)
  )

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
        langtool-language-tool-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool.jar"
        langtool-language-tool-server-jar "/opt/LanguageTool-stable/LanguageTool-5.5/languagetool-server.jar"
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

(setq! tree-sitter-load-path '( "/home/gpetrini/.config/emacs/tree-sitter/" "/home/gpetrini/.config/emacs/.local/straight/build-29.3/tree-sitter-langs/bin/" "/home/gpetrini/.tree-sitter/bin/")
       )

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
      bibtex-completion-notes-path "~/Org/notes" ;; FIXME generalize
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
 citar-notes-paths '("~/Org/notes/")
 )

(setq citar-file-open-note-function 'orb-bibtex-actions-edit-note)
(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (preview . "${author editor:30}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (note . "
#+OPTIONS: num:nil ^:{} toc:nil
#+TITLE: ${author editor}: ${title} - (${date year issued:4}, ${journal shortjournal})
#+FILETAGS: ${keywords journal shortjournal}
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
