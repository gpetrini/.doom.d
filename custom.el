(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ein))
 '(safe-local-variable-directories
   '("/home/gpetrini/Org/" "/home/gpetrini/.config/doom/" "~/.config/emacs/"))
 '(safe-local-variable-values
   '((eval setq-local org-tag-alist
      (append org-tag-persistent-alist
       '((:startgroup) ("context" . 99) ("theory" . 116) ("hypo" . 104)
         ("contrib" . 110) ("method" . 109) ("data" . 100) (:endgroup)
         ("results" . 114) ("discuss" . 115) ("critique" . 107) ("limits" . 108)
         ("future" . 102) ("empirics" . 101) ("formal" . 111) ("sim" . 103)
         ("policy" . 112) ("lit" . 105) ("insight" . 106) ("misc" . 120))))
     (buffer-read-only . 1)
     (eval setq-local org-latex-text-markup-alist
      '((bold . "\\hl{%s}") (italic . "\\emph{%s}")
        (underline . "\\underline{%s}") (code . "\\texttt{%s}")))
     (eval progn (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-variable 'disabled nil)
(put 'narrow-to-region 'disabled nil)
