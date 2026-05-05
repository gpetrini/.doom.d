(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ein))
 '(safe-local-variable-values
   '((buffer-read-only . 1)
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
