(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "{ARCHIVE}"))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-highlight-latex-and-related nil)
 '(org-safe-remote-resources '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(package-selected-packages '(eglot)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light))))
 '(window-divider ((t (:foreground "grey")))))
(put 'customize-variable 'disabled nil)
