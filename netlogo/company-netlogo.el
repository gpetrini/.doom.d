(require 'netlogo-mode)
(require 'cl)

(defgroup company-netlogo nil
  "company backend for NetLogo"
  :version "25")
(defcustom company-netlogo-fuzzy t "uses fuzzy matching (subsequence) if
non-nil"
  :type '(choice (const :tag "On" t)
                 (const :tag "Off" nil ))
  :group 'company-netlogo)

(defun company-netlogo-make-item (candidate)
  "propertized string with all info of a candidate"
  (let ((text (car candidate))
        (meta (cadr candidate))
        (docstring (cddr candidate)))
    (propertize text 'meta meta 'doc docstring)))

(defun company-netlogo-meta (candidate)
  "Returns meta for candidate"
  (format "%s " (get-text-property 0 'doc candidate)))

(defun company-netlogo-annotation (candidate)
  "returns annotation for candidate"
  (format "%s" (get-text-property 0 'meta candidate)))

(defun company-netlogo-candidates (prefix)
  "returns candidates for prefix"
  (let (candidates)
    (when company-netlogo-fuzzy
      (dolist (x company-netlogo-functions)
        (when (company-netlogo-fuzzy-match prefix (car x))
          (push (company-netlogo-make-item x) candidates))))
    (dolist (x company-netlogo-functions)
      (when (string-prefix-p prefix (car x))
        (push (company-netlogo-make-item x) candidates)))
    candidates
    ))

(defun company-netlogo-fuzzy-match (prefix candidate)
  (cl-search prefix candidate))

(defun company-netlogo (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-netlogo))
    (prefix (company-grab-symbol))
    (candidates (company-netlogo-candidates arg))
    (meta (company-netlogo-meta arg)) 
    (annotation (company-netlogo-annotation arg))
    (no-cache 't)
    (sorted 't)
    (duplicates 't)))

(load-file (file-relative-name "signatures.el"))

(provide 'company-netlogo)
