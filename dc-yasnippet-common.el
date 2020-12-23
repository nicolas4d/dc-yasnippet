;;;###autoload
(defun dc-yasnippet-list-to-string (list seperator)
  "List to string.
list is elisp list"
  (let* ((ret ""))
    (dolist (cur list)
      ;; when string is symbol.
      (when (and (not (string= cur ""))
                 (symbolp cur))
        (progn
          (setq ret (concat ret (concat (symbol-name cur)) seperator))))
      ;; when just is string.
      (when (not (string= cur ""))
        (setq ret (concat ret cur seperator))))
    ;; remove last seperator.
    (unless (or (string= ret "")
                (string= seperator ""))
      (setq ret (substring ret 0 -1)))
    ret))

;;;###autoload
(defun dc-yasnippet-uppercase-from-camel-case (str)
  "Extrack uppercase string from camel-case.

str - camel-case.
example: CamelCase cc."
  (let* ((string-list)
         (ret-uppercase ()))
    (setq string-list (string-to-list str))
    (dolist (cur string-list t)
      (when (and (>= cur 64) (<= cur 91))
        (setq ret-uppercase (cons (byte-to-string cur) ret-uppercase))))
    (reverse (dc-yasnippet-list-to-string ret-uppercase ""))))

;;(dc-yasnippet-uppercase-from-camel-case "AaaaaZaaaADS")

;;;###autoload
(defun dc-yasnippet-tow-more-camal-case-p (str)
  "Predict the string CamalCase."
  (let* ((ret)
         (uppercase)
         (len))
    (setq uppercase (dc-yasnippet-uppercase-from-camel-case str))
    (setq len (length uppercase))
    (if (> len 1)
        (setq ret t)
      (setq ret nil))
    ret))

;;(dc-yasnippet-tow-more-camal-case-p "AA")
;;(dc-yasnippet-tow-more-camal-case-p "A")
