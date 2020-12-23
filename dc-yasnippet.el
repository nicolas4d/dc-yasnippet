;;; dc-yasnippet.el --- Aotu add head information for program using yasnippet.

;; Author: nicolas4d
;; Maintainer: nicolas4d
;; Version: 0.1
;; Package-Requires: ((yasnippet))
;; Homepage: homepage
;; Keywords: keywords

;;; Code:

;;; common
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

;;; common ends here

(setq
 ;;; Define keyword
 ;; For emacs lisp
 dc-yasnippet-elisp-keyword-require "require"
 dc-yasnippet-elisp-keyword-list '(dc-yasnippet-elisp-keyword-require)

 ;; For java
 dc-yasnippet-java-keyword-import "import"
 dc-yasnippet-java-keyword-package "package"
 dc-yasnippet-java-keyword-none "none"
 dc-yasnippet-java-keyword-list '(dc-yasnippet-java-keyword-import
                            dc-yasnippet-java-keyword-package
                            dc-yasnippet-java-keyword-none)

 ;; For c++
 dc-yasnippet-cpp-keyword-include "#include"
 dc-yasnippet-cpp-keyword-none "none"
 dc-yasnippet-cpp-keyword-list '(dc-yasnippet-cpp-keyword-include
                           dc-yasnippet-cpp-keyword-none)

 ;; Python
 dc-yasnippet-python-keyword-import "import"
 dc-yasnippet-python-keyword-from "from"
 dc-yasnippet-python-keyword-path "#!"
 dc-yasnippet-python-keyword-none "none"
 dc-yasnippet-python-keyword-list '(dc-yasnippet-python-keyword-import
                              dc-yasnippet-python-keyword-from
                              dc-yasnippet-python-keyword-path
                              dc-yasnippet-python-keyword-none)

 ;; Define mode's information
 dc-yasnippet-mode-info-list '((emacs-lisp-mode
                          dc-yasnippet-elisp-keyword-list
                          dc-yasnippet-elisp-go-place
                          dc-yasnippet-elisp-to-be-found-code
                          dc-yasnippet-elisp-code-rules)

                         (java-mode
                          dc-yasnippet-java-keyword-list
                          dc-yasnippet-java-go-place
                          dc-yasnippet-java-to-be-found-code
                          dc-yasnippet-java-code-rules)

                         (c++-mode
                          dc-yasnippet-cpp-keyword-list
                          dc-yasnippet-cpp-go-place
                          dc-yasnippet-cpp-to-be-found-code
                          dc-yasnippet-cpp-code-rules)

                         (python-mode
                          dc-yasnippet-python-keyword-list
                          dc-yasnippet-python-go-place
                          dc-yasnippet-python-to-be-found-code
                          dc-yasnippet-python-code-rules))

 ;; Define callbacks
 dc-yasnippet-keyword-list nil
 dc-yasnippet-go-place-func nil
 dc-yasnippet-to-be-found-code-func nil
 dc-yasnippet-code-rules-function nil)

(defun tellme (encode)
  "adaptor"
  (interactive)
  (dc-yasnippet encode))

;;;###autoload
(defun dc-yasnippet(encode)
  "Adds needs codes for program file. using with yasnippet.

encode is code that encoded.
example :
emacs-lisp require package.
java program language's import class-name."
  (let* ((code (dc-yasnippet-decode encode)))
    ;; Support major mode?
    (when (dc-yasnippet-support-major-mode-p)
      ;; Is already coded if support?
      (unless (dc-yasnippet-code-p code)
        ;; code if not coded.
        (dc-yasnippet-code code))))
  nil)

;;;###autoload
(defun dc-yasnippet-support-major-mode-p ()
  "Is support current major mode.

Using list dc-yasnippet-mode-info-list.
Return t if support. Return nil if not support."
  (let ((ret nil))
    (dolist (mode-info-list dc-yasnippet-mode-info-list)
      (when (-contains-p mode-info-list major-mode)
        ;; Configure current mode callbacks.
        (setq dc-yasnippet-keyword-list
              (nth 1 mode-info-list)
              dc-yasnippet-go-place-func
              (nth 2 mode-info-list)
              dc-yasnippet-to-be-found-code-func
              (nth 3 mode-info-list)
              dc-yasnippet-code-rules-function
              (nth 4 mode-info-list)
              ret t)))
    ret))

;;;###autoload
(defun dc-yasnippet-code-p (code)
  "Predict code.

code is going to be find."
  (let* (ret found-code-list found-code)
    (setq found-code-list (funcall (eval 'dc-yasnippet-to-be-found-code-func) code))
    (dolist (found-code found-code-list)
      (when (search-backward-regexp (concat
                                     found-code "\\( \\|$\\)+")
                                    0 t)
        (setq ret t)))
    ret))

;;;###autoload
(defun dc-yasnippet-code (code)
  "Code.

code is going to be codes."
  (catch 'break
    (let* ((keyword nil)
           (keyword-list (eval dc-yasnippet-keyword-list)))
      (dolist (keyword keyword-list)
        (message "1 dc-yasnippet-code code %s" code)
        ;; Coding depend on keyword case.
        (when (funcall (eval 'dc-yasnippet-go-place-func) keyword)
          (message "2 dc-yasnippet-code code %s" code)
          (insert code)
          (throw 'break nil)))))
  nil)

;;;###autoload
(defun dc-yasnippet-new-snippet ()
  "New snippet to use."
  (interactive)

  (save-excursion
    (let* (snippet-variable-list snippet-list create-p need-reload-yas)
      ;; support major mode?
      (when (dc-yasnippet-support-major-mode-p)
        ;; search buffer for code list
        (setq snippet-list (dc-yasnippet-snippet-search))
        ;; new snippets
        (dolist (snippet-variable-list snippet-list)
          (setq create-p (dc-yasnippet-new-snippet-file snippet-variable-list))
          (when (and create-p (not need-reload-yas))
            (setq need-reload-yas t)))
        (unless need-reload-yas
          (message "Not found code to new snippet."))))))

;;;###autoload
(defun dc-yasnippet-snippet-file-name (code)
  "Create snippet full file name.

code is using this to concatenate file name."
  (let* (dir file)
    (setq dir (concat yas--default-user-snippets-dir "/"
                      (symbol-name major-mode) "/dc-yasnippet")
          file (concat dir "/" code))
    (unless (file-exists-p dir)
      (dired-create-directory dir)
      )
    file))

;; (dc-yasnippet-snippet-file-name "java.test.ja")
(replace-regexp-in-string "\\." "/"  "test.test.")

;;;###autoload
(defun dc-yasnippet-snippet-search ()
  "Search code that using at create snippets in current buffer.

Returns snippet list using with create snippets,
each atom on each snippet."
  (let (code startPoint endPoint code-rules
             cur-regexp snippet-variable-list code
             (ret-snippet-list ()))

    (setq code-rules (funcall (eval 'dc-yasnippet-code-rules-function)))
    ;; Find code
    (dolist (cur-rule code-rules)
      (setq cur-regexp (eval (eval (car cur-rule))))

      (save-excursion
        (goto-char (point-min))

        ;; make snippet list
        (while (search-forward-regexp cur-regexp nil t)
          (setq endPoint (point))
          (search-backward-regexp cur-regexp nil t)
          (setq startPoint (point))
          (search-forward-regexp cur-regexp nil t)
          (setq code (buffer-substring-no-properties startPoint endPoint))
          (setq snippet-variable-list (eval (eval (car (cdr cur-rule)))))
          (push snippet-variable-list ret-snippet-list)
          )))
    ret-snippet-list))

;;;###autoload
(defun dc-yasnippet-snippet-variable-list (name key text code)
  "Create list using for new snippet.

1. name is the snippet's name.
2. key is the snippet's key.
3. text is the snippet's text that will be write in the current point.
4. code that will be coded."
  (let* (ret)
    (push (dc-yasnippet-encode code) ret) ; code
    (push text ret) ; text
    (push key ret) ; key
    (push name ret) ; name
    ret))

;;;###autoload
(defun dc-yasnippet-set-envirenment-varables ()
  (let ((guessed-directories (yas--guess-snippet-directories))
        (yas-selected-text (or yas-selected-text
                               (and (region-active-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end))))))

    ;;(switch-to-buffer yas-new-snippet-buffer-name)
    ;;(erase-buffer)
    (kill-all-local-variables)
    (snippet-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas--guessed-modes)
         (mapcar (lambda (d) (yas--table-mode (car d)))
                 guessed-directories))
    (set (make-local-variable 'default-directory)
         (car (cdr (car guessed-directories))))))

;;;###autoload
(defun dc-yasnippet-new-snippet-file (snippet-variable-list)
  "Create snippet file and write contents.

snippet-variable-list :
1. name is the snippet's name.
2. key is the snippet's key.
3. text is the snippet's text that will be write in the current point.
4. code that will be coded."
  (let* ((name (nth 0 snippet-variable-list))
         (key (nth 1 snippet-variable-list))
         (text (nth 2 snippet-variable-list))
         (code (nth 3 snippet-variable-list))
         snippet-content file-name ret)

    (setq file-name (dc-yasnippet-snippet-file-name name))
    (unless (file-exists-p file-name)
      (setq ret t)

      (with-temp-file file-name
        (dc-yasnippet-set-envirenment-varables)

        ;; Construct contents
        (setq snippet-content (concat "# -*- mode: snippet -*-\n"
                                      "# name: " name "\n"
                                      "# key: " key ";\n"
                                      "# group: dc-yasnippet\n"
                                      "# --\n"
                                      text"`(dc-yasnippet \"" code "\")"
                                      "`"))
        ;; Insert contents.
        (insert snippet-content)
        (yas-maybe-load-snippet-buffer)
        )
      (message (concat "snippet "
                       (dc-yasnippet-snippet-file-name name)
                       " created successed.")))
    ret))

;;;###autoload
(defun dc-yasnippet-encode (code)
  "Encode code.

Encode \" to -.
Encode space to +."
  (let* (cur-code)
    ;; 37 is % and 34 is "
    (setq cur-code (subst-char-in-string 34 37 code))
    ;; 32 is space and 43 is +
    (subst-char-in-string 32 43 cur-code)))

;;;###autoload
(defun dc-yasnippet-decode (code)
  "Decode code.

Encode - to \".
Decode + to space."
  (let* (cur-code)
    ;; 45 is - and 37 is %
    (setq cur-code (subst-char-in-string 37 34 code))
    ;; 32 is space and 43 is +
    (subst-char-in-string 43 32 cur-code)))

;;;###autoload
(defun dc-yasnippet-beginning-new-previous-line ()
  (ignore-errors
    (beginning-of-line)
    (newline-and-indent)
    (forward-line -1)
    (indent-for-tab-command)
    )
  )

;;;###autoload
(defun dc-yasnippet-end-new-indent-line ()
  (end-of-line)
  (newline-and-indent)
  )

;;;###autoload
(defun dc-yasnippet-end-new-new-indent ()
  (end-of-line)
  (newline-and-indent)
  (newline-and-indent)
  )

;;; For elisp
;;;###autoload
(defun dc-yasnippet-elisp-to-be-found-code (code)
  "Create code list to be found. Using with code."
  (list code))

;;;###autoload
(defun dc-yasnippet-elisp-go-place (keyword)
  "Go to the place where going to be code.

keyword is for general purpose and extension."
  (dc-yasnippet-beginning-new-previous-line)
  t)

;;;###autoload
(defun dc-yasnippet-elisp-code-rules ()
  "Emacs lisp regular expression for search and extract code rulse."
  '(
    (
     '(concat "(" dc-yasnippet-elisp-keyword-require " '.*)")
     '(progn
        (let* ((ret ()) cur-code)
          (setq cur-code (substring code 10 -1))
          (dc-yasnippet-snippet-variable-list cur-code
                                        cur-code
                                        cur-code
                                        code))))))
;;; Ends here for elisp

;;; For java
;;;###autoload
(defun dc-yasnippet-java-to-be-found-code (code)
  "Create code list to be found. "
  (list code))

;;;###autoload
(defun dc-yasnippet-java-go-place (keyword)
  "Go to the palce where going to be code.

Find place by keyword. "
  (let* ((curKeywordValue (eval keyword) )
         ret)
    (if (eq curKeywordValue dc-yasnippet-java-keyword-import)
        (progn
          (when (search-backward dc-yasnippet-java-keyword-import nil t)
            (dc-yasnippet-end-new-indent-line)
            (setq ret t))))
    (if (eq curKeywordValue dc-yasnippet-java-keyword-package)
        (progn
          (when (search-backward dc-yasnippet-java-keyword-package nil t)
            (dc-yasnippet-end-new-new-indent)
            (setq ret t))))
    (if (eq curKeywordValue dc-yasnippet-java-keyword-none)
        (progn
          (goto-char (point-min))
          (setq ret t)))
    ret))

;;;###autoload
(defun dc-yasnippet-java-code-rules ()
  "Java regular expression for search and extract code rulse.

Returns ((expression)(rules))."
  '(
    (
     '(concat "^" dc-yasnippet-java-keyword-import ".*;")
     '(progn
        (let* ((ret ()) class-code class-text class-key class-name)

          (setq class-code (substring code 7 -1))
          (setq class-text (car (last (split-string class-code "\\."))))
          (setq class-key (dc-yasnippet-java-snippet-key class-text))
          (setq class-name class-code)
          (dc-yasnippet-snippet-variable-list class-name
                                        class-key
                                        class-text
                                        code))))))
;;;###autoload
(defun dc-yasnippet-java-snippet-key (str)
  "生成java snippet key。"
  (let* ((ret))
    (if (dc-yasnippet-tow-more-camal-case-p str)
        (setq ret (dc-yasnippet-uppercase-from-camel-case str))
      ;; Using first four letters;
      (progn
        (if (<= (length str) 4)
            (setq ret str)
          (setq ret (substring str 0 4)))))
    (setq ret (downcase ret))
    ret))

;;(dc-yasnippet-java-snippet-key "dd")
;;(dc-yasnippet-java-snippet-key "sdfdd")
;;(dc-yasnippet-java-snippet-key "SdFDC")
;;; Ends here for java

;;; For c++
;;;###autoload
(defun dc-yasnippet-cpp-to-be-found-code (code)
  "Create code list to be found. "
  (list code))

;;;###autoload
(defun dc-yasnippet-cpp-go-place (keyword)
  "Go to the palce where going to be code.

Find place by keyword. "
  (let* ((curKeywordValue (eval keyword) )
         ret)
    (if (eq curKeywordValue dc-yasnippet-cpp-keyword-include)
        (progn
          (when (search-backward dc-yasnippet-cpp-keyword-include nil t)
            (dc-yasnippet-end-new-indent-line)
            (setq ret t))))
    (if (eq curKeywordValue dc-yasnippet-cpp-keyword-none)
        (progn
          (goto-char (point-min))
          (setq ret t)))
    ret))

;;;###autoload
(defun dc-yasnippet-cpp-code-rules ()
  "Java regular expression for search and extract code rulse.

Returns ((expression)(rules))."
  '(
    (
     '(concat "^" dc-yasnippet-cpp-keyword-include " .*[\">]")
     '(progn
        (let* ((ret ()) class-code class-text class-key class-name)

          (setq class-code (substring code 10 -1))
          (setq class-text (first (split-string class-code "\\.")))
          (setq class-key (downcase class-text))
          (setq class-name class-code)
          (dc-yasnippet-snippet-variable-list class-name
                                        class-key
                                        class-text
                                        code))))))
;;; Ends here for c++

;;; For python
;;;###autoload
(defun dc-yasnippet-python-to-be-found-code (code)
  "Create code list to be found. "
  (list code))

;;;###autoload
(defun dc-yasnippet-python-go-place (keyword)
  "Go to the palce where going to be code.

Find place by keyword. "
  (let* ((curKeywordValue (eval keyword) )
         ret)
    (if (or (eq curKeywordValue dc-yasnippet-python-keyword-import)
            (eq curKeywordValue dc-yasnippet-python-keyword-from))
        (progn
          (when (search-backward-regexp (concat "\\("
                                                dc-yasnippet-python-keyword-import
                                                "\\|"
                                                dc-yasnippet-python-keyword-from
                                                "\\)")
                                        nil t )
            (dc-yasnippet-end-new-indent-line)
            (setq ret t))))
    (if (eq curKeywordValue dc-yasnippet-python-keyword-path)
        (progn
          (when (search-backward dc-yasnippet-python-keyword-path nil t)
            (dc-yasnippet-end-new-new-indent)
            (setq ret t))))
    (if (eq curKeywordValue dc-yasnippet-python-keyword-none)
        (progn
          (goto-char (point-min))
          (setq ret t)))
    ret))

;;;###autoload
(defun dc-yasnippet-python-code-rules ()
  "Java regular expression for search and extract code rulse.

Returns (((expression)(rules))...)."
  '(
    (
     '(concat "^" dc-yasnippet-python-keyword-from " .*$")
     '(progn
        (let* ((ret ()) class-code class-text class-key class-name
               split-string-list)

          (setq split-string-list (split-string code " "))

          (setq class-code (substring code 6 -1))
          (setq class-text (nth 3 split-string-list))
          (setq class-key (downcase class-text))
          (setq class-name (concat (nth 1 split-string-list)
                                   "."
                                   class-text))
          (dc-yasnippet-snippet-variable-list class-name
                                        class-key
                                        class-text
                                        code)))
     )(
     '(concat "^" dc-yasnippet-python-keyword-import " .*$")
     '(progn
        (let* ((ret ()) class-code class-text class-key class-name
               split-string-list)

          (setq split-string-list (split-string code " "))

          (setq class-code (nth 1 split-string-list))
          (setq class-text class-code)
          (setq class-key (downcase class-text))
          (setq class-name class-code)
          (dc-yasnippet-snippet-variable-list class-name
                                        class-key
                                        class-text
                                        code))))))
;;; ends here for python

(provide 'dc-yasnippet)

;;; dc-yasnippet.el ends here

