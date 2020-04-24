;;; python
(defun test-tell-new-snippet-python ()
  (with-current-buffer-window
   "*scratch*"
   nil
   nil

   (python-mode)

   (dolist (cur-import (list "from tellme import tm"
                             "import tm"))
     ;; New snippets from ... import
     (beginning-of-buffer)
     (kill-line 10)
     (insert cur-import)
     (tellme-new-snippet)

     ;; test case : none
     (beginning-of-buffer)
     (kill-line 10)
     (insert "tm;")
     (yas-expand)

     ;; test case : coded
     (beginning-of-buffer)
     (kill-line 10)
     (insert cur-import)
     (newline)
     (insert "tm;")
     (yas-expand)

     ;; test case : import
     (beginning-of-buffer)
     (kill-line 10)
     (insert "import some")
     (newline-and-indent)
     (insert "tm;")
     (yas-expand)

     ;; test case : from
     (beginning-of-buffer)
     (kill-line 10)
     (insert "from some import some")
     (newline-and-indent)
     (insert "tm;")
     (yas-expand)

     ;; test case : path
     (beginning-of-buffer)
     (kill-line 10)
     (insert "#!some")
     (newline-and-indent)
     (insert "tm;")
     (yas-expand)

     ;; clean
     (shell-command "rm -r ~/.emacs.d/snippets/python-mode/tellme")
     (yas-reload-all))))

(when nil
  (test-tell-new-snippet-python)

  )

;;; c++
(defun test-tell-new-snippet-cpp ()
  (with-current-buffer-window
   "*scratch*"
   nil
   nil

   (c++-mode)

   ;; New snippets
   (insert "#include \"tellme.h\"")
   (tellme-new-snippet)

   ;; test case : none
   (beginning-of-buffer)
   (kill-line 100)
   (insert "tellme;")
   (yas-expand)

   ;; test case : coded
   (beginning-of-buffer)
   (kill-line 100)
   (insert "#include \"tellme.h\"")
   (newline-and-indent)
   (insert "tellme;")
   (yas-expand)

   ;; test case : include
   (beginning-of-buffer)
   (kill-line 100)
   (insert "#include some")
   (newline-and-indent)
   (insert "tellme;")
   (yas-expand)

   ;; clean
   (shell-command "rm -r ~/.emacs.d/snippets/c++-mode/tellme")
   (yas-reload-all)))

(when nil
  (test-tell-new-snippet-cpp)

  )

;;; java test
(defun test-tell-new-snippet-java ()
  (with-current-buffer-window
   "*scratch*"
   nil
   nil

   (java-mode)
   ;; Insert import statement.
   (beginning-of-buffer)
   (insert "import tellme.tm;")
   ;; new snippet
   (tellme-new-snippet)
   ;; Delete statement.
   (beginning-of-buffer)
   (kill-line)

   ;; Test case : none
   (insert "tm;")
   (yas-expand)
   (sleep-for 10)

   ;; Test case : coded
   (backward-word)
   (newline-and-indent)
   (end-of-line)
   (insert ";")
   (yas-expand)

   ;; Test case : package
   (beginning-of-buffer)
   (kill-line 10)

   (insert "package;")
   (newline-and-indent)
   (insert "tm;")
   (yas-expand)

   ;; Test case : import
   (beginning-of-buffer)
   (kill-line 10)

   (insert "import;")
   (newline-and-indent)
   (insert "tm;")
   (yas-expand)

   ;; clean
   (shell-command "rm -r ~/.emacs.d/snippets/java-mode/tellme")
   (yas-reload-all)
   )
  )

(when nil
  (test-tell-new-snippet)

  )

;;; test emacs-lisp
(defun test-tellme-new-snippet-elisp ()
  (with-current-buffer-window
   "*scratch*"
   nil
   nil

   (emacs-lisp-mode)

   (setq test-require (tellme-encode "(require 'tellme)"))
   (dolist (cur-require (list test-require))

     (setq cur-require (tellme-decode cur-require))

     ;; new snippet
     (beginning-of-buffer)
     (kill-line 10)
     (insert cur-require)
     (tellme-new-snippet)

     ;; Test case : none
     (beginning-of-buffer)
     (kill-line 10)
     (insert "tellme;")
     (yas-expand)

     ;; test case : coded
     (beginning-of-buffer)
     (kill-line 10)
     (insert cur-require)
     (newline-and-indent)
     (insert "tellme;")
     (yas-expand)

     ;; clean
     (shell-command "rm -r ~/.emacs.d/snippets/emacs-lisp-mode/tellme")
     (yas-reload-all))))

(when nil
  (test-tellme-new-snippet-elisp)

  )

