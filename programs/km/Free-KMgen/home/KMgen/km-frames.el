;;; km-frames.el --- Extensions to the km major mode.

;; Author: Raphael Van Dyck (http://www.algo.be/dev-logiciels.htm)

;;; Commentary:

;; Mainly functions used to set indentation and fontification from files.
;; See the file documentation.txt.

;;; Code:

(km-new-face class lightgreen "Face for class names.")
(km-new-face word purple "Face for words (catch all category).")

(add-hook
 'km-mode-hook
 '(lambda ()
    (make-local-variable 'lisp-body-indent)
    (setq lisp-body-indent *km-body-indent*)
    (km-set-buffer-font-lock-keywords)))

;; make the following characters (CLHS 2.1.4.2) word constituents
;; needed for \<...\> in patterns
;; it would be better to make them symbol constituents
;; and use \_<...\_> instead of \<...\> (requires emacs version 22)
(dolist (char (append "!$%&*:<=>?@^_~+-./" nil))
  (modify-syntax-entry char "w" km-mode-syntax-table))

(defmacro with-file-buffer (buffer-var file &rest body)
  "Evaluates the body with buffer-var bound to a buffer visiting the file."
  (let ((file-var
         (make-symbol "file"))
        (buffer-already-there-p-var
         (make-symbol "buffer-already-there-p")))
    `(let* ((,file-var ,file)
            (,buffer-var (get-file-buffer ,file-var))
            (,buffer-already-there-p-var ,buffer-var))
       (unless ,buffer-already-there-p-var
         (setq ,buffer-var (find-file-noselect ,file-var)))
       (unwind-protect
           (progn ,@body)
         (unless ,buffer-already-there-p-var
           (kill-buffer ,buffer-var))))))

(defun for-each-km-buffer (function)
  "Evaluates the function inside each buffer in km mode."
  (mapc
   #'(lambda (buffer)
       (save-excursion
         (set-buffer buffer)
         (when (eq major-mode 'km-mode)
           (funcall function))))
   (buffer-list)))

(defvar *km-class-indent* 'defun "Lisp-indent-function property.")
(defvar *km-slot-indent* 'defun "Lisp-indent-function property.")
(defvar *km-anon-indent* 'defun "Lisp-indent-function property.")
(defvar *km-nonanon-ident* 'defun "Lisp-indent-function property.")
(defvar *km-word-indent* 'defun "Lisp-indent-function property.")
(defvar *km-body-indent* 4 "Lisp-body-indent property.")

(defun km-type-to-indent (type)
  "Returns the indentation to use for the given type."
  (cond ((equal type "class") *km-class-indent*)
        ((equal type "slot") *km-slot-indent*)
        ((equal type "anon") *km-anon-indent*)
        ((equal type "nonanon") *km-nonanon-ident*)
        ((equal type "word") *km-word-indent*)
        (t nil)))

(defun km-type-to-face (type)
  "Returns the face to use for the given type."
  (cond ((equal type "class") 'km-class-face)
        ((equal type "slot") 'km-slot-face)
        ((equal type "anon") 'km-anon-face)
        ((equal type "nonanon") 'km-nonanon-face)
        ((equal type "word") 'km-word-face)
        (t nil)))

(defvar *km-frames* nil
  "Highlighting keywords loaded from the frames.lisp file.
Set by km-add-frames-from-file.
Used by all buffers in KM mode.")
(defvar *km-names* nil
  "Highlighting keywords loaded from the names.lisp file.
Set by km-add-names-from-file.
Used by all buffers in KM mode.")

(defun km-init-from-files (directory)
  "Sets completion. Sets indentation and face for frames and names."
  (interactive (list
                ;; should use read-directory-name (requires emacs version 22)
                (read-file-name
                 "Directory: "
                 (expand-file-name "~/KMgen/")
                 (let ((default
                         (expand-file-name "~/KMgen/")))
                   (and (file-exists-p default) default))
                 'confirmation)))
  (condition-case err
      (progn
        (setq directory (file-name-directory (expand-file-name directory)))
        (let ((file
               (format "%scompletion-defs.lisp" directory)))
          (when (file-exists-p file)
            (add-completions-from-file file)))
        (let ((file
               (format "%sframes.lisp" directory)))
          (when (file-exists-p file)
            (km-add-frames-from-file file nil)))
        (let ((file
               (format "%snames.lisp" directory)))
          (when (file-exists-p file)
            (km-add-names-from-file file nil)))
        (km-set-font-lock-keywords))
    (error
     (message "The following error happened while initializing km:\n%s"
              (error-message-string err)))))

(defun km-add-frames-from-file (file set-keywords)
  "Sets indentation and face for frames."
  (interactive (list
                (read-file-name
                 "File: "
                 (expand-file-name "~/KMgen/")
                 (let ((default
                         (expand-file-name "~/KMgen/frames.lisp")))
                   (and (file-exists-p default) default))
                 'confirmation)
                t))
  (setq file (expand-file-name file))
  (let ((frames nil))
    (with-file-buffer
     buffer
     file
     (save-excursion
       (set-buffer buffer)
       (goto-char (point-min))
       (setq frames (read buffer))))
    ;; frames: ((<type (string)> <names (strings)> <pattern (string)>) ...)
    (setq
     *km-frames*
     (mapcar
      #'(lambda (type-names-pattern)
          (let* ((type
                  (car type-names-pattern))
                 (indent
                  (km-type-to-indent type))
                 (face
                  (km-type-to-face type)))
            (when indent
              (dolist (name (car (cdr type-names-pattern)))
                (put (intern name) 'lisp-indent-function indent)))
            (when face
              (list
               (concat "\\<\\("
                       (car (cdr (cdr type-names-pattern)))
                       "\\)\\>")
               1
               face))))
      frames))
    (when set-keywords
      (km-set-font-lock-keywords))))

(defun km-add-names-from-file (file set-keywords)
  "Sets indentation and face for names."
  (interactive (list
                (read-file-name
                 "File: "
                 (expand-file-name "~/KMgen/")
                 (let ((default
                         (expand-file-name "~/KMgen/names.lisp")))
                   (and (file-exists-p default) default))
                 'confirmation)
                t))
  (setq file (expand-file-name file))
  (let ((specs nil)
        (specs2 nil))
    (with-file-buffer
     buffer
     file
     (save-excursion
       (set-buffer buffer)
       (goto-char (point-min))
       (condition-case e
           (while t
             (let ((object (read buffer)))
               (cond ((stringp object) ; <type>
                      (push (cons object nil) specs))
                     ((symbolp object) ; <name>
                      (setcdr (car specs) (cons object (cdr (car specs)))))
                     (t
                      nil))))
         (end-of-file))))
    ;; specs: ((<type (string)> <name (symbol)> ...) ...)
    (dolist (spec specs)
      (let* ((type
              (car spec))
             (indent
              (km-type-to-indent type))
             (face
              (km-type-to-face type)))
        (when indent
          (dolist (name (cdr spec))
            (put name 'lisp-indent-function indent)))
        (when face
          (push
           (list
            (concat "\\<\\("
                    (let ((max-specpdl-size 10000))
                      (regexp-opt
                       (mapcar
                        #'(lambda (name) (symbol-name  name))
                        (cdr spec))))
                    "\\)\\>")
            1
            face)
           specs2))))
    (setq *km-names* specs2)
    (when set-keywords
      (km-set-font-lock-keywords))))

(defun km-set-font-lock-keywords ()
  (for-each-km-buffer #'km-set-buffer-font-lock-keywords))

(defun km-set-buffer-font-lock-keywords ()
  (font-lock-add-keywords nil (append *km-frames* *km-names*) 'set))

(defun indent-and-fontify-defun ()
  "Moves backward to the beginning of the defun then indents and fontifies it."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)
    (let ((beg (point)))
      (end-of-defun)
      (let ((end (point)))
        (font-lock-fontify-region beg end)))))

(defun indent-and-fontify-buffer ()
  "Indents and fontifies the current buffer."
  (interactive)
  (with-temp-message "indent-and-fontify-buffer..."
    (indent-region (point-min) (point-max) nil)
    (font-lock-fontify-buffer)))

;;; km-frames.el ends here
