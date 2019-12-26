;;; sclang-scdoc.el --- SuperCollider Documentation renderering  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Mario Lang

;; Author: Mario Lang <mlang@blind.guru>
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Render SCDoc Help files using the Emacs widget library.
;;
;; Parsing and indexing of schelp files is done by the sclang interpreter.
;; Emacs fetches the serialised document tree from the interpreter
;; and renders text, links and codeblocks into a buffer.
;; This buffer uses the `sclang-scdoc-mode' major mode.
;;
;; Codeblocks are editable inline and allow to execute lines, blocks and
;; regions of code on the fly with the same keybindings used in `sclang-mode'.

;;; Code:

(require 'sclang-interp)
(require 'sclang-language)
(require 'wid-browse)

(defvar-local sclang-scdoc-source-path nil
  "Path to the schelp source file.")

(defvar-local sclang-scdoc-metaclass nil
  "If non-nil, the (meta)class documented by this help document.")

(defvar-local sclang-scdoc-class nil
  "If non-nil, the class documented by this help document.")

(defvar sclang-scdoc-bullet nil
  "Indicates the type of bullet to use for list items.")

(defun sclang-scdoc-documents ()
  "Fetch the list of documented help topics from the interpreter."
  (sclang-eval-sync
   "SCDoc.documents.reject(_.isUndocumentedClass).keys"))

(defun sclang-scdoc-method-args (method &optional instance)
  "Fetch the arguments and respective default values for METHOD.
If INSTANCE is non-nil, METHOD is assumed to refer to an instance method."
  (let ((class (if instance sclang-scdoc-class sclang-scdoc-metaclass)))
    (when class
      (cdr
       (sclang-eval-sync
        (sclang-format
         "var method = %o.asClass.findRespondingMethodFor(%o.asSymbol.asGetter);
          method.argNames.collect{|n, i|
              Association.new(n, method.prototypeFrame[i] !? _.asCompileString)
          }"
         class (symbol-name method)))))))

(defun sclang-scdoc-superclasses ()
  "Fetch the list of superclasses.
If the current help document does not document a class, nil is returned."
  (when sclang-scdoc-class
    (sclang-eval-sync
     (sclang-format "%o.asClass.superclasses.collect(_.name)"
                    sclang-scdoc-class))))

(defun sclang-scdoc-class-file ()
  "If the current help document refers to a class, return the name
of the file implementing it."
  (when sclang-scdoc-class
    (sclang-eval-sync
     (sclang-format "%o.asClass.filenameSymbol.asString" sclang-scdoc-class))))

(defvar sclang-scdoc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-browse-mode-map)
    (define-key map (kbd "SPC") #'scroll-up)
    (define-key map (kbd "C-c C-c") #'sclang-eval-region-or-line)
    (define-key map (kbd "C-c C-d") #'sclang-eval-region)
    (define-key map (kbd "C-c C-e") #'sclang-eval-expression)
    (define-key map (kbd "C-c C-h") #'sclang-find-scdoc)
    (define-key map (kbd "C-c C-s") #'sclang-main-stop)
    (define-key map (kbd "C-c C-v") #'sclang-scdoc-find-schelp)
    (define-key map (kbd "C-c C-w") #'sclang-switch-to-workspace)
    (define-key map (kbd "C-M-x") 'sclang-eval-defun)
    (define-key map (kbd "C-c :") 'sclang-find-definitions)
    (define-key map (kbd "C-c <") 'sclang-clear-post-buffer)
    (define-key map (kbd "C-c >") 'sclang-show-post-buffer)
    map))

(define-derived-mode sclang-scdoc-mode widget-browse-mode "SCDoc"
  "Major mode for displaying SuperCollider Documentation.")

(defun sclang-scdoc-find-schelp ()
  "Visit the source schelp file for this help document."
  (interactive)
  (if sclang-scdoc-source-path
      (find-file sclang-scdoc-source-path)
    (when (called-interactively-p 'interactive)
      (message "No corresponding schelp file known for this help document"))))

(defvar sclang-scdoc-topic-history nil
  "History of help topics.")

(defun sclang-find-scdoc (topic)
  "Display help TOPIC in a buffer."
  (interactive
   (list
    (completing-read
     "Help topic: "
     (sclang-scdoc-documents)
     nil t nil
     'sclang-scdoc-topic-history)))
  (pcase (sclang-eval-sync
          (sclang-format
           "SCDoc.documents[%o] !? {|doc|
              [doc.fullPath,
               doc.isClassDoc.if(
                   {[doc.klass.class.asSymbol, doc.klass.asSymbol]},
                   {[nil, nil]}
               ),
               SCDoc.parseDoc(doc)
              ]
            }"
           topic))
    (`(,source-path (,metaclass ,class) (DOCUMENT nil (HEADER nil . ,header) (BODY nil . ,body)))
     (let ((title (car-safe (cdr (assq 'TITLE header))))
           (summary (car-safe (cdr (assq 'SUMMARY header)))))
       (with-current-buffer (get-buffer-create (format "*SCDoc:%s*" title))
         (let ((inhibit-read-only t)
               (inhibit-modification-hooks t))
           (delete-all-overlays) (erase-buffer))
         (sclang-scdoc-mode)

         (setq header-line-format (list title " - " summary)
               sclang-scdoc-source-path source-path
               sclang-scdoc-metaclass metaclass
               sclang-scdoc-class class)

         (let ((superclasses (sclang-scdoc-superclasses)))
           (when superclasses
             (widget-insert (format "%S" sclang-scdoc-class))
             (dolist (superclass superclasses)
               (widget-insert " : ")
               (widget-create
                'link
                :tag (format "%S" superclass)
                :action (lambda (widget _event)
                          (sclang-find-scdoc (widget-value widget)))
                (format "Classes/%S" superclass)))
             (widget-insert "\n\n")))

         (let ((class-file (sclang-scdoc-class-file)))
           (when class-file
             (widget-insert "Source: ")
             (widget-create
              'push-button
              :tag "Definitions"
              :action (lambda (widget _event)
			(sclang-perform-command 'classDefinitions (widget-value widget)))
	      (symbol-name sclang-scdoc-class))
             (widget-insert "\n\n")))

         (mapc #'sclang-scdoc-insert body)

         (widget-setup)
         (goto-char (point-min))
         (switch-to-buffer (current-buffer)))))))

(defvar scdoc-codeblock-keymap
  (let ((map (copy-keymap widget-text-keymap)))
    (define-key map (kbd "C-c C-c") #'sclang-eval-region-or-line)
    (define-key map (kbd "C-c C-d") #'sclang-eval-region)
    (define-key map (kbd "C-c C-e") #'sclang-eval-expression)
    (define-key map (kbd "C-c C-h") #'sclang-find-scdoc)
    (define-key map (kbd "C-c C-s") #'sclang-main-stop)
    (define-key map (kbd "C-c C-v") #'sclang-scdoc-find-schelp)
    (define-key map (kbd "C-c C-w") #'sclang-switch-to-workspace)
    (define-key map (kbd "C-M-x") 'sclang-eval-defun)
    (define-key map (kbd "C-c :") 'sclang-find-definitions)
    (define-key map (kbd "C-c <") 'sclang-clear-post-buffer)
    (define-key map (kbd "C-c >") 'sclang-show-post-buffer)
    map))

(define-widget 'scdoc-codeblock 'text
  "A multiline codeblock."
  :format "%v"
  :keymap scdoc-codeblock-keymap)

(defconst sclang-scdoc-ignore
  '(CPRIVATE IPRIVATE COPYMETHOD CCOPYMETHOD ICOPYMETHOD)
  "SCDocNode node identifiers which should be ignored.")

(defun sclang-scdoc-insert (node)
  (unless (memq (car node) sclang-scdoc-ignore)
    (let ((handler (intern-soft (format "sclang-scdoc-%S" (car node)))))
      (if handler
	  (apply handler (cdr node))
	(widget-insert (format "%S" node))))))

(defun sclang-scdoc-ARGUMENTS (text &rest children)
  (cl-assert (null text))
  (mapc #'sclang-scdoc-insert children))

(defun sclang-scdoc-ARGUMENT (name &rest children)
  (when name (widget-insert name "\n\t"))
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-CLASSMETHODS (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "# Class Methods\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-CMETHOD (_text &rest children)
  (pcase children
    (`((METHODNAMES nil . ,names) (METHODBODY nil . ,body))
     (sclang-scdoc-ensure-blank-line)
     (dolist (method-name (mapcar (lambda (arg) (intern (cadr arg))) names))
       (widget-insert (format "%S.%S"
                              sclang-scdoc-class
                              method-name))
       (let ((method-args (sclang-scdoc-method-args method-name)))
	 (when method-args
	   (widget-insert
	    "("
	    (mapconcat
	     (pcase-lambda (`(,arg . ,default))
	       (if default
		   (format "%s: %s" (symbol-name arg) default)
		 (symbol-name arg)))
	     method-args
	     ", ")
	    ")")))
       (widget-insert "\n"))
     (mapc #'sclang-scdoc-insert body))))

(defun sclang-scdoc-CODE (text)
  (widget-insert text))

(defun sclang-scdoc-CODEBLOCK (code)
  (sclang-scdoc-ensure-blank-line)
  (widget-create 'scdoc-codeblock code)
  (widget-insert "\n"))

(defun sclang-scdoc-DEFINITIONLIST (_text &rest children)
  (mapc #'sclang-scdoc-insert children))

(defun sclang-scdoc-DESCRIPTION (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "# Description\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-DISCUSSION (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "### Discussion\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-EMPHASIS (text)
  (widget-insert text))

(defun sclang-scdoc-EXAMPLES (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "# Examples\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-IMAGE (string)
  (cl-multiple-value-bind (image title)
      (pcase (split-string string "#")
	(`(,image) (list image image))
	(`(,image ,title) (list image title)))
    (if (display-images-p)
	(insert-image
	 (create-image
	  (expand-file-name image (file-name-directory sclang-scdoc-source-path)))
	 title)
      (widget-insert title))))
  
(defun sclang-scdoc-IMETHOD (_text &rest children)
  (pcase children
    (`((METHODNAMES nil . ,names) (METHODBODY nil . ,body))
     (sclang-scdoc-ensure-blank-line)
     (dolist (method-name (mapcar (lambda (arg) (intern (cadr arg))) names))
       (widget-insert (symbol-name method-name))
       (let ((method-args (sclang-scdoc-method-args method-name t)))
	 (when method-args
	   (widget-insert
	    "("
	    (mapconcat
	     (pcase-lambda (`(,arg . ,default))
	       (if default
		   (format "%s: %s" (symbol-name arg) default)
		 (symbol-name arg)))
	     method-args
	     ", ")
	    ")")))
       (widget-insert "\n"))
     (mapc #'sclang-scdoc-insert body))))

(defun sclang-scdoc-INSTANCEMETHODS (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "# Instance Methods\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-ITEM (_text &rest children)
  (cond ((numberp sclang-scdoc-bullet)
	 (prog1
	     (widget-insert (format "%d. " sclang-scdoc-bullet))
	   (setq sclang-scdoc-bullet (1+ sclang-scdoc-bullet))))
	(t
	 (widget-insert "* ")))
  (mapc #'sclang-scdoc-insert children))

(defun sclang-scdoc-LINK (string)
  (cl-multiple-value-bind (document anchor title)
      (pcase (split-string string "#")
        (`(,document) (list document "" ""))
        (`(,document ,anchor) (list document anchor ""))
        (`(,document ,anchor ,title . ,ignored) (list document anchor title)))
    (if (string-match "\\`\\(https?\\|ftp\\|file\\)://" document)
	(widget-create 'url-link document)
      (let ((title
	     (cond
	      ((> (length title) 0)
	       title)
	      ((= (length document) 0)
	       (when (> (length anchor) 0)
		 anchor))
	      (t
	       (or (sclang-eval-sync
		    (sclang-format "SCDoc.documents[%o] !? _.title" document))
		   document)))))
	(widget-create
	 'link
	 :tag title
	 :action (lambda (widget _event)
		   (sclang-find-scdoc (widget-value widget)))
	 document)))))

(defun sclang-scdoc-LIST (_text &rest children)
  (let ((sclang-scdoc-bullet t))
    (mapc #'sclang-scdoc-insert children)))

(defun sclang-scdoc-NL (_text &rest _children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-NOTE (_text &rest children)
  (widget-insert "NOTE: ")
  (mapc #'sclang-scdoc-insert children)
  (widget-insert "\n"))

(defun sclang-scdoc-NUMBEREDLIST (_text &rest children)
  (let ((sclang-scdoc-bullet 1))
    (mapc #'sclang-scdoc-insert children)))

(defun sclang-scdoc-METHOD (args &rest children)
  (pcase children
    (`((METHODNAMES nil . ,names) (METHODBODY nil . ,body))
     (sclang-scdoc-ensure-blank-line)
     (dolist (method-name (mapcar (lambda (arg) (intern (cadr arg))) names))
       (widget-insert (symbol-name method-name))
       (when args (widget-insert args))
       (widget-insert "\n"))
     (mapc #'sclang-scdoc-insert body))))

(defun sclang-scdoc-PROSE (_text &rest children)
  (let ((begin (point)))
    (mapc #'sclang-scdoc-insert children)
    (widget-insert "\n")
    (fill-region begin (point))
    (sclang-scdoc-ensure-blank-line)))

(defun sclang-scdoc-RETURNS (_text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "### Returns\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-SECTION (text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "# " text "\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-SOFT (text)
  (widget-insert text))

(defun sclang-scdoc-STRING (text)
  (widget-insert text))

(defun sclang-scdoc-STRONG (text)
  (widget-insert text))

(defun sclang-scdoc-SUBSECTION (text &rest children)
  (sclang-scdoc-ensure-blank-line)
  (widget-insert "## " text "\n\n")
  (mapc #'sclang-scdoc-insert children)
  (sclang-scdoc-ensure-blank-line))

(defun sclang-scdoc-TELETYPE (text)
  (widget-insert text))

(defun sclang-scdoc-TELETYPEBLOCK (string)
  (widget-insert "\n")
  (widget-insert string)
  (widget-insert "\n"))

(defun sclang-scdoc-TEXT (text)
  (widget-insert text))

(defun sclang-scdoc-WARNING (_text &rest children)
  (widget-insert "WARNING: ")
  (mapc #'sclang-scdoc-insert children)
  (widget-insert "\n"))

(defun sclang-scdoc-ensure-blank-line ()
  (if (not (bolp))
      (widget-insert "\n\n")
    (let ((ch (char-before (1- (point)))))
      (when (and ch (/= ch ?\n))
	(widget-insert "\n")))))

(provide 'sclang-scdoc)
;;; sclang-scdoc.el ends here
