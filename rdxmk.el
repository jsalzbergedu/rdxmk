;;; rdxmk.el --- A small set of tools for redox developments

;; Copyright (C) 2017 Jacob Salzberg

;; Author: Jacob Salzberg <jsalzbergedu@yahoo.com>
;; Version: 0.10
;; Keywords: redox, convenience, tools
;; URL: https://github.com/jsalzbergedu/rdxmk

;;; Commentary:

;; rdxmk provides a few tools to make redox development easier in Emacs.
;; rdxmk provides a few convenience functions for building redox:
;; You can run make with no arguments (M-x rdxmk-make-narg RET), make with arguments
;; (M-x rdxmk-make-warg RET arg RET), run the built in rdxmk-make-qemu and rdxmk-make-all, and use cookbook
;; (M-x Rdxmk-Cookbook RET package RET option RET.)
;; Finally, you can stop Emacs from inserting files all over your redox/ directory by
;; going to rdxmk's customization group with `M-x customize-group RET rdxmk RET`
;; and setting `lockfile-no-pollute` to t. For more information, go to the
;; documentation with `C-h i`

;;; Installation

;; For a manual installation, clone the repo in your ~/.emacs.d/, add
;;(add-to-list 'load-path "~/.emacs.d/rdxmk")
;;(load "rdxmk")
;; to your startup file, and install rdxmk.info to your root
;; dir file using the shell tool, `install-info`.
;; For faster load time, run `C-u M-x byte-recompile-directory`
;; and run it on tne ~/.emacs.d/rdxmk/ directory.
;; If this becomes available on MELPA, simply add http://melpa.org/packages
;; to your Package Archives under `M-x customize-group RET package RET` and
;; install rdxmk using `M-x package-install RET`

;;; Code:

(defun rdxmk-up-a-dir (dir)
  "Helper function to return the directory one up from DIR."
  ;; As far as I can tell, this is the idiomatic way to
  ;; "traverse a filesystem tree," even though the built in
  ;; docs have those two functions in switched places.
  (file-name-directory (directory-file-name dir)))


(defun rdxmk-recurse-for-file (file dir)
  "Will call itself FILE or until DIR is found or has a length of 1.
If DIR has a length of 1 and FILE is not found, returns nil.
If FILE is found, returns the directory and file."
  (if (= (length dir) 1)
      (if (file-exists-p (expand-file-name file dir))
	  (expand-file-name file dir)
	'())
    (if (file-exists-p (expand-file-name file dir))
	(expand-file-name file dir)
      (rdxmk-recurse-for-file file (rdxmk-up-a-dir dir)))))

(defun rdxmk-get-closest-pathname (&optional file)
 "By default, RDXMK-GET-CLOSEST-PATHNAME will find the closest makefile.
Will recursivley go up to parent directories.
If FILE is specified, searches for FILE in the same way."
 (if file
     (rdxmk-recurse-for-file file default-directory) ; recurse until it finds FILE goes here
     (rdxmk-recurse-for-file "Makefile" default-directory))) ; recurse until it finds Makefile goes here

(defun rdxmk-make-qemu ()
  "Go up to the root makefile of the redox project, run make qemu."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (rdxmk-get-closest-pathname "Makefile") 0 -8) " qemu &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))

(defun rdxmk-make-all ()
  "Go up to the root makefile of the redox project, run make all."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (rdxmk-get-closest-pathname "Makefile") 0 -8) " all &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))


(defun rdxmk-make-narg ()
  "Go up to the root makefile of the redox project, run make without arguments."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (rdxmk-get-closest-pathname "Makefile") 0 -8) " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))


(defun rdxmk-make-warg (arg)
  "Go up to the root makefile of the redox project, run make with the given argument, ARG."
  (interactive "sType argument to pass to make: ")
  (shell-command
   (concat
    "make -C " (substring (rdxmk-get-closest-pathname "Makefile") 0 -8) " " arg " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))

(defun rdxmk-cookbook (package op)
  "Run rdxmk-cookbook.sh with the specified PACKAGE and OP(tion).
Options include the following:
dist
distclean
build
clean
fetch
unfetch
publish
unpublish
stage
unstage
tar
untar
update
version"
  (interactive "sPackage: \nsOption: ")
  (shell-command
   (concat
    (substring (rdxmk-get-closest-pathname "Makefile") 0 -8) "/rdxmk-cookbook/cook.sh " package " " op " &")
   (get-buffer-create "*Rdxmk-Cookbook Output*")
   (get-buffer "*Rdxmk-Cookbook Output*")))

(define-minor-mode rdxmk-redox-mode
  "Redox mode - adds a hook for working with redox projects."
  nil ;; redox-mode is must be set true to be on
  " redox-mode ";; shows redox-mode on the mode line
  nil) ;; redox-mode does not have a keymap
  
(defun rdxmk-redox-togg-cond ()
  "Toggle `rdxmk-redox-mode` on if a file being visited is under redox/."
  (if (string-match
       (regexp-quote "redox/")
       default-directory)
     (rdxmk-redox-mode 1)
    (rdxmk-redox-mode 0)))

(add-hook 'text-mode-hook 'rdxmk-redox-togg-cond)
(add-hook 'after-change-major-mode-hook 'rdxmk-redox-togg-cond)

;;;###autoload
(defgroup rdxmk nil
  "rdxmk's customization group"
  :group 'programming)

(defcustom rdxmk-lockfile-no-pollute nil
  "If non nil, stop Emacs from making lockfiles when `rdxmk-redox-mode-hook` is run and stop Emacs from making auto saves and backups, which all can mess up redox' build system."
  :group 'rdxmk
  :type 'boolean)

(defun rdxmk-reminder-message ()
  "If `create-lockfiles` is non-nil, remind the user to set it to nil.
Otherwise, returns nil."
  (if create-lockfiles
      (message "Please set the value create-lockfiles to nil so redox will build properly.")
    '()))

(defun rdxmk-depollute-cond ()
  "If `rdxmk-lockfile-no-pollute` is t, inhibit backups for the buffer and set `create-lockfiles` and `auto-save-default` to nil when `rdxmk-redox-mode` is run."
  ;; (if (rdxmk-lockfile-no-pollute)
  ;;     ((rdxmk-reminder-message)
  ;;      (setq auto-save-default nil)
  ;;      (set (make-local-variable 'backup-inhibited) t))
  ;;   '()))
  (if rdxmk-lockfile-no-pollute
      (progn
	(rdxmk-reminder-message)
	(setq auto-save-default nil)
	(set (make-local-variable 'backup-inhibited) t))
    '()))

(add-hook 'rdxmk-redox-mode 'rdxmk-depollute-cond)

(provide 'rdxmk)
;;; rdxmk.el ends here
