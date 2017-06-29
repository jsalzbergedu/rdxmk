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

(defun rdxmk-get-closest-pathname (&optional file)
  "By default, RDXMK-GET-CLOSEST-PATHNAME will find the closest makefile. If FILE is specified, search for FILE in the same way."
  (locate-dominating-file (or buffer-file-name default-directory) (or file "Makefile")))

(defun rdxmk-shell-get-closest (&optional file)
  "RDXMK-SHELL-GET-CLOSEST will find the closest makefile, and run 'SHELL-QUOTE-ARGUMENT' on it.  If FILE is specified, search for FILE in the same way."
  (shell-quote-argument (rdxmk-get-closest-pathname file)))

(defun rdxmk-make-qemu ()
  "Go up to the root makefile of the redox project, run make qemu."
  (interactive)
  (shell-command
   (concat
    "make -C " (rdxmk-shell-get-closest) " qemu &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))

(defun rdxmk-make-all ()
  "Go up to the root makefile of the redox project, run make all."
  (interactive)
  (shell-command
   (concat
    "make -C " (rdxmk-shell-get-closest) " all &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))


(defun rdxmk-make-narg ()
  "Go up to the root makefile of the redox project, run make without arguments."
  (interactive)
  (shell-command
   (concat
    "make -C " (rdxmk-shell-get-closest) " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*")))


(defun rdxmk-make-warg (arg)
  "Go up to the root makefile of the redox project, run make with the given argument, ARG."
  (interactive "sType argument to pass to make: ")
  (shell-command
   (concat
    "make -C " (rdxmk-shell-get-closest) " " (shell-quote-argument arg) " &")
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
    (rdxmk-shell-get-closest) "/rdxmk-cookbook/cook.sh " package " " op " &")
   (get-buffer-create "*Rdxmk-Cookbook Output*")
   (get-buffer "*Rdxmk-Cookbook Output*")))

(define-minor-mode rdxmk-redox-mode
  "Redox mode - adds a hook for working with redox projects."
  nil ;; redox-mode is must be set true to be on
  " redox";; shows redox-mode on the mode line
  nil) ;; redox-mode does not have a keymap

(define-globalized-minor-mode rdxmk-global-mode ;; A mode to enable rdxmk-redox-mode
  rdxmk-redox-mode ;; the minor mode that rdxmk-global-mode turns on
  rdxmk-redox-togg-cond) ;; called on every buffer, will turn on rdxmk-redox-mode when
;; appropriate

(defun rdxmk-redox-togg-cond ()
  "Toggle `rdxmk-redox-mode` on if a file being visited is under redox/."
  (if (string-match
       (regexp-quote "redox/")
       default-directory)
      (rdxmk-redox-mode 1)
    (rdxmk-redox-mode 0)))

;;;###autoload
(defgroup rdxmk nil
  "Tools for redox development"
  :group 'programming)

(defcustom rdxmk-lockfile-no-pollute nil
  "If non nil, tells the user to turn off lockfiles when `rdxmk-redox-mode-hook` is run and to stop Emacs from making auto saves and backups, which all can mess up redox' build system."
  :group 'rdxmk
  :type 'boolean)

(defun rdxmk-reminder-message ()
  "If `create-lockfiles` is non-nil, remind the user to set it to nil.
Otherwise, returns nil."
  (if create-lockfiles
      (message "Please set the value create-lockfiles to nil so redox will build properly.")
    '()))

(defun rdxmk-depollute-cond ()
  "If `rdxmk-lockfile-no-pollute` is t, inhibit backups for the buffer, set `auto-save-default` to nil, and tell the user to set `create-lockfiles` to nil when `rdxmk-redox-mode` is run."
  (when rdxmk-lockfile-no-pollute
    (rdxmk-reminder-message)
    (set (make-local-variable 'auto-save-default) nil)
    (set (make-local-variable 'backup-inhibited) t)))

(add-hook 'rdxmk-redox-mode 'rdxmk-depollute-cond)

(provide 'rdxmk)
;;; rdxmk.el ends here
