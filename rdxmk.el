;;; rdxmk.el --- A small set of tools for redox development in Gnu Emacs

;;; Commentary:

;; rdxmk provides a few tools to make redox development easier in Emacs.
;; You can run make with no arguments (M-x make-narg RET), make with arguments
;; (M-x make-warg RET arg RET), run the built in make-qemu and make-all, or use cookbook
;; (M-x Cookbook RET package RET option RET.) Finally, you can stop
;; Emacs from inserting files all over your redox/ directory by
;; going to rdxmk's customization group with `M-x customize-group RET rdxmk RET`
;; and setting `lockfile-no-pollute` to t.

;;; Installation

;; Clone the repo in your ~/.emacs.d/, add
;;(add-to-list 'load-path "~/.emacs.d/rdxmk")
;;(load "rdxmk")
;; to your startup file, and install rdxmk.info to your root
;; dir file using the shell tool, `install-info`.
;; For faster load time, run `C-u M-x byte-recompile-directory`
;; and run it on tne ~/.emacs.d/rdxmk/ directory.

;;; Code:

;;(require 'cl) ;; Make sure to uncomment this line if you don't have it already
(push "~/.emacs.d/rdxmk/" Info-directory-list)
;; I honestly have no idea how the following function works. I've seen it on the emacs wiki and stackoverflow,
;; where people used it to have their compile find the root makefile
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) 
    (expand-file-name file
		      (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return nil))))

(defun make-qemu ()
  "Go up to the root makefile of the redox project, run make qemu."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " qemu &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 

(defun make-all ()
  "Go up to the root makefile of the redox project, run make all."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " all &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 


(defun make-narg ()
  "Go up to the root makefile of the redox project, run make without arguments."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 


(defun make-warg (arg)
  "Go up to the root makefile of the redox project, run make with the given argument, ARG."
  (interactive
   (list (read-string "Type argument to pass to make: ")) arg)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " " arg " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 

(defun cookbook (package op)
  "Run cookbook.sh with the specified package and option.
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
    (substring (get-closest-pathname "Makefile") 0 -8) "/cookbook/cook.sh " package " " op " &")
   (get-buffer-create "*Cookbook Output*")
   (get-buffer "*Cookbook Output*"))) 
  
(define-minor-mode redox-mode
  "Redox mode - adds a hook for working with redox projects."
  nil ;; redox-mode is must be set true to be on
  " redox-mode ";; shows redox-mode on the mode line
  nil) ;; redox-mode does not have a keymap
  
(defun redox-togg-cond ()
  "Toggles redox-mode on if a file being visited is under redox/."
  (if (string-match
       (regexp-quote "redox/")
       default-directory)
     (redox-mode 1) 
    (redox-mode 0)))

(add-hook 'text-mode-hook 'redox-togg-cond)
(add-hook 'after-change-major-mode-hook 'redox-togg-cond)

(defgroup rdxmk nil
  "rdxmk's customization group"
  :group 'programming)

(defcustom lockfile-no-pollute nil
  "If non nil, stop Emacs from making lockfiles when redox-mode hook is run and stop Emacs from making auto saves and backups, which all can mess up redox' build system." 
  :type '(boolean :tag "backup-no-pollute" lockfile-no-pollute)
  :group 'rdxmk)

(defun depollute-cond ()
  "If `lockfile-no-pollute` is t, inhibit backups for the buffer and set `create-lockfiles` and `auto-save-default` to nil when redox-mode is run"
  (if (lockfile-no-pollute)
      ((setq create-lockfiles nil)
       (setq auto-save-default nil)
       (set (make-local-variable 'backup-inhibited) t))
    ()))

(add-hook 'redox-mode 'depollute-cond)

(provide 'rdxmk)
;;; rdxmk ends here
