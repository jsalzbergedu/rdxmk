;;(require 'cl) ;; Make sure to uncomment this line if you don't have it already

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
  "Goes up to the root makefile of the redox project, runs make qemu."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " qemu &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 

(defun make-all ()
  "Goes up to the root makefile of the redox project, runs make all."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " all &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 


(defun make-narg ()
  "Goes up to the root makefile of the redox project, runs make without arguments."
  (interactive)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 


(defun make-warg (arg)
  "Goes up to the root makefile of the redox project, runs make with the given argument."
  (interactive
   (list (read-string "Type argument to pass to make: ")) arg)
  (shell-command
   (concat
    "make -C " (substring (get-closest-pathname "Makefile") 0 -8) " " arg " &")
   (get-buffer-create "*Redox Build Output*")
   (get-buffer "*Redox Build Output*"))) 

(defun cookbook (package op)
  "Runs cookbook.sh with the specified package and option.
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
  version
"
  (interactive "sPackage: \nsOption: ")
  (shell-command
   (concat
    (substring (get-closest-pathname "Makefile") 0 -8) "/cookbook/cook.sh " package " " op " &")
   (get-buffer-create "*Cookbook Output*")
   (get-buffer "*Cookbook Output*"))) 
  
(define-minor-mode redox-mode
  "Redox mode - adds a hook for working with redox projects."
  nil ;; redox-mode is must be set true to be on
  nil ;; redox-mode does not display anything in the mode line
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

(defgroup rdxmk nil "rdxmk's customization group")

(defcustom lockfile-no-pollute nil
  "If non nil, stops emacs from making lockfiles which can mess up redox' build system." 
  :type '(boolean :tag "backup-no-pollute" lockfile-no-pollute)
  :group 'rdxmk)

(defun depollute-cond ()
  "If lockfile-no-pollute is t, sets create-lockfiles to nil when redox-mode is run"
  (if (lockfile-no-pollute)
      (setq create-lockfiles nil)
    ()))

(add-hook 'redox-mode 'depollute-cond)
