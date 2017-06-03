# rdxmk
A small set of tools for redox development in Gnu Emacs.
# Installation
Clone this repo in your .emacs.d, then add the following in your .emacs:

```elisp
(require 'cl) ;; If you don't have this
(add-to-list 'load-path "~/.emacs.d/rdxmk")
(load "rdxmk")
```
To install the docfiles, run
```bash
sudo install-info ~/.emacs.d/rdxmk/rdxmk.info /usr/share/info/dir
```

For faster load times, run `C-u M-x byte-recompile-directory RET ~/.emacs.d/rdxmk/ RET`

Happy redoxing!
