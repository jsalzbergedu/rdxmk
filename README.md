# rdxmk
A small set of tools for redox development in Gnu Emacs.
# Installation

Clone this repo in your .emacs.d, then add to your .emacs:
```elisp
(add-to-list 'load-path "~/.emacs.d/rdxmk/")
(require 'rdxmk)
(rdxmk-global-mode t)
```
Install the documentation with 
```bash
install-info ~/.emacs.d/rdxmk/rdxmk.info /usr/share/info/dir
```

For more documentation, see the info entry with `C-h i` under rdxmk.

Happy redoxing!
