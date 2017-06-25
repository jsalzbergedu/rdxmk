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
Or if `/usr/share/info/dir` does not exist, another directory that shows up at the beginning
of the `Info-directory-list` when you describe it with `C-h v`.

For more documentation, see the info entry with `C-h i` under rdxmk.

Happy redoxing!
