# rdxmk
A small set of tools for redox development in Gnu Emacs.
# Installation
Clone this repo in your .emacs.d, then run

```bash
cd ~/.emacs.d/rdxmk/
./into_package.sh
```

Then run `M-x package-install-file RET ~/.emacs.d/rdxmk-0.10.tar RET`

Finally in your .emacs add

```elisp
(require 'rdxmk)
```

For more documentation, see the info entry with `C-h i` under rdxmk.

Happy redoxing!
