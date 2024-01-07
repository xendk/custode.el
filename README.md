

# Recommended configuration

## Colors

Most tools these days use colors, but Emacs compilation buffers
doesn't support colors per default, but since 28.1 it can be
configured to do so:

``` emacs-lisp
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
```

Or with use-package:

``` emacs-lisp
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 
```

Alternatively, there's a package that adds color support and a few
other nifty improvements to compilation buffers:
https://codeberg.org/ideasman42/emacs-fancy-compilation

## Doom-modeline

If you're using doom-modeline, you can add the following to your
configuration in order to show the Custode lighter in the modeline
without showing all minor modes:

``` emacs-lisp
(add-to-list 'global-mode-string custode-lighter)
```
