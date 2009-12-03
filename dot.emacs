(require 'cl)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/utilities/ert")
(add-to-list 'load-path "~/.emacs.d/utilities/jump")

(load-file "~/.emacs.d/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))

(vendor 'haskell-mode)
(vendor 'textmate)
(vendor 'configgy-mode)
(vendor 'magit)

(mapcar 'load-directory '("~/.emacs.d/customizations"))


                                       