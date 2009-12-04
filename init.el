(require 'cl)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/utilities/ert")
(add-to-list 'load-path "~/.emacs.d/utilities/jump")
(add-to-list 'load-path "~/.emacs.d/vendor")

(load-file "~/.emacs.d/load-directory.el")
(mapcar 'load-directory '("~/.emacs.d/utilities"))

(vendor 'haskell-mode)
(vendor 'textmate)
(vendor 'configgy-mode)
(vendor 'magit)
(vendor 'clojure-mode)
(vendor 'incanter)
(vendor 'scala-mode)
(vendor 'thrift-mode)
(vendor 'sml-mode)
(vendor 'ruby-mode)
(vendor 'rinari)
(vendor 'nxml-mode)
(vendor 'textile-mode)

;; This must be loaded last due to dependencies
(mapcar 'load-directory '("~/.emacs.d/customizations"))
