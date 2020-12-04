;;; private/prog/bindings.el -*- lexical-binding: t; -*-


(map!
 (:after company
  (:map (company-active-map evil-insert-state-map)
   "C-o" #'smarter-yas-expand-next-field-complete)))
