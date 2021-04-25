;; -*- no-byte-compile: t; -*-
;;; private/rime/packages.el

(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
