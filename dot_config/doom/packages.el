;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

(package! org-super-agenda)

(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))

(package! org-auto-tangle)
