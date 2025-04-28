;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; user info
(setq user-full-name "Boran"
      user-mail-address "borangoher@proton.me")

;; fonts
(setq doom-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "IosevkaTerm Nerd Font" :size 14)
      doom-big-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 18)
      doom-symbol-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14)
      doom-serif-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14))

;; theme
(setq doom-theme 'doom-tokyo-night)

;; org-mode
(setq org-directory "~/org/")

(after! org (setq org-hide-emphasis-markers t))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

(after! org
  (setq org-use-speed-commands
        (lambda ()
          (and (looking-at org-outline-regexp)
               (looking-back "^\**")))))

(add-hook! org-mode :append #'org-appear-mode)

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-dir-name t)))
  (org-super-agenda-mode))

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(use-package! ox-awesomecv
  :after org
  :config
  (defun org-awesomecv--cventry-right-img-code (file)
    (if file
        (format "\\begin{wrapfigure}{r}{0.15\\textwidth}
  \\raggedleft\\vspace{-10.0mm}
  \\includegraphics[width=0.1\\textwidth]{%s}
\\end{wrapfigure}" file) "")))
(use-package! ox-moderncv
  :after org)

(defun zz/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; other stuff
(setq kill-whole-line t)
(setq auto-save-default t
      make-backup-files t)
(setq confirm-kill-emacs nil)
(setq doom-modeline-enable-word-count t)
(setq display-line-numbers-type 'relative)
