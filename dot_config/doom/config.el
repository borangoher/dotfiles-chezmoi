;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Boran"
      user-mail-address "borangoher@proton.me")

(setq doom-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "IosevkaTerm Nerd Font" :size 14)
      doom-big-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 18)
      doom-symbol-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14)
      doom-serif-font (font-spec :family "IosevkaTerm Nerd Font Mono" :size 14))

(after! nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(setq doom-theme 'doom-tokyo-night)

(add-to-list 'default-frame-alist '(alpha . (95 . 85)))

(setq doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-lsp-icon t
      doom-modeline-enable-word-count t
      doom-modeline-major-mode-color-icon t)

(defun my/open-org-directory ()
  "Open org-directory."
  (interactive)
  (find-file org-directory))

(defun my/open-org-super-agenda ()
  "Open org-agenda with custom view."
  (interactive)
  (org-agenda nil "o"))

(setq +doom-dashboard-menu-sections
      '(("Recently opened files"
         :icon (nerd-icons-faicon "nf-fa-file_text" :face 'doom-dashboard-menu-title)
         :when (fboundp 'recentf-open-files)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action recentf-open-files)
        ("Open project"
         :icon (nerd-icons-octicon "nf-oct-repo" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action projectile-switch-project)
        ("Open org-directory"
         :icon (nerd-icons-octicon "nf-oct-organization" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action my/open-org-directory
         :key "SPC f O")
        ("Open org-super-agenda"
         :icon (nerd-icons-octicon "nf-oct-calendar" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :when (fboundp 'org-agenda)
         :action my/open-org-super-agenda
         :key "SPC o A o")
        ("Open private configuration"
         :icon (nerd-icons-octicon "nf-oct-gear" :face 'doom-dashboard-menu-title)
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/open-private-config)))

(map! :leader
      (:prefix ("f" . "file")
       :desc "Open org-directory" "O" (lambda () (interactive) (find-file org-directory))))

(setq kill-whole-line t
      delete-by-moving-to-trash t
      auto-save-default t
      make-backup-files t
      confirm-kill-emacs nil
      display-line-numbers-type 'relative
      which-key-idle-delay 0.2
      mac-command-modifier 'meta)

(pixel-scroll-precision-mode t)
(blink-cursor-mode t)
(setq-default abbrev-mode t)

(setq org-directory "~/Library/CloudStorage/ProtonDrive-borangoher@proton.me-folder/org/")
(add-hook! 'after-init-hook
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")))

(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-provide-todo-statistics t)

  (setq org-use-speed-commands
        (lambda ()
          (and (looking-at org-outline-regexp)
               (looking-back "^\**")))))

(add-hook! org-mode :append
           #'org-appear-mode
           #'visual-line-mode
           #'variable-pitch-mode)

(after! org-mode
  (custom-set-faces!
    `((org-document-title)
      :foreground ,(face-attribute 'org-document-title :foreground)
      :height 1.3 :weight bold)
    `((org-level-1)
      :foreground ,(face-attribute 'outline-1 :foreground)
      :height 1.1 :weight medium)
    `((org-level-2)
      :foreground ,(face-attribute 'outline-2 :foreground)
      :weight medium)
    `((org-level-3)
      :foreground ,(face-attribute 'outline-3 :foreground)
      :weight medium)
    `((org-level-4)
      :foreground ,(face-attribute 'outline-4 :foreground)
      :weight medium)
    `((org-level-5)
      :foreground ,(face-attribute 'outline-5 :foreground)
      :weight medium)))

(after! org
  (setq org-capture-templates
        `(("n" "Note" entry
           (file ,(concat org-directory "/misc.org"))
           "* %^{Heading}\n%^{Explanation}"
           :empty-lines 1)

          ("i" "Inbox" entry
           (file+headline ,(concat org-directory "/inbox.org") "Inbox Items")
           "** TODO %^{Task description}"
           :empty-lines 0))))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Appearance
   org-modern-radio-target    '("❰" t "❱")
   org-modern-internal-target '("↪ " t "")
   org-modern-todo nil
   org-modern-tag nil
   org-modern-timestamp nil
   org-modern-statistics nil
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule "──────────"
   org-modern-hide-stars "·"
   org-modern-star ["⁖"]
   org-modern-keyword "‣"
   org-modern-list '((43 . "•")
                     (45 . "–")
                     (42 . "↪")))
  (custom-set-faces!
    `((org-modern-tag)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg) 0.1)
      :foreground ,(doom-color 'grey))
    `((org-modern-radio-target org-modern-internal-target)
      :inherit 'default :foreground ,(doom-color 'blue)))
  )

(use-package! org-agenda
  :config
  ;; Setting the TODO keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ;What needs to be done
           "NEXT(n)"                    ;A project without NEXTs is stuck
           "|"
           "DONE(d)")
          (sequence
           "REPEAT(e)"                    ;Repeating tasks
           "|"
           "DONE")
          (sequence
           "HOLD(h)"                    ;Task is on hold because of me
           "PROJ(p)"                    ;Contains sub-tasks
           "WAIT(w)"                    ;Tasks delegated to others
           "|"
           "CANCEL(c)"                    ;Stopped/cancelled
           ))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("REVIEW" . +org-todo-onhold)
          ("HOLD" . +org-todo-cancel)
          ("PROJ" . +org-todo-project)
          ("DONE"   . +org-todo-cancel)))
  ;; Appearance
  (setq org-agenda-span 10
        org-agenda-prefix-format       " %i %?-2 t%s"
        org-agenda-todo-keyword-format "%-6s"
        org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
        org-agenda-time-grid '((today require-timed remove-match)
                               (0900 1200 1400 1700 2100)
                               "      "
                               "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        )

  (setq org-clock-persist 'history
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
        org-agenda-start-with-log-mode t)
  (org-clock-persistence-insinuate))

(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; Enable org-super-agenda
  (org-super-agenda-mode)
  (setq org-agenda-block-separator ?―)
  ;; Customise the agenda view
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "")
            (tags-todo "inbox"
                       ((org-agenda-overriding-header "Inbox Items")
                        (org-super-agenda-groups
                         '((:auto-map hp/agenda-auto-group-title-olp)))))
            (todo "NEXT"
                  ((org-agenda-overriding-header
                    "Up Next")
                   (org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            (todo "TODO|HOLD|WAIT"
                  ((org-agenda-overriding-header
                    "Other Tasks")
                   (org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            ))))


  (defun hp/agenda-auto-group-title-olp (item)
    (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                             (get-text-property 0 'org-hd-marker item)))
                 (buffer (->> marker marker-buffer ))
                 (title (cadar (org-collect-keywords '("title"))))
                 (filledtitle (if (> (length title) 70)
                                  (concat (substring title 0 70)  "...") title))
                 (tags (org-get-tags))
                 (olp (org-super-agenda--when-with-marker-buffer
                          (org-super-agenda--get-marker item)
                        (s-join " → " (org-get-outline-path)))))
      (concat (if (not (member "journal" tags))
                  (concat "「" filledtitle "」" ) "    ") olp)))

  (after! evil-org-agenda
    (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))

  (custom-set-faces!
    `(org-todo
      :weight bold :foreground ,(doom-color 'blue))
    `(+org-todo-active
      :weight bold :foreground ,(doom-color 'green))
    `(org-super-agenda-header
      :inherit 'variable-pitch
      :weight bold :foreground ,(doom-color 'cyan))
    `(org-agenda-structure
      :inherit 'variable-pitch
      :weight bold :foreground ,(doom-color 'blue))))

(use-package! org-archive
  :after org
  :config
  (setq org-archive-location "archive.org::datetree/"))

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(use-package! mixed-pitch
  :hook ((org-mode      . mixed-pitch-mode)
         (org-roam-mode . mixed-pitch-mode)
         (LaTeX-mode    . mixed-pitch-mode))
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'warning
            'org-drawer 'org-cite-key 'org-list-dt 'org-hide
            'corfu-default 'font-latex-math-face)
  (setq mixed-pitch-set-height t))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package! toc-org
  :after org
  :hook (org-mode . toc-org-mode))

(use-package! evil
  :init
  (setq evil-move-beyond-eol t
        evil-move-cursor-back nil))

(use-package! evil-escape
  :config
  (setq evil-esc-delay 0.25))

(use-package! evil-vimish-fold
  :config
  (global-evil-vimish-fold-mode))

(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-change t
        evil-goggles-enable-delete t
        evil-goggles-pulse         t
        evil-goggles-duration      0.25)
  :config
  (custom-set-faces!
    `((evil-goggles-yank-face evil-goggles-surround-face)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-paste-face
      :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-delete-face
      :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-change-face
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-commentary-face
      :background ,(doom-blend (doom-color 'grey) (doom-color 'bg-alt) 0.5)
      :extend t)
    `((evil-goggles-indent-face evil-goggles-join-face evil-goggles-shift-face)
      :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg-alt) 0.25)
      :extend t)
    ))

(after! projectile
  (setq +workspaces-on-switch-project-behavior t)

  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

(use-package! treemacs
  :commands treemacs
  :init
  (map! :leader
        (:prefix ("f" . "file")
         :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
  (treemacs-git-mode 'extended)
  (setq treemacs-is-never-other-window nil)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package! elfeed
  :commands elfeed
  :init
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Open elfeed" "e" #'=rss)))

(after! elfeed
  (map! :map elfeed-search-mode-map
        :localleader
        :desc "Elfeed update" "r" #'elfeed-update))

(use-package! elfeed-web
  :defer t
  :commands elfeed-web-stop)

(setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))

(setq mu4e-maildir "~/.mail"
      mu4e-attachment-dir "~/Downloads")

(setq mu4e-get-mail-command "mbsync protonmail"
      mu4e-change-filenames-when-moving t
      mu4e-update-interval 120)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 1025)

(add-load-path! "/opt/homebrew/share/emacs/site-lisp/mu4e")
(after! gnutls
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge-v3/cert.pem")))
