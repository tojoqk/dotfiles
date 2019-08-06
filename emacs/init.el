;;;; Package initialization
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package mozc
  :config
  (setq default-input-method "japanese-mozc"))

(global-set-key (kbd "C-c e c")
                (lambda ()
                  (interactive)
                  (find-file "~/git/dotfiles/emacs/init.el")))

;; Backup files
(setq backup-directory-alist '(("." . "~/tmp/emacs/backups")))
(setq backup-by-copying t)

;; Appearance
(setq inhibit-startup-screen t)
(display-battery-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(setq display-time-default-load-average nil)
(display-time-mode t)

;;;; Indent
(setq-default indent-tabs-mode nil)
(setq sh-basic-offset 2)

;; Browser
(setq browse-url-new-window-flag t)

(use-package paredit
  :ensure t
  :config 
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package helm
  :ensure t
  :init
  (use-package helm-config)
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

(use-package magit
  :ensure t)

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-repl-mode-hook      #'enable-paredit-mode))

(use-package yaml-mode
  :ensure t)

(use-package review-mode
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package helm-ag
  :ensure t)

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package helm-ghq
  :ensure t)

(use-package helm-google
  :ensure t
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package helm-dash
  :ensure t
  :config
  (global-set-key (kbd "C-c d h") 'helm-dash)
  (global-set-key (kbd "C-c d a") 'helm-dash-activate-docset)
  (global-set-key (kbd "C-c d d") 'helm-dash-deactivate-docset)
  (global-set-key (kbd "C-c d i") 'helm-dash-install-docset)
  (global-set-key (kbd "C-c d u") 'helm-dash-install-user-docset))

(use-package helm-mt
  :ensure t)

(use-package restart-emacs
  :ensure t
  :config
  (global-set-key (kbd "C-c e r") 'restart-emacs))

(use-package multi-term
  :config
  (term-line-mode)
  (global-set-key (kbd "C-c t t")
                  (lambda ()
                    (interactive)
                    (if (get-buffer "*terminal<1>*")
                        (switch-to-buffer "*terminal<1>*")
                      (multi-term))))
  (global-set-key (kbd "C-c t c")
                  (lambda ()
                    (interactive)
                    (multi-term)))
  (global-set-key (kbd "C-c t n") 'multi-term-next)
  (global-set-key (kbd "C-c t p") 'multi-term-prev))

(use-package image+
  :ensure t)

(use-package helm-eww
  :ensure t)

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

(use-package markdown-mode+
  :ensure t)

(use-package exwm
  :ensure t
  :init
  (use-package exwm-edit
    :ensure t
    :config
    (defun qk-exwm/on-exwm-edit-compose ()
      (funcall 'markdown-mode))
    (add-hook 'exwm-edit-compose-hook 'qk-exwm/on-exwm-edit-compose))
  :config
  (require 'exwm-config)
  (defun exwm-rename-buffer ()
    (interactive)
    (let ((new-buffer-name
           (cond
            ((string-match-p "Toggl" (or exwm-title ""))
             (concat exwm-class-name ": Toggl"))
            (t
             (concat exwm-class-name ": " (or exwm-title ""))))))
      (when (not (string-equal (or (buffer-name) "") new-buffer-name))
        (exwm-workspace-rename-buffer new-buffer-name))))
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
		      (interactive)
		      (start-process "" nil "/usr/bin/slock")))
          ([s-escape] . (lambda ()
                          (interactive)
                          (start-process "" nil "/usr/bin/systemctl" "suspend")))))

  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])
          ;; escape
          ([?\C-g] . [escape])
          ))

  (setq exwm-workspace-number 4)
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
                         (string= exwm-class-name "Sakura"))
                (exwm-input-set-local-simulation-keys nil))))
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable))

(use-package editorconfig
  :ensure t)

(use-package terraform-mode
  :ensure t
  :config
  (terraform-format-on-save-mode t))

(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

(use-package erc-twitch
  :ensure t
  :config
  (erc-twitch-enable))
