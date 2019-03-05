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

(when (memq window-system '(mac ns x))
  (toggle-scroll-bar nil)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

;;;; Mac OS X
(when (eq system-type 'darwin)
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx))


;;;; Theme
(load-theme 'wombat)

;;;; Indent
(setq indent-tabs-mode nil)
(setq sh-basic-offset 2)

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (require 'skk nil t)
  (package-refresh-contents)
  (package-install 'ddskk))
(setq default-input-method "japanese-skk")
(setq skk-kakutei-key (kbd "C-o"))

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'eww-browse-url))

(use-package org
  :config
  (setq org-agenda-files (list "~/org/" "~/org/diary/" "~/org/project/" "~/org/notes"))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w)" "NOTE(n)" "ALWAYS(a)" "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
  (setq org-capture-templates
	'(("t" "TODO" entry (file "~/org/todo.org")
	   "* TODO %?\n")
	  ("T" "期限付きTODO" entry (file "~/org/todo.org")
	   "* TODO %?\n  DEADLINE: %^t\n")
	  ("n" "メモ" entry (file "~/org/notes.org")
	   "* NOTE %?\n")
	  ("N" "期限付きメモ" entry (file "~/org/notes.org")
	   "* NOTE %?\n  SCHEDULED: %^t\n")
	  ("c" "日程" entry (file "~/org/calendar.org")
	   "* NOTE %?\n  SCHEDULED: %^t\n")))
  (setq org-log-done 'time)
  (setq org-clock-into-drawer t)
  (setq org-hide-leading-stars t)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "\C-c c") 'org-capture)
  (global-set-key (kbd "\C-c l") 'org-store-link)
  (global-set-key (kbd "\C-c b") 'org-iswitchb)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

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
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)	    ;; M-X を 絞り込み
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-config)

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
  (add-hook 'racket-repl-mode-hook      #'enable-paredit-mode)
  (setq racket-memory-limit 256))

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
  :ensure t)

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

(use-package buffer-expose
  :ensure t)

(use-package presentation
  :ensure t)

(use-package winner
  :ensure t
  :config
  (winner-mode t))

(use-package elscreen
  :ensure t
  :config
  (elscreen-start))

(use-package helm-elscreen
  :ensure t)

(use-package clocker
  :ensure t
  :config
  (clocker-mode 1)
  (setq clocker-keep-org-file-always-visible nil))

(use-package restart-emacs
  :ensure t)

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
   	      ("<S-up>" . org-clock-convenience-timestamp-up)
   	      ("<S-down>" . org-clock-convenience-timestamp-down)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-clock-convenience org-clock-today slack helm-elscreen elscreen-persist elscreen yaml-mode use-package undohist undo-tree review-mode racket-mode presentation paredit magit helm-projectile helm-mt helm-google helm-ghq helm-dash helm-ag ddskk buffer-expose))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
