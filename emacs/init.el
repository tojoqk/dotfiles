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

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(when (memq window-system '(mac ns x))
  (toggle-scroll-bar nil)
  (tool-bar-mode -1)
  (defun set-alpha (alpha-num)
    "set frame parameter 'alpha"
    (interactive "nAlpha: ")
    (set-frame-parameter nil 'alpha (cons alpha-num '(90))))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

(setq inhibit-startup-screen t)
(global-set-key (kbd "C-c e c")
                (lambda ()
                  (interactive)
                  (find-file "~/.ghq/github.com/tojoqk/dotfiles/emacs/init.el")))

;;;; Mac OS X
(when (eq system-type 'darwin)
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx))

(when (string= (system-name) "tojo.local")
  (setq make-backup-files nil)
  (setq auto-save-default nil))

;;;; Indent
(setq indent-tabs-mode nil)
(setq sh-basic-offset 2)

;; (unless (require 'skk nil t)
;;   (package-refresh-contents)
;;   (package-install 'ddskk))
;; (setq default-input-method "japanese-skk")
;; (setq skk-kakutei-key (kbd "C-o"))

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'eww-browse-url))

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

(use-package buffer-expose
  :ensure t)

(use-package presentation
  :ensure t)

(use-package winner
  :ensure t
  :config
  (winner-mode t))

;; (use-package elscreen
;;   :ensure t
;;   :config
;;   (elscreen-start))

;; (use-package helm-elscreen
;;   :ensure t)

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

;; (use-package slack
;;   :ensure t)

(use-package image+
  :ensure t)

(use-package helm-eww
  :ensure t)

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))
