;; Packages
(setq package-list '(magit window-numbering paredit
			   elisp-slime-nav auto-complete
			   company company-ghci company-quickhelp
			   haskell-mode hi2 flycheck flycheck-haskell
			   ws-butler font-lock+ git-gutter-fringe
			   clojure-mode cider ac-cider rainbow-delimiters
			   projectile ido-ubiquitous
			   atom-dark-theme ample-zen-theme
			   zenburn-them darcula-theme
			   dockerfile-mode))

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


; fetch list of packages
(unless package-archive-contents
  (package-refresh-contents))

; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)


;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Modes
(require 'window-numbering)
(window-numbering-mode)

(require 'font-lock)
(require 'font-lock+)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

(require 'ws-butler)
(ws-butler-global-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-change-major-mode-hook 'company-quickhelp-mode)

(require 'projectile)
(projectile-global-mode)
(add-to-list 'projectile-globally-ignored-file-suffixes "~")
(global-set-key "\C-j" 'projectile-find-file)

(require 'recentf)
(recentf-mode 1)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key "\C-xf" 'ido-recentf-open)
(global-set-key "\C-x\C-f" 'ido-find-file)


;; Appearance
(require 'hl-line)
(add-hook 'after-change-major-mode-hook 'hl-line-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

; a curious mix
(load-theme 'atom-dark t)
(load-theme 'ample-zen t)

;(load-theme 'zenburn t)
;(load-theme 'dichromacy t)
;(load-theme 'darcula t)
;(load-theme 'ample-flat t)
;(load-theme 'darkburn t)

(set-frame-font
 (if (eq system-type 'darwin)
     "Inconsolata 22"
   "Ubuntu Mono 19"))

;; Haskell
(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell)
     (require 'company-ghci)

     (require 'hi2)
     (add-hook 'haskell-mode-hook 'turn-on-hi2)
     (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
     (add-hook 'haskell-mode-hook 'show-paren-mode)
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (add-hook 'haskell-mode-hook 'flycheck-mode)

     (defun haskell-organize-imports ()
       (interactive)
       (haskell-sort-imports)
       (haskell-align-imports))

     (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-organize-imports)
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c M-j") 'haskell-session-change)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
     ;; Infer the type of the thing at point.
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     ;; Display info (in the REPL) about the thing at point.
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     ;; Insert the inferred type of the function at point into the code.
     (define-key haskell-mode-map (kbd "C-c C-s") (lambda () (interactive) (haskell-process-do-type t)))
     ;; Run `cabal test' in a compile buffer.
     (define-key haskell-mode-map (kbd "C-c C-,") 'ohai-haskell/run-test-suite)))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Mined from https://github.com/bodil/ohai-emacs/blob/master/modules/ohai-haskell.el
;; bodil kills!
(defun ohai-haskell/run-test-suite ()
  (interactive)
  (require 'compile)
  (projectile-with-default-dir (projectile-project-root)
    (compile "cabal test")))

;; ELisp
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)


;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'facts 1)))


;; Magit
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)


;; Custom Keybindings
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(define-key global-map (kbd "<f11>") 'toggle-fullscreen)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C-+") 'text-scale-decrease)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(define-key global-map (kbd "M-=") 'toggle-selective-display)
(define-key global-map (kbd "M-\\") 'delete-horizontal-space)


;; File variables
(add-hook 'find-file-hook
	  (lambda () (set-variable 'show-trailing-whitespace t)))

;; Custom variables
(custom-theme-set-variables
 'user

 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output 'first-error)

 '(haskell-tags-on-save t)

 '(ido-completion-buffer-all-completions t)
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t))

