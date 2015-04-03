;; Packages
(setq package-list '(magit window-numbering paredit darcula-theme
			   elisp-slime-nav auto-complete
			   company company-ghci company-quickhelp
			   haskell-mode flycheck flycheck-haskell
			   ws-butler font-lock+ git-gutter-fringe
			   clojure-mode cider ac-cider))

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

(require 'recentf)
(recentf-mode 1)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))

(global-set-key "\C-xf" 'ido-recentf-open)


;; Appearance
(require 'hl-line)
(add-hook 'after-change-major-mode-hook 'hl-line-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(load-theme 'darcula t)
;(load-theme 'ample-flat t)
;(load-theme 'darkburn t)

(set-frame-font
 (if (eq system-type 'darwin)
     "Inconsolata 22"
     "Inconsolata 19"))

;; Haskell
(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell)
     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
     (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (add-hook 'haskell-mode-hook 'flycheck-mode)
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c M-j") 'haskell-session-change)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))


;; ELisp
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)


;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)


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
