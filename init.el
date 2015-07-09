;; Packages
(setq package-list '(magit window-numbering paredit
			   elisp-slime-nav auto-complete
			   company company-ghci company-quickhelp
			   haskell-mode hi2 flycheck flycheck-haskell
			   ws-butler font-lock+ git-gutter-fringe
			   clojure-mode cider ac-cider rainbow-delimiters
			   projectile ido-ubiquitous ace-jump-mode
			   zenburn-theme darcula-theme seti-theme leuven-theme
			   dockerfile-mode idris-mode))

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-enable-at-startup nil)
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
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-o") 'ace-jump-mode)

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

;(load-theme 'leuven t)
;(load-theme 'seti t)
(load-theme 'zenburn t)
;(load-theme 'dichromacy t)
;(load-theme 'darcula t)
;(load-theme 'ample-flat t)
;(load-theme 'darkburn t)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun my/windows-appearance ()
  (set-frame-font "Consolas 20")
  (set-face-font 'mode-line "Consolas 15")
  (set-face-font 'mode-line-inactive "Consolas 14"))

(defun my/default-appearance ()
  (set-frame-font "Ubuntu Mono 23")
  (set-face-font 'mode-line "droid sans mono 15")
  (set-face-font 'mode-line-inactive "droid sans mono 14"))

(defun my/osx-appearance ()
  (set-frame-font "Source Code Pro Light 24")
  (set-face-font 'mode-line "Inconsolata 19")
  (set-face-font 'mode-line-inactive "Inconsolata 18"))

(defun my/initial-appearance ()
  (interactive)
  (pcase system-type
    (`darwin     (my/osx-appearance))
    (`windows-nt (my/windows-appearance))
    (_           (my/default-appearance)))
  (toggle-fullscreen))

(my/initial-appearance)

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

     (defun my/haskell-insert-type ()
       (interactive)
       (haskell-process-do-type t))

     (defun define-haskell-key (key f)
       (define-key haskell-mode-map (kbd key) f))

     (define-haskell-key "C-c C-o" 'haskell-organize-imports)
     (define-haskell-key "C-," 'haskell-move-nested-left)
     (define-haskell-key "C-." 'haskell-move-nested-right)
     (define-haskell-key "C-c M-j" 'haskell-session-change)
     (define-haskell-key "C-c C-c" 'haskell-compile)
     ;; Infer the type of the thing at point.
     (define-haskell-key "C-c C-t" 'haskell-process-do-type)
     ;; Display info (in the REPL) about the thing at point.
     (define-haskell-key "C-c C-i" 'haskell-process-do-info)
     ;; Insert the inferred type of the function at point into the code.
     (define-haskell-key "C-c C-s" 'my/haskell-insert-type)
     ;; Run `cabal test' in a compile buffer.
     (define-haskell-key "C-c C-," 'ohai-haskell/run-test-suite)))

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

;; idris
(eval-after-load "idris-mode"
  '(progn
     (define-key idris-mode-map (kbd "C-c ,") 'idris-compile-and-execute)))

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "clojure-mode"
  '(progn
     (require 'ac-cider)
     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
     (add-hook 'cider-mode-hook 'ac-cider-setup)
     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (eval-after-load "auto-complete"
       '(progn
	  (add-to-list 'ac-modes 'cider-mode)
	  (add-to-list 'ac-modes 'cider-repl-mode)))

     (put-clojure-indent 'go-trace 0)


     (dolist (m '(facts fact match while-let go-loop-trace go-while-let alt!))
       (put-clojure-indent m 1))))

;; Metascript
(add-hook 'metascript-mode-hook #'flymake-mode)

(eval-after-load "metascript-mode"
  '(progn
     (define-key metascript-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)))

;; Magit
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)


;; Custom Keybindings
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

