
(package-initialize)
(require 'package)

;(add-to-list 'load-path "~/.emacs.d")

; enable melpa
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

; enable marmalade
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(unless (package-installed-p 'projectile) (package-install 'projectile))
(unless (package-installed-p 'flx-ido) (package-install 'flx-ido))
(unless (package-installed-p 'iedit) (package-install 'iedit))

(require 'iedit)

; do not display the splash screen
(setq inhibit-startup-screen t)

;http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html

(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
;((nil . ((projectile-git-command . "/path/to/other/git ls-files -zco --exclude-standard"))))

; replace BufferMenu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(autoload 'ibuffer "ibuffer" "List buffers." t)

; ibuffer-auto-mode is a minor mode that automatically keeps the buffer list up to date
(add-hook 'ibuffer-mode-hook
	  (lambda ()
                (ibuffer-auto-mode 1)
                (setq ibuffer-filter-groups
                    (append
                         (quote (
                             ("dired" (mode . dired-mode))
                             ("emacs" (or
                                 (name . "^\\*scratch\\*$")
                                 (name . "^\\*Messages\\*$")
                                 (name . "^.emacs$")))))
                          (ibuffer-projectile-generate-filter-groups)))

                (setq ibuffer-hidden-filter-groups nil)

                (ibuffer-update nil t)
))

; consider _ and - as part of the word (while selection)
(add-hook 'after-change-major-mode-hook
    (lambda ()
        (modify-syntax-entry ?_ "w")
        (modify-syntax-entry ?- "w")))

;Unless you turn this variable on you will be prompted every time you want to delete a buffer, even unmodified ones
(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'prog-mode-hook
    (lambda()
        (auto-complete-mode)
        (limum-mode)))

(add-hook 'text-mode-hook 'auto-complete-mode)

; highlight matching parenthesis
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

; switch window on M-arrow
(windmove-default-keybindings 'meta)

; save session on quit
(desktop-save-mode t)

; highlight current line
;(global-hl-line-mode +1)

; show line numbers
(setq linum-format "%d ")
;(global-linum-mode 1)

(setq-default show-trailing-whitespace t)

; indentation
; disable auto indentation
(electric-indent-mode -1)

(setq default-tab-width 4)
(setq c-basic-offset 4)

; do not use tabs for indentation
(setq-default indent-tabs-mode nil)

; y instead of yes
(fset 'yes-or-no-p 'y-or-n-p)

; dont ask to kill a buffer with a live process attached to it
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq confirm-nonexistent-file-or-buffer nil)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

; better heuristics and highlights for flex matching for ido
(flx-ido-mode 1)
; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;(setq flx-ido-threshold 10000)

(require 'recentf)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; auto-complete
(ac-config-default) ; enable auto-complete
(setq ac-disable-faces nil) ; enable comletion inside strings
;(defun ac-common-setup ()
;  (setq ac-sources (append ac-sources '(ac-source-filename))))

; ag setup
(setq ag-highlight-search t)
;(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

(setq make-backup-files        nil) ; disable backup files
(setq auto-save-list-file-name nil) ; disable .saves files
(setq auto-save-default        nil) ; disable auto saving

; hide details in dired by defalt
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))

;(global-set-key [(double-mouse-1)] 'highlight-symbol-at-point);

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-b") 'bookmark-set)
(global-set-key (kbd "<f2>") 'bookmark-bmenu-list)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

; smex - ido for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(projectile-mode +1)
(setq projectile-enable-caching t)
(setq projectile-track-known-projects-automatically nil)
(define-key projectile-mode-map (kbd "C-c") 'projectile-command-map)
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

(defun toggle-highlight-at-point()
    (interactive)
    (let*(
;              (word-at-point (thing-at-point 'word 'no-propertie))
;              (hi-lockified-word-at-point (concat "\\_<" word-at-point "\\_>"))
          (hi-lockified-word-at-point (find-tag-default-as-symbol-regexp))
          (hi-lock-flattened-patterns (mapcar 'car hi-lock-interactive-patterns))
        )
        (if (member hi-lockified-word-at-point hi-lock-flattened-patterns)
          (unhighlight-regexp hi-lockified-word-at-point)
          (highlight-symbol-at-point))
    )
)

(global-set-key (kbd "S-<f8>") 'toggle-highlight-at-point)

; add timestamps to *Messages*

(defun with-timestamp (args)
    (push
        (concat
            (format-time-string "%T.%3N ")
            (car args))
        (cdr args))
)
(advice-add 'message :filter-args 'with-timestamp)

; do not bother me at start
(setq hi-lock-file-patterns-policy (lambda (pattern) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("-C 1" "--smart-case" "--stats")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(ibuffer-projectile-prefix "")
 '(package-selected-packages
   (quote
    (iedit flx-ido smex projectile-speedbar magit ibuffer-sidebar ibuffer-projectile ibuffer-git auto-complete ag)))
 '(package-selected-packgaes
   (quote
    (magit ibuffer-git ibffuer-projectile ibuffer-sidebar projectile projectile-speedbar sr-speedbar ag auto-complete)))
 '(projectile-git-command
   "find . -type f -not -path '*/.git/*' -not -path '*/.hg/*' -not -path '*.pyc' -print0")
 '(projectile-verbose t)
 '(sr-speedbar-auto-refresh t)
 '(sr-speedbar-delete-windows t)
 '(sr-speedbar-right-side nil)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(iedit-occurrence ((t (:inherit highlight :background "gold" :foreground "black")))))

