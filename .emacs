(package-initialize)
(require 'package)

;(add-to-list 'load-path "~/.emacs.d")

; enable melpa
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

; enable marmalade
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(require 'iedit)

; auto-reload changed outside files
(global-auto-revert-mode t)

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
	  (lambda()
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

;Unless you turn this variable on you will be prompted every time you want to delete a buffer, even unmodified ones
(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

(defun all-modes-hook()
    (auto-complete-mode)
    ; consider _ and - as part of the word (while selection)
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
)

(add-hook 'prog-mode-hook
    (lambda()
        (all-modes-hook)
        (linum-mode)
        (setq show-trailing-whitespace t)))

(add-hook 'text-mode-hook
    (lambda()
       (all-modes-hook)))

; highlight matching parenthesis
(setq show-paren-style 'parenthesis)
(show-paren-mode 1)

; switch window on M-arrow
(windmove-default-keybindings 'meta)

; save session on quit
(desktop-save-mode t)

; highlight current line
;(global-hl-line-mode +1)

(setq linum-format "%d ")

;(setq-default show-trailing-whitespace t)

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

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

; better heuristics and highlights for flex matching for ido
(flx-ido-mode 1)
; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;(setq flx-ido-threshold 10000)

(global-set-key (kbd "C-k") 'kill-buffer-and-window)

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
(setq ac-disable-faces nil) ; enable completion inside strings
(setq ac-auto-show-menu nil)
;(defun ac-common-setup ()
;  (setq ac-sources (append ac-sources '(ac-source-filename))))

; scroll up/down to bounds
(setq scroll-error-top-bottom t)

(setq-default cursor-type 'bar)

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
 '(ac-auto-show-menu t)
 '(ag-arguments (quote ("-C 1" "--smart-case" "--stats")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-light)))
 '(ibuffer-projectile-prefix "")
 '(package-selected-packages
   (quote
    (undo-tree bm ido-vertical-mode iedit flx-ido smex projectile sr-speedbar projectile-speedbar ibuffer-sidebar ibuffer-projectile auto-complete ag)))
 '(projectile-git-command
   "find . -type f -not -path '*/.git/*' -not -path '*/.hg/*' -not -path '*.pyc' -print0")
 '(projectile-verbose t)
 '(show-paren-mode t)
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

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (replace-string "_" " " nil start end)
            (upcase-initials-region start end)
            (replace-string " " "" nil start end)
            (downcase-region start (1+ start))
          )
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))


(global-set-key (kbd "<f9>") 'toggle-camelcase-underscores)

(add-hook
     'c++-mode-hook
      (lambda ()
      (local-set-key (kbd "C-d") #'duplicate-line)))

(global-set-key (kbd "C-d") 'duplicate-line)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

; bookmarks

(setq bm-restore-repository-on-load t)
(require 'bm)
;; Включаем/выключаем закладку - Alt+F5.
(global-set-key (kbd "<M-f5>") 'bm-toggle)
;; Переход на следующую закладку - F5.
(global-set-key (kbd "<f5>")   'bm-next)
;; Переход на предыдущую закладку - Shift+F5.
(global-set-key (kbd "<S-f5>") 'bm-previous)

;; Сохраняем закладки между сессиями.
(setq-default bm-buffer-persistence t)

;; Визуально закладки помечаются как маркер во fringe.
;(setq-default bm-highlight-style 'bm-highlight-only-fringe)

(setq bm-cycle-all-buffers t)
(setq-default bm-highlight-style 'bm-highlight-line-and-fringe)

;; При старте загружаем репозиторий закладок.
(add-hook' after-init-hook 'bm-repository-load)

;; Восстанавливаем закладки для буфера при открытии файла.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Сохраняем букмарки при закрытии буфера.
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Сохраняем репозиторий при выходе.
;; kill-buffer-hook не вызывается при выходе из emacs,
;; поэтому сперва сохраняем все закладки.
(add-hook 'kill-emacs-hook
    (lambda()
        (bm-buffer-save-all)
        (bm-repository-save)))

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)

(add-hook 'emacs-lisp-mode-hook
    (lambda() (prettify-symbols-mode)))
