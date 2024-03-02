;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Appearance
(setq doom-font "JetBrains Mono 10")
(setq doom-theme 'doom-monokai-classic)

;; Absolute line numbers.
(setq display-line-numbers-type t)

;; Start maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable only in text- and prog-modes.
(use-package! display-fill-column-indicator
  :defer t
  :init
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'text-mode-hook #'display-fill-column-indicator-mode))


;;; General settings

;; Display transient menu with isearch keybindings.
(use-package! cc-isearch-menu
  :defer t
  :bind
  (:map isearch-mode-map ("<f2>" . cc-isearch-menu-transient)))

;; Handle camelCase words properly everywhere.
(use-package! subword
  :diminish subword-mode
  :config
  (global-subword-mode 1))

;; Move files to trash when deleting.
(setq delete-by-moving-to-trash t)

(use-package! autorevert
  :diminish auto-revert-mode
  :config
  ;; Revert buffers automatically associated with files when the file changes
  ;; on disk.
  (global-auto-revert-mode 1)
  ;; Also auto refresh dired and be quiet.
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package! hippie-exp
  :bind
  (("M-/" . hippie-expand)
   ("C-M-/" . hippie-expand-lines))
  :config
  ;; Custom hippie-expand expansion functions.
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package! calendar
  :defer t
  :config
  ;; Start week at Monday.
  (setq calendar-week-start-day 1))

(use-package! browse-kill-ring
  :bind
  ("C-x C-y" . browse-kill-ring))

;; Undo/redo window configuration with C-c <left>/<right>.
(use-package! winner
  :config
  (winner-mode 1))

;; Buffers moving.
(use-package! buffer-move
  :bind
  (("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" . buf-move-right)))

;; Jump to things.
(use-package! avy
  :defer t
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g C" . avy-goto-char-2)
  ("M-g m" . avy-goto-char-in-line)
  ("M-g g" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g W" . avy-goto-word-0)
  ("M-g s" . avy-goto-subword-1))

(use-package! multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

;; Show number of search matches in mode line.
(use-package! anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

;; Visual query replace.
(use-package! visual-regexp
  :bind
  ("C-M-%" . vr/query-replace))

;; Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

;; Show the current function name in the header line only in prog modes.
(which-function-mode 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq header-line-format
                  '((which-func-mode ("" which-func-format " "))))))
(setq mode-line-misc-info
      ;; Remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(use-package! imenu
  :defer t
  :config
  ;; Always rescan buffer for imenu
  (setq-default imenu-auto-rescan t))

(use-package! fancy-narrow
  :init
  (fancy-narrow-mode 1)
  :config
  (setq fancy-narrow-lighter nil))

(use-package! evil-numbers
  :bind
  ;; Increase number at point.
  ("C-+" . evil-numbers/inc-at-pt))

;; Display major mode key bindings in popup menu.
(use-package discover-my-major
  :bind
  ("C-h C-m" . discover-my-major))

(use-package! google-this
  :defer t
  :diminish google-this-mode
  :init
  (progn
    (setq google-this-keybind (kbd "C-x g"))
    (google-this-mode 1)))


;;; Core key bindings.

;; Repeat last command.
(bind-key "C-z" #'repeat)             ; which used to be suspend-frame

;; Paragraph movement.
(bind-key "M-n" #'forward-paragraph)
(bind-key "M-p" #'backward-paragraph)

;; Activate occur inside isearch with C-o.
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Join line.
(bind-key "C-x j" #'join-line)
(bind-key "C-x J" (lambda (arg)
                    (interactive "p")
                    (join-line (- arg))))

;; Duplicate region or current line
(bind-key "C-c d" #'duplicate-dwim)

;;;###autoload
(defun unfill-paragraph ()
  "Unfill paragraph or region."
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))

(bind-key "M-Q" #'unfill-paragraph)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t"))      ; which used to be transpose-words
(bind-key "M-t c" #'transpose-chars)
(bind-key "M-t l" #'transpose-lines)
(bind-key "M-t w" #'transpose-words)
(bind-key "M-t s" #'transpose-sexps)
(bind-key "M-t p" #'transpose-params)

(bind-key "<f2>"
          (defhydra hydra-zoom ()
            "zoom"
            ("+" text-scale-increase "in")
            ("-" text-scale-decrease "out")
            ("0" (text-scale-adjust 0) "reset" :color blue)))

;;;###autoload
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Eval and replace anywhere.
(bind-key "C-x E" #'eval-and-replace)


;;; Major modes

;; Highlight changes made to files under vc.
(use-package! diff-hl
  :defer t
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode))
