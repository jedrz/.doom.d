;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path doom-user-dir t)
(load "private.el")

;;; Appearance
(setq doom-font "JetBrains Mono 10")

;; Absolute line numbers.
(setq display-line-numbers-type t)

;; Start maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Display fill column.
(use-package! display-fill-column-indicator
  :defer t
  :hook
  prog-mode)

;;; General settings

;; Do not ask before killing Emacs.
(setq confirm-kill-emacs nil)

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

(after! auth-source
  (setq auth-source-save-behavior nil))

(after! plstore
  (setq plstore-cache-passphrase-for-symmetric-encryption t))

(use-package! browse-kill-ring
  :bind
  ("C-x C-y" . browse-kill-ring))

;; Undo/redo window configuration with C-c w u/U.
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

;; Wrap text with polish quotation characters.
(after! smartparens
  (sp-local-pair 'text-mode "„" "”"))

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
(use-package! discover-my-major
  :bind
  ("C-h C-m" . discover-my-major))

(use-package! google-this
  :defer t
  :diminish google-this-mode
  :init
  (progn
    (setq google-this-keybind (kbd "C-x g"))
    (google-this-mode 1)))

(use-package! ws-butler
  :defer t
  :config
  (setq ws-butler-keep-whitespace-before-point t))


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

(use-package! ispell
  :init
  (defun my-set-ispell-dictionary-in-prog-mode ()
    (setq-local ispell-local-dictionary "en"))
  (defun my-set-ispell-dictionary-in-text-mode ()
    (setq-local ispell-local-dictionary "pl"))
  (add-hook 'prog-mode-hook #'my-set-ispell-dictionary-in-prog-mode)
  (add-hook 'text-mode-hook #'my-set-ispell-dictionary-in-text-mode))

;; Guess ispell dictionary language.
(use-package! guess-language
  :config
  (setq guess-language-languages '(en pl)))

;;; Projects

;; Treemacs
(use-package! treemacs
  :defer t
  :init
  (add-hook 'doom-init-ui-hook
            (lambda ()
              (map! :map general-override-mode-map
                    "C-c t t" #'treemacs)))
  :config
  ;; Automatically switch treemacs for the current buffer.
  (treemacs-project-follow-mode))

;;; Major modes

;;; Prog mode

;; Highlight changes made to files under vc.
(use-package! diff-hl
  :defer t
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode))

;; Highlight symbol at point.
(use-package! highlight-symbol
  :defer t
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (bind-key "M-s n" #'highlight-symbol-next prog-mode-map)
  (bind-key "M-s p" #'highlight-symbol-prev prog-mode-map))


;;; Lisp modes

(defconst my-lisp-mode-hooks
  '(emacs-lisp-mode-hook
    inferior-emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook))

(defconst my-lisp-modes
  (mapcar (lambda (hook)
            (let ((hook-name (symbol-name hook)))
              (intern (substring hook-name 0
                                 (- (length hook-name) (length "-hook"))))))
          my-lisp-mode-hooks))

(with-eval-after-load 'smartparens
  ;; paredit's wrap-round.
  (sp-local-pair my-lisp-modes "(" nil :wrap "M-("
                 :post-handlers '(:add my-restore-paren-location))

  ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
  (defun my-add-space-after-sexp-insertion (id action _context)
    "Add space if pair of parens is followed by a sexp or word."
    (when (eq action 'insert)
      (save-excursion
        (forward-char (length (plist-get (sp-get-pair id) :close)))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  ;; sp-wrap-with-pair doesn't execute post-handlers?
  (defun my-restore-paren-location ()
    "Move preceding paren to the previous line if it is empty."
    (let ((empty-line-above (save-excursion
                              (forward-line -1)
                              (looking-at "^\\s-*$"))))
      (when empty-line-above
        (save-excursion
          (forward-line -1)
          (delete-region (point) (1+ (line-end-position))))
        (save-excursion
          (newline-and-indent)))))

  ;; Enable strict mode in lisp modes.
  (mapcar (lambda (hook) (add-hook hook #'smartparens-strict-mode)) my-lisp-mode-hooks))


;;; Org mode

;; Configuration based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html

;; org-directory must be set before doom-package:org has loaded
(setq org-directory (file-truename "~/Dokumenty/org"))

(use-package! org
  :defer t
  :config
  ;; Bring back original keybindings.
  (map! :map org-mode-map
        "C-RET" #'org-insert-heading-respect-content
        "C-S-RET" #'org-insert-todo-heading-respect-content
        [C-return] #'org-insert-heading-respect-content
        [C-S-return] #'org-insert-todo-heading-respect-content)
  (require 'vulpea)

  ;; Indent (view only) headlines and text.
  (setq org-startup-indented t)

  ;; Startup
  (setq org-startup-folded 'show2levels)

  ;; Single key navigation for headlines.
  (setq org-use-speed-commands t)

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WAITING(w!)" "|" "SOMEDAY(s!)" "DONE(d!)" "CANCELLED(c!)")))

  (setq org-todo-keyword-faces
        '(("WAITING" :foreground "orange" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("SOMEDAY" :foreground "dark green" :weight bold)))

  ;; Log state transitions with timestamps into logbook drawer.
  (setq org-log-state-notes-into-drawer t)

  (setq org-capture-templates
        `(("i" "Inbox" entry
           (file ,(concat (file-name-as-directory org-roam-directory) "inbox.org"))
           "* TODO %?\n%T")
          ("l" "Read It Later" entry
           (file ,(concat (file-name-as-directory org-roam-directory) "read_it_later.org"))
           "* %?\n%U")
          ("L" "Read It Later from clipboard" entry
           (file ,(concat (file-name-as-directory org-roam-directory) "read_it_later.org"))
           "* %(org-cliplink-capture)%?\n%U")
          ("T" "Tickler" entry
           (file+headline ,(concat (file-name-as-directory org-roam-directory) "tickler.org") "Tickler")
           "* TODO %i%?\n\n%^t\n\n")
          ("p" "Org Protocol" entry
           (file+headline ,(concat (file-name-as-directory org-roam-directory) "inbox.org") "Org Protocol")
           "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1)))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-agenda-custom-commands
        '(("g" "Getting Things Done"
           ((agenda "")
            (tags
             "inbox"
             ((org-agenda-overriding-header "Inbox")
              (org-tags-match-list-sublevels nil)
              (org-agenda-prefix-format "  %?-12t% s")))
            (todo
             ""
             ((org-agenda-overriding-header "All")
              (org-agenda-skip-function
               '(or (my-org-agenda-skip-file "tickler.org")
                    (my-org-agenda-skip-file "inbox.org")
                    (my-org-agenda-skip-file-with-filetag "work")))
              (org-agenda-prefix-format "  %-20:c%?-12t% s")))))
          ("w" "Work"
           ((tags-todo
             "work"
             ((org-agenda-prefix-format "  %-20:c%?-12t% s")))))))

  ;; Do not split line when cursor in not at the end.
  (setq org-M-RET-may-split-line nil)

  ;; Highlight source code.
  (setq org-src-fontify-natively t)

  (setq org-ellipsis "⤵")

  ;; Hide characters like *word*, etc.
  (setq org-hide-emphasis-markers t)

  ;; Index more levels with imenu.
  (setq org-imenu-depth 5)

  ;; Show entities as UTF8 characters.
  (setq org-pretty-entities t)

  ;; Follow link on RET.
  (setq org-return-follows-link t)

  ;; Align org tags before saving.
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-align-all-tags nil t)))

  ;; Org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     ;; (R . t)
     (calc . t)
     (sql . t)))

  ;; Never evaluate blocks when exporting.
  (setq org-export-babel-evaluate nil)

  (defun my-org-agenda-skip-file (filename)
    (when (string-suffix-p filename (buffer-file-name))
      (point-max)))

  (defun my-org-agenda-skip-file-with-filetag (tag-to-skip)
    (let ((tags (vulpea-buffer-tags-get)))
      (when (member tag-to-skip tags)
        (point-max)))))

(use-package! ox-jira
  :defer t
  :after ox
  :init
  (require 'ox-jira))

(use-package! orgtbl-aggregate
  :defer t)

(use-package! org-gcal
  :defer t
  :init
  (progn
    (add-hook 'org-agenda-mode-hook #'org-gcal-sync)
    (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync))
  :config
  (setq org-gcal-down-days 730))

(use-package! org-tree-slide
  :defer t
  :config
  (setq org-tree-slide-cursor-init nil)
  :bind (:map org-mode-map ("<f8>" . org-tree-slide-mode)))

(use-package! org-protocol
  :defer t
  :config
  (setq org-protocol-default-template-key "p"))

;; Based on:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

(use-package! org-roam
  :defer t
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :init
  (setq org-roam-directory (file-truename "~/Dokumenty/org/roam"))
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Pozycja\n\nAutor: %^{Autor}\nTytuł: ${title}\nRok: %^{Rok}\n\n* Podsumowanie\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title} - %^{Autor}\n")
      :unnarrowed t)
     ("p" "project" plain "* Zadania\n\n** TODO Pierwsze zadanie\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}")
      :unnarrowed t)))
  ;; Add name of day to title of a daily.
  (org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d %A>\n"))))
  :autoload
  (my-org-roam-project-update-tag
   my-org-roam-agenda-files-update)
  :init
  (add-hook 'find-file-hook #'my-org-roam-project-update-tag)
  (add-hook 'before-save-hook #'my-org-roam-project-update-tag)
  (advice-add 'org-agenda :before #'my-org-roam-agenda-files-update)
  (advice-add 'org-todo-list :before #'my-org-roam-agenda-files-update)
  :config
  (require 'vulpea)
  (require 'org-roam-dailies) ;; Ensure the keymap is available

  (defvar my-org-roam-agenda-static-nodes '("inbox" "tickler" "Google Calendar"))

  (defun my-org-roam-project-p ()
    "Return non-nil if current buffer has any todo entry and is not excluded.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks.

Buffers having inactive tag or one of `my-org-roam-agenda-static-nodes' are excluded."
    (and
     (org-element-map
         (org-element-parse-buffer 'headline)
         'headline
       (lambda (h)
         (eq (org-element-property :todo-type h)
             'todo))
       nil 'first-match)
     (let ((tags (vulpea-buffer-tags-get)))
       (not (member "inactive" tags)))
     (let ((title (vulpea-buffer-title-get)))
       (not (member title my-org-roam-agenda-static-nodes)))))

  (defun my-org-roam-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

  (defun my-org-roam-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (my-org-roam-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (my-org-roam-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun my-org-roam-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun my-org-roam-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files' using project files and some static files."
    (setq org-agenda-files
          (append (my-org-roam-project-files)
                  (mapcar #'org-roam-node-file
                          (delq nil
                                (mapcar #'org-roam-node-from-title-or-alias
                                        my-org-roam-agenda-static-nodes))))))

  (org-roam-db-autosync-mode))

(use-package! vulpea
  :defer t)

(use-package! consult-org-roam
  :ensure t
  :defer t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

(add-hook 'doom-init-ui-hook
          (lambda ()
            (map! :map general-override-mode-map
                  "C-c n f" #'consult-org-roam-file-find
                  "C-c n r" #'consult-org-roam-search
                  "C-c n b" #'consult-org-roam-backlinks
                  "C-c n l" #'consult-org-roam-forward-links
                  "C-c n i" #'org-roam-node-insert
                  "C-c n w" #'org-roam-refile
                  "C-c n N" #'org-roam-capture)))

(use-package! ox-clip
  :defer t
  :bind
  ("M-W" . ox-clip-formatted-copy))

;; Automatically balance window margins.
(use-package! olivetti
  :defer t
  :hook
  (text-mode . my-enable-olivetti-except-some-modes)
  :init
  (defun my-enable-olivetti-except-some-modes ()
    (unless (derived-mode-p '(yaml-mode))
      (olivetti-mode 1))))

;; Smartparens
(use-package! smartparens
  :defer t
  :config
  ;; Highlights matching pairs
  (show-smartparens-global-mode)

  ;; Always skip closing pair even if the expression is not active
  (setq sp-autoskip-closing-pair 'always)

  ;; Some of the bindings are already defined by Doom

  (bind-key "C-M-f" #'sp-forward-sexp)
  (bind-key "C-M-b" #'sp-backward-sexp)

  (bind-key "C-M-n" #'sp-next-sexp)
  (bind-key "C-M-p" #'sp-previous-sexp)

  (bind-key "C-S-a" #'sp-beginning-of-sexp)
  (bind-key "C-S-e" #'sp-end-of-sexp)

  (bind-key "C-M-d" #'sp-down-sexp)
  (bind-key "C-M-S-d" #'sp-backward-down-sexp)

  (bind-key "C-M-u" #'sp-up-sexp)
  (bind-key "C-M-S-u" #'sp-backward-up-sexp)
  (bind-key "C-M-t" 'sp-transpose-sexp)

  (bind-key "C-M-k" #'sp-kill-sexp)
  (bind-key "C-M-w" #'sp-copy-sexp)

  (bind-key "C-)" #'sp-forward-slurp-sexp)
  (bind-key "C-}" #'sp-forward-barf-sexp)
  (bind-key "C-(" #'sp-backward-slurp-sexp)
  (bind-key "C-{" #'sp-backward-barf-sexp)

  (bind-key "M-s M-s" #'sp-splice-sexp)
  (bind-key "M-s M-S" #'sp-split-sexp)
  (bind-key "M-s M-c" #'sp-convolute-sexp)
  (bind-key "M-s M-a" #'sp-absorb-sexp)
  (bind-key "M-s M-e" #'sp-emit-sexp)
  (bind-key "M-s M-n" #'sp-add-to-next-sexp)
  (bind-key "M-s M-p" #'sp-add-to-previous-sexp)
  (bind-key "M-s M-j" #'sp-join-sexp))


;;; Consult

(use-package! consult
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
                                        ;("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
                                        ;("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
                                        ;("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
                                        ;("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
                                        ;("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-x C-i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  )

(use-package! embark
  :defer t
  :bind
  (("C-c C-a" . embark-dwim)
   ("C-c a" . embark-act)
   ("C-h B" . embark-bindings)))  ;; alternative for `describe-bindings'

(use-package! orderless
  :defer t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-literal orderless-flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; Dired
(after! dired-x
  ;; Omit hidden files.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.[^\\.].*$")))

(use-package! dired-imenu
  :defer t
  :commands (dired-setup-imenu)
  :init
  (add-hook 'dired-mode-hook #'dired-setup-imenu))

(use-package! dired-open
  :defer t
  :bind
  (:map dired-mode-map
        ("<C-return>" . dired-open-xdg)))

;; Edit text in browser with GhostText.
(use-package! atomic-chrome
  :init
  (atomic-chrome-start-server))
