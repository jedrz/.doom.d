;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font "JetBrains Mono 10")
(setq doom-theme 'doom-monokai-classic)

;; Absolute line numbers.
(setq display-line-numbers-type t)

;; Start maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable only in text- and prog-modes.
(use-package display-fill-column-indicator
  :defer t
  :init
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'text-mode-hook #'display-fill-column-indicator-mode))
