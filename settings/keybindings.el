;;; settings/keybindings.el

;; This file contains all the custom keybindings for this Emacs configuration
;; that do not rely on deferred package loading.
;; It is loaded at the end of init.el.

(use-package bind-key
  :ensure t
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Global Keybindings
  ;;
  (global-unset-key (kbd "C-x C-c"))  ; disable quit
  (global-unset-key (kbd "C-x c"))    ; disable quit
  (global-unset-key (kbd "C-z"))      ; disable minimize
  (global-unset-key (kbd "s-t"))      ; disable font-panel
  (global-unset-key (kbd "s-p"))      ; disable ns-print-buffer

  (bind-keys
   ;; File and Buffer Management
   ("C-x C-j" . (lambda () (interactive) (dired default-directory))) ; Open dired in the current directory
   ("C-x r q" . save-buffers-kill-terminal) ; Remapped quit-key
   ("C-x C-f" . jez-find-file)

   ;; Window Management
   ("C-x |" . toggle-window-split)

   ;; Text Editing and Navigation
   ("<f12>" . line-copy-char)
   ("<f5>" . sort-lines)
   ("C-c C-<return>" . delete-trailing-whitespace)
   ("C-c t" . toggle-truncate-lines)
   ("M-J" . jez-simplify)
   ("M-SPC" . cycle-spacing)
   ("M-i" . back-to-indentation)
   ("M-j" . jez-join-line)
   ("M-n" . (lambda (arg) (interactive "p") (next-line (* arg 5))))
   ("M-p" . (lambda (arg) (interactive "p") (previous-line (* arg 5))))
   ("C-M-<backspace>" . jez-kill-back-to-indentation)
   ("S-<return>" . sanityinc/newline-at-end-of-line)

   ;; Search
   ("s-r" . isearch-backward-regexp)
   ("s-s" . isearch-forward-regexp)

   ;; Shell and Terminal
   ("C-z" . jez-shell-shortcut)

   ;; Snippets and Expansion (Hippie Expand)
   ("M-/" . hippie-expand-no-case-fold)
   ("C-:" . hippie-expand-lines)
   ("C-," . jez-helm-hippie-expand)

   ;; Other Custom Functions
   ("C-c j d" . jez-insert-date)
   ("C-c j t" . jez-insert-time)
   ("C-c j z" . jez-create-shell-buffer))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Mode-Specific Keybindings for Non-Deferred Packages
  ;;

  ;; Isearch
  (bind-keys :map isearch-mode-map
             ("s-s" . isearch-repeat-forward)
             ("s-r" . isearch-repeat-backward))

  ;; Symbol Overlay
  (bind-keys
   ("s-m" . symbol-overlay-put)
   ("s-M" . symbol-overlay-remove-all)
   ("s-n" . symbol-overlay-jump-next)
   ("s-p" . symbol-overlay-jump-prev)))

(provide 'keybindings)
