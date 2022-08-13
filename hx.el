;; -*- lexical-binding: t; -*-
;;; hx.el ---
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords:

;; Package-Requires: (
;;     (emacs "28")
;;     (iedit "0.9.9.9.9"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;; See documentation on https://github.com/Yevgnen/hx.el.

;;; Code:

(require 'view)
(require 'cl-seq)
(require 'iedit)

;; Utils
(defun hx-normal--inhibit-read-only (f)
  (lambda ()
    (interactive)
    (let ((inhibit-read-only t))
      (call-interactively f))))

(defun hx-normal--set-mark (f)
  (lambda ()
    (interactive)
    (unless (region-active-p)
      (set-mark-command nil))
    (call-interactively f)))

(defun hx-normal--repeat (f)
  (lambda (arg)
    (interactive "p")
    (unless arg
      (setq arg 1))
    (dolist (i (number-sequence (1- arg)))
      (call-interactively f))))

(defun hx-normal--set-mark-repeat (f)
  (lambda (arg)
    (interactive "p")
    (unless (region-active-p)
      (set-mark-command nil))
    (unless arg
      (setq arg 1))
    (dolist (i (number-sequence 1 arg))
      (call-interactively f))))

(defun hx-normal--repeat-set-mark (f)
  (lambda (arg)
    (interactive "p")
    (unless arg
      (setq arg 1))
    (dolist (i (number-sequence 1 arg))
      (set-mark-command nil)
      (call-interactively f))))

;; Movement

(defun hx-normal--next-symbol ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (symbol-start (car bounds))
         (symbol-end (cdr bounds)))
    (cond ((and symbol-start
                (= (point) symbol-start))
           (goto-char symbol-end))
          (t (search-forward-regexp "\\<")))))

;;;###autoload
(defun hx-normal-next-symbol ()
  (interactive)
  (call-interactively
   (hx-normal--repeat-set-mark #'hx-normal--next-symbol)))

;;;###autoload
(defun hx-normal-previous-symbol ()
  (interactive)
  (set-mark-command nil)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (symbol-start (car bounds))
         (symbol-end (cdr bounds)))
    (cond ((and symbol-end
                (= (point) symbol-end))
           (goto-char symbol-start))
          (t (search-backward-regexp "\\>")))))

(defun hx-normal--jump-to-space-forward()
  (interactive)
  (let ((old-pos (point)) m-end m-begin)
    (when (re-search-forward "[ \t\n]+"  nil t)
      (setq m-begin (match-beginning 0))
      (setq m-end (match-end 0))
      (goto-char m-begin)
      (if (equal old-pos m-end)
          (progn
            (re-search-forward "[ \t\n]+"  nil t)
            (goto-char (match-beginning 0)))
        (if (equal m-begin old-pos)
            (goto-char m-end))))))

;;;###autoload
(defun hx-normal-jump-to-space-forward()
  (interactive)
  (call-interactively
   (hx-normal--repeat-set-mark #'hx-normal--jump-to-space-forward)))

;;;###autoload
(defun hx-normal-find-till-next-char ()
  (interactive)
  )

;;;###autoload
(defun hx-normal-find-till-previous-char ()
  (interactive)
  )

;;;###autoload
(defun hx-normal-find-next-char ()
  (interactive)
  (let* ((chr (read-char))
         (str (char-to-string chr))
         (chr-after (char-after))
         (target (save-excursion
                   (if (= chr-after chr)
                       (forward-char))
                   (search-forward str nil 'no-error))))
    (when target
      (goto-char target)
      (backward-char))
    target))

;;;###autoload
(defun hx-normal-find-previous-char ()
  (interactive)
  (let* ((chr (read-char))
         (str (char-to-string chr))
         (chr-before (char-before))
         (target (save-excursion
                   (if (= chr-before chr)
                       (backward-char))
                   (search-backward str nil 'no-error))))
    (when target
      (goto-char target))
    target))

;;;###autoload
(defun hx-eval-and-forward (arg)
  (interactive "P")
  (forward-sexp)
  (eval-last-sexp arg))

;; Changes

;;;###autoload
(defun hx-normal-replace-char ()
  (interactive)
  (let ((c (read-char)))
    (delete-char 1)
    (insert c)))

;;;###autoload
(defun hx-normal-new-line-below ()
  (interactive)
  (hx-normal-into-insert-mode)
  (end-of-line)
  (newline-and-indent)
  (back-to-indentation))

;;;###autoload
(defun hx-normal-new-line-above ()
  (interactive)
  (hx-normal-into-insert-mode)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

;;;###autoload
(defun hx-normal-insert-at-bol ()
  (interactive)
  (back-to-indentation)
  (hx-normal-into-insert-mode))

;;;###autoload
(defun hx-normal-insert-at-eol ()
  (interactive)
  (end-of-line)
  (hx-normal-into-insert-mode))

;;;###autoload
(defun hx-normal-change-selection ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (region-active-p)
        (delete-region (region-beginning) (region-end))))
  (hx-normal-into-insert-mode))

;;;###autoload
(defun hx-normal-format ()
  (interactive)
  (hx-normal--inhibit-read-only
   (lambda ()
     (interactive)
     (if lsp-mode
         (if (region-active-p)
             (lsp-format-region (region-beginning) (region-end))
           (lsp-format-buffer))
       (if (region-active-p)
           (indent-region (region-beginning) (region-end))
         (indent-region (point-min) (point-max)))))))

;; Goto mode

;;;###autoload
(defun hx-normal-next-user-buffer ()
  "Switch to the next user buffer in cyclic order.
User buffers are those not starting with *."
  (interactive)
  (let ((bufname (buffer-name)))
    (next-buffer)
    (while (and (string-match "^*" (buffer-name))
                (not (string= bufname (buffer-name))))
      (next-buffer))))

;;;###autoload
(defun hx-normal-previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.
User buffers are those not starting with *."
  (interactive)
  (let ((bufname (buffer-name)))
    (previous-buffer)
    (while (and
            (string-match "^*" (buffer-name))
            (not (string= bufname (buffer-name))))
      (previous-buffer))))

;; View mode

;;;###autoload
(defun hx-view-recenter-top ()
  (interactive)
  (let ((recenter-positions '(top)))
    (recenter-top-bottom)))

;;;###autoload
(defun hx-view-recenter-middle ()
  (interactive)
  (let ((recenter-positions '(middle)))
    (recenter-top-bottom)))

;;;###autoload
(defun hx-view-recenter-bottom ()
  (interactive)
  (let ((recenter-positions '(bottom)))
    (recenter-top-bottom)))

;; Selection manipulation

;;;###autoload
(defun hx-select-keep-only-primary-selection ()
  (interactive)
  (mc/remove-fake-cursors))

;;;###autoload
(defun hx-select-current-line (&optional arg)
  (interactive "p")
  (unless (region-active-p)
    (beginning-of-line)
    (set-mark-command nil))
  (next-line arg))

;;;###autoload
(defun hx-select-current-line-extend ()
  (interactive)
  (let ((flag (> (point) (mark))))
    (unless flag
      (exchange-point-and-mark))
    (end-of-line)
    (exchange-point-and-mark)
    (beginning-of-line)
    (if flag
        (exchange-point-and-mark))))

;;;###autoload
(defun hx-select-join-lines ()
  (interactive)
  (join-line current-prefix-arg (region-beginning) (region-end)))

;;;###autoload
(defun hx-select-comment ()
  (interactive)
  (when (region-active-p)
    (call-interactively
     (hx-normal--inhibit-read-only #'comment-or-uncomment-region))
    (activate-mark)))

;; Space mode

;;;###autoload
(defun hx-space-find-file ()
  (interactive)
  (let* ((commands '(proejct-find-file find-file))
         (command (car (cl-remove-if-not #'fboundp commands))))
    (call-interactively command)))

;;;###autoload
(defun hx-sapce-rename (&optional arg)
  (interactive "P")
  (if lsp-mode
      (call-interactively #'lsp-rename)
    (iedit-mode arg)))

;;;###autoload
(defun hx-space-doc ()
  (interactive)
  (cond ((bound-and-true-p lsp-mode)
         (lsp-describe-thing-at-point))
        ((eq major-mode 'emacs-lisp-mode)
         (cond ((fboundp #'elisp-slime-nav-describe-elisp-thing-at-point)
                (call-interactively #'elisp-slime-nav-describe-elisp-thing-at-point))
               (t nil)))))

;; Umimpared

;;;###autoload
(defun hx-unimpared-add-newline-above (&optional arg)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (newline arg)))

;;;###autoload
(defun hx-unimpared-add-newline-below (&optional arg)
  (interactive "p")
  (save-excursion
    (end-of-line)
    (newline arg)))

;;;###autoload
(defun hx-normal-into-insert-mode ()
  (interactive)
  (hx-normal-mode -1)
  (hx-insert-mode 1))

(defvar hx-normal--cursor-status nil)
(defvar hx-normal--transient-mark-mode-status nil)
(defvar hx-normal--delete-selection-mode-status nil)
(defvar hx-normal--read-only-mode-status nil)

(defalias 'sh #'shell-command)
(defalias 'w #'save-buffer)
(defalias 'bc #'kill-current-buffer)
(defalias 'o #'find-file)
(defalias 'q #'save-buffers-kill-emacs)

(defvar hx-normal-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; Movement
    (define-key keymap "w" #'hx-normal-next-symbol)
    (define-key keymap "b" #'hx-normal-previous-symbol)
    (define-key keymap "k" #'previous-line)
    (define-key keymap "j" #'next-line)
    (define-key keymap "h" #'backward-char)
    (define-key keymap "l" #'forward-char)
    (define-key keymap "W" #'hx-normal-jump-to-space-forward)
    (define-key keymap "t" #'hx-normal-find-till-next-char)
    (define-key keymap "f" #'hx-normal-find-next-char)
    (define-key keymap "T" #'hx-normal-find-till-previous-char)
    (define-key keymap "F" #'hx-normal-find-previous-char)
    (define-key keymap "G" #'goto-line)
    (define-key keymap "e" #'hx-eval-and-forward)
    (define-key keymap (kbd "C-f") #'scroll-up-command)
    (define-key keymap (kbd "C-b") #'scroll-down-command)
    (define-key keymap (kbd "C-u") #'View-scroll-half-page-forward)
    (define-key keymap (kbd "C-d") #'View-scroll-half-page-backward)
    ;; Changes
    (define-key keymap "r" #'hx-normal-replace-char)
    (define-key keymap "R" #'yank)
    (define-key keymap "`" #'downcase-region)
    (define-key keymap (kbd "M-`") #'upcase-region)
    (define-key keymap "i" #'hx-normal-into-insert-mode)
    (define-key keymap "I" #'hx-normal-insert-at-bol)
    (define-key keymap "A" #'hx-normal-insert-at-eol)
    (define-key keymap "o" #'hx-normal-new-line-below)
    (define-key keymap "O" #'hx-normal-new-line-above)
    (define-key keymap "u" (hx-normal--inhibit-read-only #'undo))
    (define-key keymap "U" (hx-normal--inhibit-read-only #'undo-redo))
    (define-key keymap "y" #'kill-ring-save)
    (define-key keymap "=" #'hx-normal-format)
    (define-key keymap "d" (hx-normal--inhibit-read-only #'kill-region))
    (define-key keymap "c" #'hx-normal-change-selection)
    (define-key keymap "Q" #'kmacro-start-macro-or-insert-counter)
    (define-key keymap "q" #'kmacro-end-or-call-macro)
    ;; Shell
    (define-key keymap "!" #'shell-command)
    ;; Goto mode
    (define-key keymap "gg" #'hx-goto-goto-line-or-beginning-of-buffer)
    (define-key keymap "ge" #'end-of-buffer)
    (define-key keymap "gf" #'find-file)
    (define-key keymap "gh" #'beginning-of-line)
    (define-key keymap "gl" #'end-of-line)
    (define-key keymap "gs" #'back-to-indentation)
    (define-key keymap "gd" #'xref-find-definitions)
    (define-key keymap "gr" #'lsp-find-references)
    (define-key keymap "gi" #'lsp-find-implementation)
    (define-key keymap "gy" #'lsp-find-type-definition)
    (define-key keymap "gn" #'hx-normal-next-user-buffer)
    (define-key keymap "gp" #'hx-normal-previous-user-buffer)
    ;; Match mode
    (define-key keymap "mf" #'forward-sexp)
    (define-key keymap "mb" #'backward-sexp)
    (define-key keymap "mw" #'easy-kill)
    ;; View mode
    (define-key keymap "zz" #'hx-view-recenter-middle)
    (define-key keymap "zc" #'hx-view-recenter-middle)
    (define-key keymap "zt" #'hx-view-recenter-top)
    (define-key keymap "zb" #'hx-view-recenter-bottom)
    (define-key keymap "%" #'mark-whole-buffer)
    ;; Selection manipulation
    (define-key keymap "s" #'mc/mark-all-in-region-regexp)
    (define-key keymap "," #'hx-select-keep-only-primary-selection)
    (define-key keymap "x" #'hx-select-current-line)
    (define-key keymap "X" #'hx-select-current-line-extend)
    (define-key keymap "J" #'hx-select-join-lines)
    (define-key keymap (kbd "C-c") #'hx-select-comment)
    (define-key keymap ";" #'set-mark-command)
    ;; Search
    (define-key keymap "/" #'View-search-regexp-forward)
    (define-key keymap "?" #'View-search-regexp-backward)
    (define-key keymap "n" #'View-search-last-regexp-forward)
    (define-key keymap "N" #'View-search-last-regexp-backward)
    ;; Space mode
    (define-key keymap " f" #'hx-space-find-file)
    (define-key keymap " b" #'switch-to-buffer)
    (define-key keymap " k" #'hx-space-doc)
    (define-key keymap " s" #'imenu)
    (define-key keymap " S" #'lsp-ui-find-workspace-symbol)
    (define-key keymap " r" #'hx-sapce-rename)
    (define-key keymap " a" #'lsp-execute-code-action)
    (define-key keymap " ?" #'execute-extended-command)
    (define-key keymap " /" #'consult-git-grep)
    ;; Unimpaired
    (define-key keymap "[d" #'flycheck-previous-error)
    (define-key keymap "]d" #'flycheck-next-error)
    (define-key keymap "[D" #'flycheck-first-error)
    (define-key keymap "[f" #'beginning-of-defun)
    (define-key keymap "]f" #'end-of-defun)
    (define-key keymap "[p" #'backward-paragraph)
    (define-key keymap "]p" #'forward-paragraph)
    (define-key keymap "[ " (hx-normal--inhibit-read-only #'hx-unimpared-add-newline-above))
    (define-key keymap "] " (hx-normal--inhibit-read-only #'hx-unimpared-add-newline-below))
    ;; Help
    (define-key keymap "He" #'view-echo-area-messages)
    (define-key keymap "Hk" #'describe-key)
    (define-key keymap "Hc" #'describe-char)
    (define-key keymap "Hf" #'describe-function)
    (define-key keymap "HF" #'describe-function)
    (define-key keymap "HV" #'describe-variable)
    ;; Minor modes
    (define-key keymap "v" #'set-mark-command)
    (define-key keymap ":" #'execute-extended-command)
    (dolist (i (number-sequence 0 9))
      (define-key keymap (int-to-string i) #'digit-argument))
    keymap))

;;;###autoload
(define-minor-mode hx-normal-mode
  "Helix normal mode."
  :keymap hx-normal-mode-map
  (if hx-normal-mode
      (progn
        (setq hx-normal--cursor-status cursor-type
              hx-normal--transient-mark-mode-status transient-mark-mode
              hx-normal--read-only-mode-status buffer-read-only
              cursor-type 'box)
        (transient-mark-mode)
        (read-only-mode 1))
    (setq cursor-type hx-normal--cursor-status)
    (unless hx-normal--transient-mark-mode-status
      (transient-mark-mode))
    (unless hx-normal--read-only-mode-status
      (read-only-mode -1))))

;;;###autoload
(defun hx-kill-to-beginning-of-line ()
  (interactive)
  (kill-region (line-beginning-position) (point)))

;;;###autoload
(defun hx-kill-to-end-of-line ()
  (interactive)
  (kill-region (point) (line-end-position)))

;;;###autoload
(defun hx-insert-into-normal-mode ()
  (interactive)
  (hx-insert-mode -1)
  (hx-normal-mode 1))

(defvar hx-insert-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<escape>") #'hx-insert-into-normal-mode)
    (define-key keymap (kbd "C-w") #'backward-kill-word)
    (define-key keymap (kbd "M-d") #'kill-word)
    (define-key keymap (kbd "M-b") #'backward-word)
    (define-key keymap (kbd "M-f") #'forward-word)
    (define-key keymap (kbd "C-b") #'backward-char)
    (define-key keymap (kbd "C-f") #'forward-char)
    (define-key keymap (kbd "C-a") #'beginning-of-line)
    (define-key keymap (kbd "C-e") #'end-of-line)
    (define-key keymap (kbd "C-u") #'hx-kill-to-beginning-of-line)
    (define-key keymap (kbd "C-k") #'hx-kill-to-end-of-line)
    (define-key keymap (kbd "C-j") #'newline)
    (define-key keymap (kbd "<backspace>") #'backward-delete-char-untabify)
    (define-key keymap (kbd "C-d") #'delete-char)
    keymap))

;;;###autoload
(define-minor-mode hx-insert-mode
  "Helix insert mode."
  :keymap hx-insert-mode-map
  (if hx-insert-mode
      (setq cursor-type '(bar . 1))))

;;;###autoload
(defun hx-goto-into-normal-mode ()
  (interactive)
  (hx-goto-mode -1)
  (hx-normal-mode 1))

(defvar hx-goto-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "ESC") #'hx-goto-into-normal-mode)
    keymap))

;;;###autoload
(defun hx-goto-goto-line-or-beginning-of-buffer (arg)
  (interactive "p")
  (if arg
      (goto-line arg)
    (beginning-of-buffer)))

;;;###autoload
(define-minor-mode hx-goto-mode
  "Helix goto mode."
  :keymap hx-goto-mode-map)

(defvar hx-mode-line-format
  '(:eval (cond (hx-normal-mode " NOM")
                (hx-insert-mode " INS")
                (hx-mode " Hx"))))

;;;###autoload
(define-minor-mode hx-mode
  "Helix like editing."
  :global nil
  (if hx-mode
      (progn
        (cl-pushnew hx-mode-line-format mode-line-format :test #'equal)
        (hx-normal-mode 1))
    (cl-remove hx-mode-line-format mode-line-format)
    (hx-normal-mode -1)
    (hx-insert-mode -1)))

(provide 'hx)

;;; hx.el ends here
