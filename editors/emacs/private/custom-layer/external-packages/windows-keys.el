;; Piyush@PrivatePC.15464:1725315737
;;; Windows-style undo/redo
;;; (***Must bind 'undo before activating CUA-mode; see below)
;; https://emacs.stackexchange.com/questions/42527/trying-to-make-control-f-search-exactly-like-control-s-does
;; https://fingerfans.dreamhosters.com/archive/attachments/
;; https://fingerfans.dreamhosters.com/archive/attachments/5023_cnewcombe_emacs_like_windows.txt
;; https://emacs.stackexchange.com/questions/33770/redo-ctrly-undo
;; https://www.emacswiki.org/emacs/redo+.el
(require 'redo+)
(global-set-key [(control z)] 'undo)
(global-set-key [(control y)] 'redo)


;;; CUA cut/copy/paste emulation.  Can be customized.
                                        ;
                                        ; "If you bind any keys to 'undo or 'advertised-undo in your .emacs file,
                                        ;  CUA-mode should be activated *after* those bindings!  Otherwise,
                                        ;  bind the key to CUA-undo"
                                        ;
                                        ;   S-arrow  (highlight)
                                        ;   C-c      Copy when region is highlighted
                                        ;   C-x      Cut when region is highlighted
                                        ;   C-v      Paste when region is highlighted
                                        ;   S-return (highlight rectangle + other support)
;;(require 'cua)
(cua-mode t)
                                        ; See also pc-bindings-mode in 'pc-mode.el'.  However, this seems to be unecessary
                                        ; (or perhaps loaded by something else?).


;;; Bind C-a to 'select-all' like Windows. (Emacs also binds it to C-x h)
(global-set-key [(control a)] 'mark-whole-buffer)


;;; Visible bookmarks, mapped to same keys as VisualStudio.  Can be customized.
;; (require 'bm)
;; (if (< emacs-major-version 21)
;;     (progn
;;                                         ; bm v0.5
;;       (global-set-key [(control f2)] 'bm-toggle-bookmark)
;;       (global-set-key [(f2)] 'bm-goto-bookmark)
;;       (global-set-key [(shift f2)] 'bm-goto-bookmark-previous))
;;   (progn
;;                                         ; bm v1.29 (persistent bookmarks)
;;     (global-set-key [(control f2)] 'bm-toggle)
;;     (global-set-key [(f2)] 'bm-next)
;;     (global-set-key [(shift f2)] 'bm-previous)))


;;; Windows-style buffer-cycling, mapped to keypad-'+' and keypad-'-'.
;; (require 'swbuff)
;; (global-set-key [(kp-subtract)] 'swbuff-switch-to-next-buffer)
;; (global-set-key [(kp-add)] 'swbuff-switch-to-previous-buffer)

;; ;;; More buffer and window management
;; (global-set-key [(kp-multiply)] 'kill-this-buffer)
;; (global-set-key [(kp-period)] 'delete-window)  ; close window; native is C-x 0
;; (defun kill-this-buffer-and-close-current-window ()
;;   (interactive)
;;   (kill-this-buffer)
;;   (delete-window))
;; (global-set-key [(kp-divide)] 'kill-this-buffer-and-close-current-window)
;; (global-set-key [(kp-enter)] 'other-window)


;;; Directionally changing focus between multiple on-screen windows
;; built in
;; (require 'windmove)
                                        ; the default key bindints would be shift-cursorkeys, which clashes with 'select region' in cua-mode.
(global-set-key [(meta left)]  'windmove-left)
(global-set-key [(meta up)]    'windmove-up)
(global-set-key [(meta right)] 'windmove-right)
(global-set-key [(meta down)]  'windmove-down)


;;; We need 'goto line'.  Note that we want to keep C-g as "quit".
;;(global-set-key [(control l)] 'goto-line)


;;; Move cursor to matching parenthesis.
(defun find-matching-paren ()
  "Locate the matching parenthtical"
  (interactive)
  (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
        ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
        (t (ding))))

                                        ; Bind it to C-e (like VisualStudio 6)
;;(global-set-key [(control e)] 'find-matching-paren)


;;; Emacs' forward-word leaves the cursor at the _end_ of the word (start of non-word separators)
;;; We want to leave the cursor at the start of the next word.
(defun geosoft-forward-word ()
  ;; Move one word forward. Leave the pointer at start of word
  ;; instead of emacs default end of word. Treat _ as part of word
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (forward-word 2)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (forward-char 1) (geosoft-forward-word))
        (t (forward-char 1))))

(defun geosoft-backward-word ()
  ;; Move one word backward. Leave the pointer at start of word
  ;; Treat _ as part of word
  (interactive)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (geosoft-backward-word))
        (t (forward-char 1))))

(global-set-key [(control right)] 'geosoft-forward-word)
(global-set-key [(control left)] 'geosoft-backward-word)


;;; Support functions for search/replace through all current buffers.
;;; Note, use "M-," (tags-loop-continue) to continue the search/replace.
                                        ; helpers
(defun remove-nils (lis)
  "Return list LIS with nils removed."
  (let ((res nil))
    (while lis
      (if (car lis)
          (if res
              (nconc res (cons (car lis) nil))
            (setq res (cons (car lis) nil))))
      (setq lis (cdr lis)))
    res))
(defun visited-files ()
  "The filenames of all buffers."
  (remove-nils (mapcar 'buffer-file-name (buffer-list))))

(defun buffer-search-regexp (regexp)
  "Search through all buffers which are visiting a file for REGEXP, just like 'tags-search'."
  (interactive "sBuffer search (regexp): ")   ; the 's' at the start of the string is required
  (tags-search regexp (list 'visited-files)))

(defun buffer-query-replace-regexp (from to &optional delimited start end)
  "Query-replace-regexp FROM with TO through all buffers which are visiting a file, just like 'tags-query-replace'."
  (interactive (query-replace-read-args "Buffer search/replace (regexp)" t))
  (tags-query-replace from to delimited (list 'visited-files) start end))


;;;; Bind default TouchStream gestures to their normal keys/functions where possible.
;;;; Adapted from touchstream.el

;;; Open and Save file:  compatibility mapping to Ctrl-o and Ctrl-s.
(global-set-key [(control o)]  'find-file)  ; replaces 'open line'
(global-set-key [(control O)]  'find-file-other-frame)
(global-set-key [(control s)]  'save-buffer) ; replaces i-search (which is remapped below)
(global-set-key [(control S)]  'write-file)  ; write-file is 'Save As'
(global-set-key [(control f4)] 'kill-buffer)

;;; Search commands
                                        ;   Ctrl-f variants initiate search
                                        ;   Ctrl-n repeat last search forward
                                        ;   Ctrl-p repeat last search backward
                                        ;   Ctrl-r variants initiate search & replace
                                        ; Gestures generate the Ctrl- keystrokes.  The following modifier keys can
                                        ; be used to choose between the different search functions:
                                        ;   none       normal search/replace
                                        ;   Shift-     regexp search/replace
                                        ;   Meta-      tags search/replace

(global-set-key [(control f)] 'isearch-forward)
(global-set-key [(control n)] 'isearch-forward)
(global-set-key [(control p)] 'isearch-backward)
(global-set-key [(control r)] 'query-replace)
(global-set-key [(control F)] 'isearch-forward-regexp)
(global-set-key [(control N)] 'isearch-forward-regexp)
(global-set-key [(control P)] 'isearch-backward-regexp)
(global-set-key [(control R)] 'query-replace-regexp)
                                        ; backward and forward tags searches may be improperly mapped here...
(global-set-key [(control meta f)] 'buffer-search-regexp) ; touchstream.el binds this to tags-search
(global-set-key [(control meta n)] 'tags-loop-continue)
(global-set-key [(control meta p)] 'find-tag)
(global-set-key [(control meta r)] 'tags-query-replace) ; touchstream.el binds this to buffer-query-replace-regexp

                                        ; isearch requires some customization to work with none default keys,
                                        ; since it uses its own keymap during a search.  These changes are *always*
                                        ; active, and not toggled with touchstream mode!  Luckly for us, the keys are
                                        ; we need are not used by isearch so there are no conflicts.
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)
(define-key isearch-mode-map [(control n)] 'isearch-repeat-forward)
(define-key isearch-mode-map [(control p)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(control s)] 'save-buffer)

;; variable completions.
(global-set-key [(control <)] 'dabbrev-expand)
(global-set-key [(control >)] 'dabbrev-expand)  ; no such thing as prev/next dabbrev in emacs
;; (if (string-match "XEmacs" emacs-version)
;;                                         ; XEmacs requires "space" spelled out
;;     (global-set-key [(control shift space)] 'dabbrev-expand-copy)
;;                                         ; Emacs does not understand "space", but rather needs "? ".
;;   (global-set-key   [(control shift ? )]    'dabbrev-expand-copy))

;; window operations
;; (global-set-key [(control f5)] 'iconify-frame)
;; (global-set-key [(meta f4)] '(lambda ()
;;                                (interactive)
;;                                (delete-frame nil t)))

;; programming
(global-set-key [(control \;)] 'comment-region)


;;; Improved highlighting of matching/mis-matched parentheses. Can be customized.
;; (require 'mic-paren)
;; (paren-activate)


;;; Optionally show whitespace
;; (if (< emacs-major-version 21)
;;                                         ; This is pretty poor, but better than nothing.  Adds a Tool menu.
;;     (require 'tab-display)
;;                                         ; This one only works in Emacs 21. Can be customized.
;;                                         ; WARNING: turning on blank-mode can occasionally affect C/C++ smart indenting (TAB) for some reason.
;;   (progn
;;     (require 'blank-mode)
;;     (global-set-key [(f5)] 'blank-mode)))  ; toggle


;;; Truncation/wrapping of lines (set to truncate by default in Customize)
(global-set-key [(f6)] 'toggle-truncate-lines)


;;; Convenient smart-indenting of regions
(global-set-key [(control tab)] 'indent-region)


;;; Indent & Outdent marked block (copied from .emacs found on internet)
(defun my-indent-fn (cols)
  ;; Original called (transient-mark-mode nil) but it doesn't make any difference.
  (let ((m (mark))
        (p (point)))
    (if (< m p)
        (indent-rigidly m p cols)
      (indent-rigidly p m cols)))
  ;; Original called (transient-mark-mode t) but it doesn't make any difference
  )

                                        ; Original comment: "The only way the indent functions don't invisibilize the selected
                                        ; text is if the handler fn causes an error.  That's right, the fn must
                                        ; cause an error for it to work right.  What the hell.  It's not worth it."
(defun my-indent ()
  "indent region"
  (interactive)
  (my-indent-fn 1))

(defun my-outdent ()
  "outdent region"
  (interactive)
  (my-indent-fn -1))

;; These functions are currently unbound, as 'alt-cursorkeys; is now
;; uses by windmove.
                                        ;(global-set-key [(meta right)] 'my-indent)
                                        ;(global-set-key [(meta left)] 'my-outdent)


;;; C++ compilation
                                        ; Note use "M-x kill-compilation" to abort without starting a new compilation.
(setq compilation-scroll-output t)  ; note that 'Compiling' status is also displayed on all buffer mode-lines
(global-set-key [(f9)] 'compile)
(global-set-key [(f4)] 'next-error)
(global-set-key [(shift f4)] 'previous-error)
