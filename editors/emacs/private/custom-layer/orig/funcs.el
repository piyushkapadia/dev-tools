(message "Ending ~/.emacs.d/private/my-custom/func.el %s" (format-time-string "%Y-%m-%dTT"))
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2010-07/msg00291.html
;; https://emacs.stackexchange.com/questions/32552/how-do-i-invoke-a-non-interactive-lisp-function-interactively
;; https://emacs.stackexchange.com/questions/32753/call-interactive-function-from-elisp-code-without-worrying-about-arguments
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Call.html
;; http://ergoemacs.org/emacs/emacs_key_macro_example_add_url_title.html
;; https://teaching.sociology.ul.ie/bhalpin/wordpress/?p=580
;; https://emacs.stackexchange.com/questions/27620/orgmode-capturing-original-document-title
;; https://orgmode.org/manual/Handling-Links.html
;; https://stackoverflow.com/questions/1642184/extracting-urls-from-an-emacs-buffer
;; (www-get-page-title "http://www.emacswiki.org/emacs/Git")
;; => "EmacsWiki: Git"



(defun www-get-page-title (url)
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1))
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (decode-coding-string title (intern (match-string 1))))))

;; `decode-coding-string' doesn't handle upper-case charsets, like UTF-8,only utf-8.	  
;; (decode-coding-string title (intern (downcase (match-string 1)))))))	  

;; -------------------------- separator --------------------------
(defun get-page-title()
  "Get title of web page, whose url can be found in the current line"
  (interactive)
  ;; Get url from current line
  (copy-region-as-kill (re-search-backward "^") (re-search-forward "$"))
  (setq url (substring-no-properties (current-kill 0)))
  ;; Get title of web page, with the help of functions in url.el
  (with-current-buffer (url-retrieve-synchronously url)
    ;; find title by grep the html code
    (goto-char 0)
    (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
    (setq web_title_str (match-string 1))
    ;; find charset by grep the html code
    (goto-char 0)
    (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
    ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while
    utf-8 is ok.
    (setq coding_charset (downcase (match-string 1)))
    ;; decode the string of title.
    (setq web_title_str (decode-coding-string web_title_str (intern
                                                             coding_charset)))
    )
  ;; Insert the title in the next line
  (reindent-then-newline-and-indent)
  (insert web_title_str)
  )

;; C-c C-l runs the command org-insert-link (found in org-mode-map), which is an interactive autoloaded Lisp closure in `ol.el'.
;; https://irreal.org/blog/?p=1129
(defun org-linkify (label)
  "Turn a URL into an org-mode link."
  (interactive "sLabel: ")
  (let ((url (thing-at-point 'url))
        (bnds (bounds-of-thing-at-point 'url)))
    (delete-region (car bnds) (cdr bnds))
    (insert (concat "[[" url "][" label "]]"))))

;; https://github.com/alphapapa/yequake
;; https://github.com/alphapapa/org-notepad
;; https://dingyichen.wordpress.com/2014/09/09/sticky-note-solution-that-is-reliable-cross-platfrom-off-line-and-synchronizable/
;; https://www.reddit.com/r/spacemacs/comments/99k6gd/how_do_i_set_up_an_org_mode_config_as_a_private/
;; https://www2.lib.uchicago.edu/keith/software/note/

;; https://emacs.stackexchange.com/questions/5465/how-to-migrate-markdown-files-to-emacs-org-mode-format
;; https://www.bigeekfan.com/post/20171010_hugo_org_functions/
;; 
(defun org-inside-link-p ()
  "Return t if point is inside an org-mode link."
  (save-excursion)
  ;; save our point so we can restore it later
  (let ((restore-point (point)))
    ;; ensure we're not on the first characters of a link
    (while (string-equal "[" (thing-at-point 'char t))
      (forward-char 2))
    ;; ensure we're not on the last characters of a link
    (while (string-equal "]" (thing-at-point 'char t))
      (backward-char 2))
    

    (let ((curr (point)) ;; current position
	        (start nil)    ;; start of org tag
	        (end nil)      ;; end of org tags
	        (next nil))     ;; next org tag.

      
      ;; search for the positions of our link start,
      ;; link end, and the start of another link. 

      (setq start (search-backward "[[" nil t))
      (goto-char curr)
      (setq end (search-forward "]]" nil t))
      (goto-char curr)
      (setq next (search-forward "[[" nil t))
      (goto-char restore-point)
      ;; check if the searches returned a number, if they did
      ;; check that start < curr < end < next.
      (cond
       ((and start end next curr)
	      (< start curr end next))
       ((and start end curr)
	      (< start curr end))
       (t nil)))))

(defun hugo-org-link-to-markdown-link ()
  "Convert an org-mode link under point to a markdown link."
  (interactive)
  (save-excursion)
  ;;make certain we are not sitting on the first two characters
  ;;of the link
  (while (string-equal "[" (thing-at-point 'char t))
    (forward-char 2))

  ;; check that we're actually inside a link. 
  (when (not (org-inside-link-p))
    (error "Not inside a org-mode link."))

  
  (let ((pos1 nil) ; start of org link
	      (pos2 nil) ; end of org link
	      (pos3 nil)) ; separation between url and link txt. 
    (setq pos1 (search-backward "[["))
    (setq pos2 (search-forward "]]"))
    (goto-char pos1)
    (setq pos3 (search-forward "]["))
    (goto-char pos1)
    ;; throw an error if there's no text description. 
    (when (not (< pos1 pos3 pos2))
      (error "Link not in [[url][text]] format"))
    (let ((url (buffer-substring-no-properties (+ pos1 2) (- pos3 2)))
	        (txt (buffer-substring-no-properties pos3 (- pos2 2))))
      (kill-region pos1 pos2)
      (insert (format "[%s](%s)" txt url))
      )
    )
  )

;;
;;  Describe All Variables Key Maps
;;
;; https://emacs.stackexchange.com/questions/59483/how-to-list-a-key-binding-in-any-modes
(defun describe-all-keymaps ()
  "Describe all keymaps in currently-defined variables."
  (interactive)
  (with-output-to-temp-buffer "*keymaps*"
    (let (symbs seen)
      (mapatoms (lambda (s)
                  (when (and (boundp s) (keymapp (symbol-value s)))
                    (push (indirect-variable s) symbs))))
      (dolist (keymap symbs)
        (unless (memq keymap seen)
          (princ (format "* %s\n\n" keymap))
          (princ (substitute-command-keys (format "\\{%s}" keymap)))
          (princ (format "\f\n%s\n\n" (make-string (min 80 (window-width)) ?-)))
          (push keymap seen))))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'my-describe-all-keymaps)))))

(defun get-current-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun run-python-command (str)
  (shell-command-to-string
   (concat "D:/Dev/Tools/Anaconda3/python.exe  D:\\Dev\\Home\\.emacs.d\\private\\python\\GetTitleFromUrl.py "
           (shell-quote-argument ( str )))))

(defun eval-line-in-python ()
  "Evaluates the current line in python, then copies the result to the clipboard."
  (interactive)
  (let ((str (run-python-command (get-current-line))))
    (message str)
    (kill-new str)))


;;
;;  Windows Command Prompt
;;
(defun cmd ()
  (interactive)
  (let ((shell-file-name "cmd.exe"))
    (shell "*Windows Command*")))

;;
;;  Windows Command Prompt
;;
(defun powershell ()
  (interactive)
  (let ((shell-file-name "powershell.exe"))
    (shell "*Power Shell*")))


(message "Ending ~/Home/.emacs.d/private/custom-layer/func.el %s" (format-time-string "%Y-%m-%dTT"))
