;; https://www.emacswiki.org/emacs/NTEmacsWithCygwin#toc1
;; https://sourceware.org/legacy-ml/cygwin/2012-05/msg00114.html
;; bash: cannot set terminal process group (-1): Inappropriate ioctl for device
;; bash: no job control in this shell
;; https://stackoverflow.com/questions/11821378/what-does-bashno-job-control-in-this-shell-mean/11824420
;; https://cygwin.cygwin.narkive.com/DtP2JgPQ/bash-under-emacs-gives-cannot-set-terminal-process-group
;; https://sourceware.org/legacy-ml/cygwin/2012-02/msg00831.html
;; https://stackoverflow.com/questions/9471341/emacs-shell-command-outputting-cannot-set-terminal-process-group-and-no-job-c/9500684
;; https://emacs.stackexchange.com/questions/3447/cannot-set-terminal-process-group-error-when-running-bash-script
;; https://www.johndcook.com/blog/2016/11/30/setting-up-emacs-shell-on-a-mac/
;; https://stackoverflow.com/questions/6532998/how-to-run-multiple-shells-on-emacs
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; https://develop.spacemacs.org/layers/+tools/shell/README.html
;; Set Cygwin

;; (require 'epa-file)
;;    (epa-file-enable)

;;(setq load-path (cons (expand-file-name "D:/Dev/emacs/.emacs.d/private/custom-layer") load-path))
;;(load-file "D:/Dev/emacs/.emacs.d/private/custom-layer/cygwin-support.el")

;; https://www.emacswiki.org/emacs/MsWindowsNetworkPrinter
;; Printer Name "ET-2750 Series(Network)"
;; Port EP41FD39ET-2750 SERIES


;; if you map to the tcp/ip port, then once the printer
;; is installed, share it from the printer properties dialog, and
;; assign the SHARE name to 'MyPrinter' or similar
(defconst PRINTER_NAME "ET-2750Series"
  "printer to use within emacs")

;; now build the network printer name
(defconst MY_PRINTER  (concat "//" (getenv "COMPUTERNAME") "/" PRINTER_NAME)
  "Point to the printer emacs is to use. Defaults to host/`PRINTER_NAME'")

(defconst MY_PS_PRINTER MY_PRINTER
  "Point to the ps printer emacs is to use. Defaults to `MY_PRINTER'")

(setq printer-name MY_PRINTER)
;;(setq realgud:pdb-command-name "python -m pdb")

;; Enable Defaults
(cua-mode t)
(cua-selection-mode t)
(cua-rectangle-mark-mode t)
(show-paren-mode t)
(menu-bar-mode t)
(tab-bar-mode t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq next-line-add-newlines t)
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
;;--(setq epg-gpg-program "gpg")
;; on Windows or Windows Subsystem Linux (WSL) the local gnutls-utils may not support TLS1.3
;; https://emacs.stackexchange.com/questions/54427/failed-to-update-packages-getting-error-gnutls-error-process-elpa-gnu-org-5
;; (setq gnutls-algorithm-priority "normal:-vers-t1s1.3")
;;--(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2")
(require 'server)
(unless (server-running-p) (server-start))
(add-to-list 'exec-path  "D:/Dev/Tools/bin/bin")
(add-to-list 'exec-path  "D:/Dev/Tools/msys64/mingw64/bin")
;; try hunspell at first
;; if hunspell does NOT exist, use aspell
;;(executable-find "hunspell")
(setq ispell-program-name "hunspell")
;; "en_US" is key to lookup in `ispell-local-dictionary-alist'.
;; Please note it will be passed as default value to hunspell CLI `-d` option
;; if you don't manually setup `-d` in `ispell-local-dictionary-alist`
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "D:/Dev/Tools/dict/en_US.aff")))
(setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
(flyspell-mode 1)

;;(setq load-path (cons (expand-file-name "D:/Dev/Home/.emacs.d/private/custom-layer") load-path))
;;(load-file "D:/Dev/Home/.emacs.d/private/custom-layer/cygwin-support.el")

(setq flycheck-python-pycompile-executable "D:/Dev/anaconda3/python.exe")
(custom-set-variables
 '(flycheck-python-flake8-executable "python")
 '(flycheck-python-pycompile-executable "python")
 '(flycheck-python-pylint-executable "python"))

;; https://github.com/sebastiencs/company-box/
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))

;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; https://www.mattduck.com/lsp-python-getting-started.html
;; https://utcc.utoronto.ca/~cks/space/blog/python/PythonPylspNotes
;; https://emacs-lsp.github.io/lsp-mode/page/languages/
;; https://github.com/atom-community/ide-python/issues/134
;; M-x lsp-install-server
;; (use-package lsp-mode
;;   :config
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)))
;;   :hook
;;   ((python-mode . lsp)
;;    (setq lsp-clients-pylsp-library-directories "d:/dev/tools/anaconda3/lib/site-packages")
;;    (setq lsp-pyls-plugins-flake8-enabled t)))

;; (setq lsp-pyls-plugins-flake8-enabled t)

;; (use-package lsp-ui
;;   :config (setq lsp-ui-sideline-show-hover t
;;                 lsp-ui-sideline-delay 0.5
;;                 lsp-ui-doc-delay 5
;;                 lsp-ui-sideline-ignore-duplicates t
;;                 lsp-ui-doc-position 'bottom
;;                 lsp-ui-doc-alignment 'frame
;;                 lsp-ui-doc-header nil
;;                 lsp-ui-doc-include-signature t
;;                 lsp-ui-doc-use-childframe t)
;;   :commands lsp-ui-mode)

;; https://www.reddit.com/r/emacs/comments/bjrd3f/how_can_i_unlock_folder_from_lspmode_blacklist/
;; M-x lsp-workspace-folders-add 
;;(setf (lsp-session-folders-blacklist (lsp-session)) nil)
;;(lsp--persist-session (lsp-session))
;;(setf (lsp-session-folders-blacklist (lsp-session)) nill)
;; (lsp--persist-session (lsp-session))

;; https://github.com/emacs-lsp/dap-mode
;; (require 'dap-python)
;;(setq dap-python-debugger "debugpy")
;;(dap-mode 1)

;; The modes below are optional

;;(dap-ui-mode 1)
;; enables mouse hover support
;(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
;;(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
;;(dap-ui-controls-mode 1)

;; (dap-register-debug-template "My App"
;;                              (list :type "python"
;;                                    :args "-i"
;;                                    :cwd nil
;;                                    :env '(("DEBUG" . "1"))
;;                                    :target-module (expand-file-name "D:/Dev/Projects/Python/test")
;;                                    :request "launch"
;;                                    :name "My App"))

;; example of setting env var named “path”, by appending a new path to existing path
(setenv "PATH"
        (concat
         "D:/Dev/Tools/Anaconda3" ";"
         (getenv "PATH")
         )
        )		

;; pip install importmagic epc
;; show env var named path
(getenv "PATH")

(message "Ending ~/Home/private/custom-layer/custom.el %s" (format-time-string "%Y-%m-%dTT"))












;https://www.wesleyan.edu/admission/apply/application-process.html
;;https://admissions.unt.edu/international/how-to-apply
;;https://www.uta.edu/admissions/apply/when-to-apply
;;https://admissions.web.baylor.edu/admissions/incoming-freshman/application-process
;;https://www.usnews.com/best-colleges/the-university-of-texas-at-dallas-9741/applying#:~:text=The%20application%20deadline%20is%20May,a%20very%20important%20academic%20factor.
;https://www.smu.edu/admission/financialaid#:~:text=When%20you%20apply%20to%20SMU,Decision%20II%20and%20Regular%20Decision.
;https://www.smu.edu/Admission/Apply/FirstYear
;;https://www.smu.edu/Admission/Apply/FirstYear/Early-Decision
;;https://www.smu.edu/Admission/Apply/FirstYear/DatesandDeadlines

;universities near me