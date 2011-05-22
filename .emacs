;;; for Emacs 23

;;* ----- �������� -----{{{1
;; ���顼���˥��顼�ս�ɽ��
(setq debug-on-error t)

;; path {{{2
;; load-path���ɲä���ؿ� (from WEB+DP PRESS Vol.58)
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
;; elisp, conf�ǥ��쥯�ȥ�ȥ��֥ǥ��쥯�ȥ��load-path���ɲ�
(add-to-load-path "elisp" "conf" "colors")

(setq load-path
      (append
        (list
          "/sbin"
          "/usr/sbin"
          "/bin"
          "/usr/bin"
          "/opt/local/bin"
          "/sw/bin"
          "/usr/local/bin"
          (expand-file-name "~/bin")
          (expand-file-name "~/site-lisp")
          (expand-file-name "~/.emacs.d")
          (expand-file-name "~/.emacs.d/bin")
          )
        load-path))

;; encoding {{{2
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; font {{{2
;;; Inconsolata and Takao�����å� {{{3
;(set-face-attribute 'default nil
;                    :family "Inconsolata"
;                    :height 160)
;(set-fontset-font
; nil 'japanese-jisx0208
; (font-spec :family "TakaoExGothic"))
;; Rikty {{{3 (http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html)
(set-face-attribute 'default nil
                   :family "Ricty Discord"
                   :height 160)
(set-fontset-font
nil 'japanese-jisx0208
(font-spec :family "Ricty Discord"))

;; imput {{{2
(setq default-imput-method "MacOSX")

;; key mapping {{{2
;; C-z�ǵե������� {{{3
(define-key global-map (kbd "C-z") 'scroll-down)
;; C-h��Хå����ڡ����˳������ {{{3
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-x ?") 'help-command)
;; Option������Meta�����Ȥ����Ѥ���(for Carbon Emacs) {{{3
(setq mac-option-modifier 'meta)
;; command��option���줫�� {{{3
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
;; C-x b�ǥߥ˥Хåե��˥Хåե������ɽ�� {{{3
(iswitchb-mode t)
(iswitchb-default-keybindings)
;; C-k�ǹ����Τ��� {{{3
(setq kill-whole-line t)
;; C-m��newline-and-indent {{{3
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; M-k�ǥ����ȥХåե����Ĥ��� {{{3
(define-key global-map (kbd "M-k") 'kill-this-buffer)
;; M-K�ǥ����ȥե졼����Ĥ��� {{{3
(define-key global-map (kbd "M-K") 'delete-frame)
;; C-t�ǥ�����ɥ����ڤ��ؤ��� {{{3
(define-key global-map (kbd "C-t") 'other-window)
;; C-T�ǥե졼����ڤ��ؤ��� {{{3
(define-key global-map (kbd "C-T") 'other-frame)
;; ���֤�����˶������� {{{3
(setq-default indent-tabs-mode nil)
;; ������� {{{3
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; visual {{{2
;; ���ѥ��ڡ����ȥ���ʸ���βĻ벽 {{{3
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\��])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")
;; ������������ON {{{3
(blink-cursor-mode t)

;; �����ȥ�С��˥Хåե�̾��ɽ�� {{{3
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; ��ư���β��̤�ɽ�������ʤ� {{{3
(setq inhibit-startup-screen t)

;; �ġ���С���ɽ�� {{{3
(tool-bar-mode 0)

;; �б������̤ζ�Ĵɽ�� {{{3
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "gray40")

;; ���ֹ��ɽ�� {{{3
(global-linum-mode)
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d")

;; �⡼�ɥ饤�� {{{3
(line-number-mode t)                    ; ���ֹ�
(column-number-mode t)                  ; ���ֹ�
(require 'time)                         ; �����ɽ��
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; ��ʬ����ι��ޤ��֤����� {{{3
(setq truncate-partial-width-windows nil)

;; frame {{{3
(setq initial-frame-alist
      (append (list '(foreground-color . "azure3")
		    '(background-color . "black")
		    '(border-color . "black")
		    ;'(mouse-color . "white")
		    ;'(cursor-color . "white")
                    '(cursor-type . bar)
                    '(alpha . (80 80 0 0))
		    '(width . 155)
		    '(height . 60)
		    '(top . 0)
		    '(left . 0)
		    )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; ���顼�ơ��ޤ����� (http://www.nongnu.org/color-theme/) {{{3
(when (require 'color-theme nil t)
  (color-theme-initialize))
(load-file "~/.emacs.d/colors/color-theme-ir-black.el") ;; http://d.hatena.ne.jp/a666666/20100206/1265403745
(color-theme-ir-black)

;; �ԤΥϥ��饤�� (from WEB+DB PRESS Vol.58) {{{3
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "gray5"
      :underline  "gray20"))
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; �����ե������ɽ�� {{{3
(auto-image-file-mode t)

;; backup {{{2
;; ��¸�� {{{3
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacsbackup"))
            backup-directory-alist))

;; ���� {{{3
(setq verison-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)

;; ���֤���¸ {{{3
(require 'desktop)
(desktop-save-mode 1)

;; fontlock {{{2
(global-font-lock-mode t)
;; text-mode fontlock (import from simple-hatena-mode) {{{3
(add-hook 'text-mode-hook
          '(lambda ()
             ;; (require 'hatenahelper-mode)
             ;; (hatenahelper-mode 1)
             (defvar hatena-text-font-lock-keywords nil)
             (defvar hatena-text-slag-face 'hatena-text-slag-face)
             (defvar hatena-text-subtitle-face 'hatena-text-subtitle-face)
             (defvar hatena-text-inline-face 'hatena-text-inline-face)
             (defvar hatena-text-markup-face 'hatena-text-markup-face)
             (defvar hatena-text-link-face 'hatena-text-link-face)

             (font-lock-add-keywords 'text-mode
                                     (list
                                       (list  "^\\(\\*[*a-zA-Z0-9_-]*\\)\\(.*\\)$"
                                              '(1 hatena-text-slag-face t)
                                              '(2 hatena-text-subtitle-face t))
                                       ;; ɬ��[]�ǰϤޤ�Ƥ��ʤ���Фʤ�ʤ����
                                       (list "\\[[*a-zA-Z0-9_-]+\\(:[^\n]+\\)+\\]"
                                             '(0 hatena-text-inline-face t))
                                       ;; ɬ������[]�ǰϤޤ�Ƥ��ʤ��Ƥ�褤���
                                       (list "\\[?\\(id\\|a\\|b\\|d\\|f\\|g\\|graph\\|i\\|idea\\|map\\|question\\|r\\|isbn\\|asin\\)\\(:[a-zA-Z0-9_+:-]+\\)+\\]?"
                                             '(0 hatena-text-inline-face t))
                                       (list  "^\\(:\\)[^:\n]+\\(:\\)"
                                              '(1 hatena-text-markup-face t)
                                              '(2 hatena-text-markup-face t))
                                       (list  "^\\([-+]+\\)"
                                              '(1 hatena-text-markup-face t))
                                       (list  "\\(((\\).*\\())\\)"
                                              '(1 hatena-text-markup-face t)
                                              '(2 hatena-text-markup-face t))
                                       (list  "^\\(>>\\|<<\\|><!--\\|--><\\|>|?[^|]*|\\||?|<\\|=====?\\)"
                                              '(1 hatena-text-markup-face t))
                                       (list  "\\(s?https?://\[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#\]+\\)"
                                              '(1 hatena-text-link-face t))))

             (defface hatena-text-slag-face
                      '((((class color) (background light)) (:foreground "IndianRed"))
                        (((class color) (background dark)) (:foreground "wheat")))
                      "�����Ф���*�����ॹ�����or����å�*��ʬ�Υե�������")

             (defface hatena-text-subtitle-face
                      '((((class color) (background light)) (:foreground "DarkOliveGreen"))
                        (((class color) (background dark)) (:foreground "wheat")))
                      "�����Ф��Υե�������")

             (defface hatena-text-inline-face
                      '((((class color) (background light)) (:foreground "MediumBlue" :bold t))
                        (((class color) (background dark)) (:foreground "wheat" :bold t)))
                      "id��ˡ��[keyword:Emacs]����face")

             (defface hatena-text-markup-face
                      '((((class color) (background light)) (:foreground "DarkOrange" :bold t))
                        (((class color) (background dark)) (:foreground "IndianRed3" :bold t)))
                      "�ϤƤʤΥޡ������åפΥե�������")

             (defface hatena-text-link-face
                      '((((class color) (background light)) (:foreground "DeepPink"))
                        (((class color) (background dark)) (:foreground "wheat")))
                      "��󥯤Υե�������")
             (font-lock-mode 1)
             (font-lock-fontify-buffer)
             ))

;; -------------------- elisp ---------------------- {{{1

;; word-count-mode
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; ����åץܡ��ɤȥ����󥰤ζ�ͭ http://blog.lathi.net/articles/2007/11/07/sharing-the-mac-clipboard-with-emacs {{{2
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; egg.el (git://github.com/bogolisk/egg.git) {{{2
(require 'egg)
(define-key global-map (kbd "C-t") 'other-window) ;; ���Τ�̵���ˤʤ�Τ�

;; (install-elisp "http://www.emacswiki.org/emacs/download/multi-term.el") {{{2
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/zsh"))

;; migemo.el (from WEB+DB PRESS Vol.58) {{{2
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary
        "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

;;; anything (from WEB+DB PRESS Vol.58) {{{2
;; (auto-install-batch "anything") {{{3
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3              ; ����ɽ���ޤǤλ���
   anything-input-idle-delay 0.2        ; �����פ��������ޤǤλ���
   anything-candidate-number-limit 100  ; �������ɽ����
   anything-quick-update t              ; ���䤬¿���Ȥ��δ�®�ٸ���
   anything-enable-shortcuts 'alphabet) ; ��������򥢥�ե��٥åȤ�
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))  ; root���¼¹Ի��Υ��ޥ��
  (require 'anything-match-plugin nil t)
  (and (equal current-language-environment "Japanese")
       (executable-find "cmigemo")
       (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; (anything-read-string-mode 1)    ; M-x��֤�Anything��
    (anything-lisp-complete-symbol-set-timer 150)); lisp����ܥ��䴰����κƸ�������
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))       ; describe-bindings��Anything���֤�������
  (require 'anything-grep nil t))

;; �ɥ�����ȸ����Τ����anything���ޥ�� (WEB+DB PRESS Vol.58) {{{3
(setq anything-for-document-sources
      (list
       anything-c-source-man-pages
       anything-c-source-info-cl
       anything-c-source-info-pages
       anything-c-source-info-elisp
       anything-c-source-apropos-emacs-commands
       anything-c-source-apropos-emacs-functions
       anything-c-source-apropos-emacs-variables))
(defun anything-for-document ()
  "Preconfigured 'anything' for anything-for-document."
  (interactive)
  (anything anything-for-document-sources
            (thing-at-point 'symbol) nil nil nil
            "*anything for document*"))
(global-set-key (kbd "s-d") 'anything-for-document)

;; gist.el (https://github.com/defunkt/gist.el) {{{2
(add-to-list 'load-path "~/.emacs.d/elisp/gist.el")
(require 'gist)

;; anything-gist.el (gist:467982) {{{2
(require 'anything-gist)
   
;; (install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el") ;; {{{2
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el) ;; {{{2
(when (require 'redo+ nil t)
  ;; global-map
  (global-set-key (kbd "C-'") 'redo)) ;C-'��redo

;;** smartchr.el {{{2 http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)

;;** org-mode {{{2
;; org-mode��GTD�˻Ȥ���ˡ #1 - ��¿�ʳФ��� http://d.hatena.ne.jp/t0m0_tomo/20091229/1262082716
(load "org-mode")
(setq org-combined-agenda-icalendar-file "~/org/calendar/org.ics")
(setq org-icalendar-include-todo nil)
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

(defun org-push-daily-my ()
  (interactive)
  (org-export-icalendar-combine-agenda-files)
  (call-process "org2googleCalendar.pl" nil nil nil
                (expand-file-name org-combined-agenda-icalendar-file)))
(define-key global-map [f12] 'org-push-daily-my)

(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;;** screen-lines-mode {{{2
(autoload 'screen-lines-mode "screen-lines"
  "Toggle Screen Lines minor mode for the current buffer." t)
(autoload 'turn-on-screen-lines-mode "screen-lines"
  "Turn on Screen Lines minor mode for the current buffer." t)
(autoload 'turn-off-screen-lines-mode "screen-lines"
  "Turn off Screen Lines minor mode for the current buffer." t)

;; simple-hatena-mode {{{2
(setq load-path
      (cons "~/.emacs.d/elisp/simple-hatena-mode" load-path))
(require 'simple-hatena-mode)
(setq simple-hatena-default-id "Rion778")
(setq simple-hatena-bin "~/local/bin/hw.pl")
(setq simple-hatena-root "~/.s-hatena")

;; latex������google chart api��Ȥä�����ɽ�����Ѵ�
(defun latex-to-google-chart-api ()
  (interactive)
  (replace-regexp "\\[tex:\\(.*?\\)\\]"
    (query-replace-compile-replacement
     "<img src=\"http://chart.apis.google.com/chart?cht=tx&chl=\\,(w3m-url-encode-string \\1)\"/>" t) nil (point-min) (point-max)))
(global-set-key "\C-c\C-l\C-t" 'latex-to-google-chart-api)


(add-hook 'simple-hatena-mode-hook
          '(lambda ()
             (turn-on-screen-lines-mode)
             (require 'hatenahelper-mode)
             (hatenahelper-mode 1)
             (setq simple-hatena-use-timestamp-permalink-flag nil)
             ))

;; reftex-mode {{{2
(add-hook 'yatex-mode-hook
          #'(lambda ()
              (reftex-mode 1)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;** ESS {{{2 
(require 'ess-site)
;; ������ɥ�ʬ������
(defun ess-format-window ()
  (split-window-horizontally)
  (other-window 1)
  (split-window)
  (other-window 1)
  )
(add-hook 'ess-pre-run-hook 'ess-format-window)
(setq auto-mode-alist
      (cons (cons "\\.r$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(setq ess-ask-for-ess-directory nil)
(setq-default inferior-R-program-name "/Library/Frameworks/R.framework/Resources/bin/R")
;; align (http://d.hatena.ne.jp/yag_ays/20090712/1247346981)
(require 'align)
(add-to-list 'align-rules-list
             '(ess-assignment-operator
	       (regexp . "\\(\\s-*\\)<-[^#\t\n]")
	       (repeat . nil)
	       (modes  . '(ess-mode))))

;;** R-object-popup.el {{{2
;;http://sheephead.homelinux.org/2010/03/02/1807/
(require 'ess-R-object-popup)

;;** auto-complete.el {{{2
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)

;;** yasunippet.el {{{2
;(add-to-list 'load-path "~/site-lisp/auto-complete-acr")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/auto-complete-acr/snippets")

;;** auto-complete-acr.el {{{2
(require 'auto-complete)
(global-auto-complete-mode t)
(add-hook 'ess-pre-run-hook 'auto-complete-mode)
(require 'auto-complete-yasnippet)
(require 'auto-complete-acr)
;;** YaTeX {{{2
;; .tex�ե�����Ǽ�ưŪ��yatex-mode
(setq auto-mode-alist 
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; TeXShop�ǥץ�ӥ塼
;(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8"
;      dvi2-command "open -a TexShop")

;; latexmk and Skim
(setq tex-command "latexmk -pdf"
      dvi2-command "open -a Skim")

;; ����������
(setq YaTeX-kanji-code 4)

;; AMS-LaTeX
(setq YaTeX-use-AMS-LaTeX t)

;; GNU MDK {{{2
;; ���ɥѥ����ɲ�
(setq load-path (cons "/opt/local/share/mdk" load-path))
;; .mixal�ե������ɤ߹��ߤǼ�ưŪ��mixal-mode
(autoload 'mixal-mode "mixal-mode" t)
(add-to-list 'auto-mode-alist '("\\.mixal\\'" . mixal-mode))
;; mixvm�λ���
(autoload 'mixvm "mixvm" "mixvm/gud interaction" t)

;;; SLIME {{{2
;; M-x my-slime: ʬ�䤷��������ɥ���slime��ư
;; C-c C-r: �����ϰϤ�slime-repl�����ä�ɾ��
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
(require 'slime-autoloads)
(setq slime-lisp-implementations
     `((sbcl ("/opt/local/bin/sbcl"))
       (abcl ("/opt/local/bin/abcl"))
       (clisp ("/opt/local/bin/clisp"))))
(setq slime-net-coding-system 'utf-8-unix)
(add-hook 'lisp-mode-hook
           (lambda ()
             (global-set-key "\C-cH" 'hyperspec-lookup)
             (cond ((not (featurep 'slime))
                    (require 'slime)
                    (normal-mode)))))
(eval-after-load "slime"
   '(slime-setup '(slime-fancy slime-banner)))
(global-set-key "\C-cs" 'slime-selector)
(defun my-slime (&optional command coding-system)
  "Run slime and split window."
  (interactive)
  (if (< (count-windows) 2)
      (split-window-horizontally)
  )
  (other-window 1)
  (slime command coding-system)
  (other-window 1)
  (global-set-key "\C-cH" 'hyperspec-lookup)
  )
(defun slime-repl-send-region (start end)
  "Send region to slime-repl."
  (interactive "r")
  (let ((buf-name (buffer-name (current-buffer)))
        (sbcl-buf (get-buffer "*slime-repl sbcl*")))
    (cond (sbcl-buf 
           (copy-region-as-kill start end)
           (switch-to-buffer-other-window sbcl-buf)
           (yank)
           (slime-repl-send-input "\n")
           (switch-to-buffer-other-window buf-name))
          (t (message "Not exist *slime-repl sbcl* buffer!")))
    ))
(global-set-key "\C-c\C-r" 'slime-repl-send-region)

;;; ejacs {{{2
;; C-c C-j��js-console��ư
;; C-c r�������ϰϤ�¹� 
(autoload 'js-console "js-console" nil t)
(defun js-console-execute-region (start end)
  "Execute region"
  (interactive "r")
  (let ((buf-name (buffer-name (current-buffer))))
    (copy-region-as-kill start end)
    (switch-to-buffer-other-window "*js*")
    (js-console-exec-input (car kill-ring))
    (switch-to-buffer-other-window buf-name)))
(defun run-js-console-and-split-window ()
  "Run js-console and split window horizontally."
  (interactive)
  (if (< (count-windows) 2)
      (split-window-horizontally)
  )
  (js-console)
  (other-window 1)
  )
(add-hook 'js-mode-hook
          (lambda ()
            (moz-minor-mode 1) ;; ��moz.el
            (local-set-key "\C-c\C-j" 'run-js-console-and-split-window)
            (local-set-key "\C-cr" 'js-console-execute-region)
            ))

;;; moz.el {{{2
; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; tabber.el (http://sourceforge.net/projects/emhacks/) {{{2
;; scratch buffer �ʳ���ޤȤ�ƥ��֤�ɽ������
(require 'cl)
(when (require 'tabbar nil t)
   (setq tabbar-buffer-groups-function
  (lambda (b) (list "All Buffers")))
   (setq tabbar-buffer-list-function
         (lambda ()
           (remove-if
            (lambda(buffer)
              (find (aref (buffer-name buffer) 0) " *"))
            (buffer-list))))
   (tabbar-mode))

;; Ctrl-Tab, Ctrl-Shift-Tab �ǥ��֤��ڤ��ؤ���
 (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
   (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
 (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
   `(defun ,name (arg)
      (interactive "P")
      ,do-always
      (if (equal nil arg)
 	 ,on-no-prefix
        ,on-prefix)))
 (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
 (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
 (global-set-key [(control tab)] 'shk-tabbar-next)
 (global-set-key [(control shift tab)] 'shk-tabbar-prev)

;; �����ѹ�
(set-face-attribute
  'tabbar-default-face nil
  :background "gray30")
 (set-face-attribute
  'tabbar-unselected-face nil
  :background "gray40"
  :foreground "gray"
  :box nil)
 (set-face-attribute
  'tabbar-selected-face nil
  :background "black"
  :foreground "gray"
  :box nil)
 (set-face-attribute
  'tabbar-button-face nil
  :box '(:line-width 1 :color "gray40" :style released-button))
 (set-face-attribute
  'tabbar-separator-face nil
  :height 0.7)

;; w3m {{{2
(require 'w3m-load)
(setq w3m-home-page "http://google.com")
(setq w3m-use-cookies t)
(setq brouwse-url-brouser-function 'w3m-brouse-url)
(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key "\C-t" 'other-window)
            ))
;; HyperSpec��w3m�Ǹ���
(defadvice common-lisp-hyperspec
  (around hyperspec-lookup-w3m () activate)
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration ,window-configuration)))
               (use-local-map hs-map)))))
    ad-do-it))

;;; zlc.el http://d.hatena.ne.jp/mooz/20101003/p1 {{{2
;(require 'zlc)
;(let ((map minibuffer-local-map))
;  (define-key map (kbd "<backtab>") 'zlc-select-previous)
;  (define-key map (kbd "S-<tab>") 'zlc-select-previous)
;  (define-key map (kbd "C-p") 'zlc-select-previous-vertical)
;  (define-key map (kbd "C-n") 'zlc-select-next-vertical)
;  (define-key map (kbd "C-b") 'zlc-select-previous)
;  (define-key map (kbd "C-f") 'zlc-select-next)
;)

;; Yet another incomplete http://d.hatena.ne.jp/tarao/20101011/1286804507 {{{2
(require 'yaicomplete)
(yaicomplete-mode)
