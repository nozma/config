;;* ----- 基本設定 -----{{{1
;; エラー時にエラー箇所表示
(setq debug-on-error t)

;; path {{{2
;; load-pathを追加する関数 (from WEB+DP PRESS Vol.58)
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
;; elisp, confディレクトリとサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf")

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
; ;; for mac
; (set-language-environment  "Japanese")
; (require 'ucs-normalize)
; (prefer-coding-system 'utf-8-hfs)
; (setq file-name-coding-system 'utf-8-hfs)
; (setq locale-coding-system 'utf-8-hfs)
;; other operating system
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; font {{{2
;; Monaco and Osaka {{{3
; (set-face-attribute 'default nil
; 		    :family "Monaco"
; 		    :height 120)
; 		    ;:height 90)
; (set-fontset-font "fontset-default"
; 		  'japanese-jisx0208
; 		  '("Osaka" . "iso10646-1"))
; (set-fontset-font "fontset-default"
; 		  'katakana-jisx0201
; 		  '("Osaka" . "iso10646-1"))
; (setq face-font-rescale-alist
;       '((".*Monaco-bold.*" . 1.0)
; 	(".*Monaco-medium.*" . 1.0)
; 	(".*Osaka-bold.*" . 1.2)
; 	(".*Osaka-medium.*" . 1.2)
; 	("-cdac$" . 1.4)))

;; ;; Inconsolata and Takaoゴシック {{{3
;; (set-default-font "Inconsolata-14")
;; (set-face-font 'variable-pitch "Inconsolata-14")
;; (set-fontset-font
;;  nil 'japanese-jisx0208
;;  (font-spec :family "TakaoExGothic"))
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "TakaoExGothic"))

;; key mapping {{{2
;; C-zで逆スクロール {{{3
(define-key global-map (kbd "C-z") 'scroll-down)
;; C-hをバックスペースに割り当て {{{3
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-x ?") 'help-command)
;; OptionキーをMetaキーとして用いる(for Carbon Emacs) {{{3
(setq mac-option-modifier 'meta)
;; commandとoptionいれかえ {{{3
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
;; C-x bでミニバッファにバッファ候補を表示 {{{3
(iswitchb-mode t)
(iswitchb-default-keybindings)
;; C-kで行全体を削除 {{{3
(setq kill-whole-line t)
;; C-mでnewline-and-indent {{{3
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; M-kでカレントバッファを閉じる {{{3
(define-key global-map (kbd "M-k") 'kill-this-buffer)
;; C-tでウィンドウを切り替える {{{3
(define-key global-map (kbd "C-t") 'other-window)
;; タブの代わりに空白を使用 {{{3
(setq-default indent-tabs-mode nil)
;; 矩形選択 {{{3
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; visual {{{2
;; カーソル点滅ON {{{3
(blink-cursor-mode t)

;; タイトルバーにバッファ名を表示 {{{3
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 起動時の画面を表示させない {{{3
(setq inhibit-startup-screen t)

;; ツールバー非表示 {{{3
(tool-bar-mode 0)

;; 対応する括弧の強調表示 {{{3
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "gray")

;; 行番号の表示 {{{3
(global-linum-mode)
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d")

;; モードライン {{{3
(line-number-mode t)                    ; 行番号
(column-number-mode t)                  ; 列番号
(require 'time)                         ; 時刻の表示
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; ;; カラーテーマの設定 (http://www.nongnu.org/color-theme/) {{{3
;; (when (require 'color-theme nil t)
;;   (color-theme-initialize))

;; frame {{{3
;(setq initial-frame-alist '((width . 198)(height . 68)(top . 0)(left . 2)))
(setq initial-frame-alist
      (append (list '(foreground-color . "azure3")
                    '(background-color . "black")
                    '(border-color . "black")
                    '(mouse-color . "white")
                    '(cursor-color . "white")
                    '(alpha . (75 75 0 0))
		    '(width . 155)
		    '(height . 60)
		    '(top . 0)
		    '(left . 0)
		    )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 行のハイライト (from WEB+DB PRESS Vol.58) {{{3
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode)

;; backup {{{2
;; 保存先 {{{3
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacsbackup"))
            backup-directory-alist))

;; 設定 {{{3
(setq verison-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)

;; 縦分割時の行折り返し設定
(setq truncate-partial-width-windows nil)

;; fontlock {{{2
(global-font-lock-mode t)
;; text-mode fontlock (import from simple-hatena-mode) {{{3
(add-hook 'text-mode-hook
          '(lambda ()
             (require 'hatenahelper-mode)
             (hatenahelper-mode 1)
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
                                       ;; 必ず[]で囲まれていなければならないもの
                                       (list "\\[[*a-zA-Z0-9_-]+\\(:[^\n]+\\)+\\]"
                                             '(0 hatena-text-inline-face t))
                                       ;; 必ずしも[]で囲まれていなくてもよいもの
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
                      "小見出しの*タイムスタンプorスラッグ*部分のフェイス。")

             (defface hatena-text-subtitle-face
                      '((((class color) (background light)) (:foreground "DarkOliveGreen"))
                        (((class color) (background dark)) (:foreground "wheat")))
                      "小見出しのフェイス。")

             (defface hatena-text-inline-face
                      '((((class color) (background light)) (:foreground "MediumBlue" :bold t))
                        (((class color) (background dark)) (:foreground "wheat" :bold t)))
                      "id記法や[keyword:Emacs]等のface")

             (defface hatena-text-markup-face
                      '((((class color) (background light)) (:foreground "DarkOrange" :bold t))
                        (((class color) (background dark)) (:foreground "IndianRed3" :bold t)))
                      "はてなのマークアップのフェイス。")

             (defface hatena-text-link-face
                      '((((class color) (background light)) (:foreground "DeepPink"))
                        (((class color) (background dark)) (:foreground "wheat")))
                      "リンクのフェイス。")
             (font-lock-mode 1)
             (font-lock-fontify-buffer)
             ))

;; -------------------- elisp ---------------------- {{{1

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
   anything-idle-delay 0.3              ; 候補表示までの時間
   anything-input-idle-delay 0.2        ; タイプから再描画までの時間
   anything-candidate-number-limit 100  ; 候補最大表示数
   anything-quick-update t              ; 候補が多いとき体感速度向上
   anything-enable-shortcuts 'alphabet) ; 候補選択をアルファベットで
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))  ; root権限実行時のコマンド
  (require 'anything-match-plugin nil t)
  (and (equal current-language-environment "Japanese")
       (executable-find "cmigemo")
       (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; (anything-read-string-mode 1)    ; M-x補間をAnythingで
    (anything-lisp-complete-symbol-set-timer 150)); lispシンボル補完候補の再検索時間
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))       ; describe-bindingsをAnythingに置き換える
  (require 'anything-grep nil t))

;; ドキュメント検索のためのanythingコマンド (WEB+DB PRESS Vol.58) {{{3
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
   
;; (install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el") ;; {{{2
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el) ;; {{{2
(when (require 'redo+ nil t)
  ;; global-map
  (global-set-key (kbd "C-'") 'redo)) ;C-'でredo

;;** smartchr.el {{{2 http://tech.kayac.com/archive/emacs-tips-smartchr.html
(require 'smartchr)
;; ;;** 行カーソル {{{2 http://blog.iwa-ya.net/2009/06/21/093100
;; (defface hlline-face
;;   '((((class color)
;;       (background dark))
;;      ;;(:background "dark state gray"))
;;      (:background "gray10"
;;                   :underline "gray24"))
;;     (((class color)
;;       (background light))
;;      (:background "ForestGreen"
;;                   :underline nil))
;;     (t ()))
;;   "*Face used by hl-line.")
;; (setq hl-line-face 'hlline-face)
;; ;;(setq hl-line-face 'underline)
;; (global-hl-line-mode)

;;** org-mode {{{2

;; Emacsでメモ・TODO管理 (http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html)
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c\C-r" 'org-remember)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files '("~/memo/notes.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")))

;;** sense-region.el {{{2 (http://taiyaki.org/elisp/sense-region/)
(autoload 'sense-region-on "sense-region"
  "System to toggle region and rectangle." t nil)
(sense-region-on)

;;** screen-lines-mode {{{2
(autoload 'screen-lines-mode "screen-lines"
  "Toggle Screen Lines minor mode for the current buffer." t)
(autoload 'turn-on-screen-lines-mode "screen-lines"
  "Turn on Screen Lines minor mode for the current buffer." t)
(autoload 'turn-off-screen-lines-mode "screen-lines"
  "Turn off Screen Lines minor mode for the current buffer." t)

;;** simple-hatena-mode {{{2
(setq load-path
      (cons "~/.emacs.d/elisp/simple-hatena-mode" load-path))
(require 'simple-hatena-mode)
(setq simple-hatena-default-id "Rion778")
(setq simple-hatena-bin "~/local/bin/hw.pl")
(setq simple-hatena-root "~/.s-hatena")
(add-hook 'simple-hatena-mode-hook
          '(lambda ()
             (turn-on-screen-lines-mode)
             (require 'hatenahelper-mode)
             (hatenahelper-mode 1)
             (setq simple-hatena-use-timestamp-permalink-flag nil)
             ))

;;** Outline-mode-setting {{{2
;; base 
(make-variable-buffer-local 'outline-level)
(setq-default outline-level 'outline-level)
(make-variable-buffer-local 'outline-heading-end-regexp)
(setq-default outline-heading-end-regexp "\n")
(make-variable-buffer-local 'outline-regexp)
(setq-default outline-regexp "[*\f]+")
;; prefixの変更
;; (setq outline-minor-mode-putrefix "\C-c\C-o")
(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key "\C-c\C-o"
				    outline-mode-prefix-map)))

;; text & outline 
; (setq auto-mode-alist
;  (cons ' ( "\\.txt\\'" . outline-mode) auto-mode-alist))

;; emacs-lisp 
(add-hook 'lisp-interaction-mode-hook
 '(lambda ()
    (setq outline-regexp ";;[*\f]+")
    (outline-minor-mode t)))
(add-hook 'emacs-lisp-mode-hook
 '(lambda ()
    (setq outline-regexp ";;[*\f]+")
    (outline-minor-mode t)))

;; YaTeX mode 
(add-hook 'yatex-mode-hook
          '(lambda () (outline-minor-mode t)))
(make-variable-buffer-local 'outline-regexp)
(add-hook
 'yatex-mode-hook
 (function
  (lambda ()
    (progn
      (setq outline-level 'latex-outline-level)
      (setq outline-regexp
            (concat "[ \t]*\\\\\\(documentstyle\\|documentclass\\|"
                    "chapter\\|section\\|subsection\\|subsubsection\\)"
                    "\\*?[ \t]*[[{]")
     )))))
(make-variable-buffer-local 'outline-level)
(setq-default outline-level 'outline-level)
(defun latex-outline-level ()
  (save-excursion
    (looking-at outline-regexp)
    (let ((title (buffer-substring (match-beginning 1) (match-end 1))))
      (cond ((equal (substring title 0 4) "docu") 15)
            ((equal (substring title 0 4) "chap") 0)
            ((equal (substring title 0 4) "appe") 0)
            (t (length title))))))

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
;; ウィンドウ分割設定
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
;; .texファイルで自動的にyatex-mode
(setq auto-mode-alist 
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; TeXShopでプレビュー
;(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8"
;      dvi2-command "open -a TexShop")

;; latexmk and Skim
(setq tex-command "latexmk -pdfps"
      dvi2-command "open -a Skim")

;; 漢字コード
(setq YaTeX-kanji-code 4)

;; AMS-LaTeX
(setq YaTeX-use-AMS-LaTeX t)

;; GNU MDK {{{2
;; ロードパスの追加
(setq load-path (cons "/opt/local/share/mdk" load-path))
;; .mixalファイル読み込みで自動的にmixal-mode
(autoload 'mixal-mode "mixal-mode" t)
(add-to-list 'auto-mode-alist '("\\.mixal\\'" . mixal-mode))
;; mixvmの使用
(autoload 'mixvm "mixvm" "mixvm/gud interaction" t)

;;; SLIME {{{2

(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
(require 'slime-autoloads)
(setq slime-lisp-implementations
     `((sbcl ("/opt/local/bin/sbcl"))
       (abcl ("/opt/local/bin/abcl"))
       (clisp ("/opt/local/bin/clisp"))))
(add-hook 'lisp-mode-hook
           (lambda ()
             (global-set-key "\C-cH" 'hyperspec-lookup)
             (cond ((not (featurep 'slime))
                    (require 'slime)
                    (normal-mode)))))

(eval-after-load "slime"
   '(slime-setup '(slime-fancy slime-banner)))

(global-set-key "\C-cs" 'slime-selector)

;;; ejacs {{{2
;; C-c C-jでjs-consoleを起動
;; C-c C-rで選択範囲をインタプリタで実行 
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
  (split-window-horizontally)
  (js-console)
  (other-window 1)
  )
(add-hook 'js-mode-hook
          (lambda ()
            (moz-minor-mode 1)
            (local-set-key "\C-c\C-j" 'run-js-console-and-split-window)
            (local-set-key "\C-c\C-r" 'js-console-execute-region)
            ))

;;; moz.el {{{2
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

