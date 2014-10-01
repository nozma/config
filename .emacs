;; for Emacs 24

;;;; ======= Basic Settings =====================================

;;; path --------------------------------------------------------
;; load-pathを追加する関数 (from WEB+DP PRESS Vol.58)
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
;; elisp, confその他サブディレクトリの追加
(add-to-load-path "elisp" "conf" "colors" "public_repos")

;; backup -------------------------------------------------------
;; 保存先
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacsbackup"))
            backup-directory-alist))
;; 保存設定
(setq verison-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)
;; 状態を保存
(require 'desktop)
(desktop-save-mode 1)

;;; encoding ----------------------------------------------------
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;; imput -------------------------------------------------------
(setq default-imput-method "MacOSX")
;;; key mapping
(define-key global-map (kbd "C-z") 'scroll-down) ; C-z: 逆スクロール
(keyboard-translate ?\C-h ?\C-?)                 ; C-h: backspace
(global-set-key (kbd "C-x ?") 'help-command)     ; C-x ?: help
;; command<->option
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
(iswitchb-mode t)                       ; C-x b: バッファ候補選択
(setq kill-whole-line t)                ; C-k: 行削除
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "M-k") 'kill-this-buffer)
(define-key global-map (kbd "M-K") 'delete-frame)
(define-key global-map (kbd "C-t") 'other-window)
(setq-default indent-tabs-mode nil)     ; tabを空白に
;; 矩形選択(cua-mode)
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;; visual ------------------------------------------------------
;; font
;; Rikty (http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html)
(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 160)
(set-fontset-font
 nil 'japanese-jisx0208
(font-spec :family "Ricty"))
;; 空白の可視化
;(whitespace-mode)
;; タイトルフォーマット
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
(setq inhibit-startup-screen t)         ; スタートアップ画面非表示
(tool-bar-mode 0)                       ; ツールバー非表示
;; 対応するカッコの強調表示
;; (setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'single)
;; 行番号表示
(global-linum-mode)
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d: ")
;; mode-line
(line-number-mode t)                    ; 行
(column-number-mode t)                  ; 列
(require 'time)                         ; 時刻
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)                     ; 時間表示
(size-indication-mode t)                  ; ファイルサイズ
(setq truncate-partial-width-windows nil) ; 縦分割時の折り返し
;;frame
(setq initial-frame-alist
      (append (list '(foreground-color . "azure3")
                    '(background-color . "black")
                    '(border-color . "black")
                    ;'(mouse-color . "white")
                    ;'(cursor-color . "white")
                    '(cursor-type . bar)
                    '(alpha . (80 80 0 0))
                    '(width . 158)
                    '(height . 57)
                    '(top . 0)
                    '(left . 0)
                    )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)
;; 現在行をハイライト (from WEB+DB PRESS Vol.58)
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "gray10"
      :underline  "gray20"))
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)
;; fontlock
(global-font-lock-mode t)
(add-hook 'text-mode-hook
          (require 'hatena-markup-mode)
          (setq text-mode 'hatena:markup-mode))

;; ===== elisp ==================================================
;; auto-install -------------------------------------------------
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))
;; redo+.el
;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")
(when (require 'redo+ nil t)
  ;; C-.にredoをセット
  (global-set-key (kbd "C-.") 'redo))

;; package.el ---------------------------------------------------
;; リポジトリの追加
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

;; paredit ------------------------------------------------------
;; カッコの対応を保持して編集
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; smart-compile.el ---------------------------------------------
(when (require 'smart-compile nil t)
  (global-set-key "\C-x\C-x" 'smart-compile))

;; open-junk-file.el --------------------------------------------
;; 試行錯誤用ファイルを開く
(require 'open-junk-file)
(global-set-key (kbd "C-x C-z") 'open-junk-file)

;; lispxmp.el ---------------------------------------------------
;; S式の評価結果を注釈する
(require 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; latex数式をgoogle chart apiを使った数式表現に変換-------------
(defun latex-to-google-chart-api ()
  (interactive)
  (replace-regexp "\\[tex:\\(.*?\\)\\]"
    (query-replace-compile-replacement
     "<img src=\"http://chart.apis.google.com/chart?cht=tx&chl=\\,(url-hexify-string \\1)\"/>" t) nil (point-min) (point-max)))
(global-set-key "\C-c\C-l\C-t" 'latex-to-google-chart-api)
;; 逆変換
(defun google-chart-api-to-latex ()
  (interactive)
  (replace-regexp "<img src=\"http://chart.apis.google.com/chart\\?cht=tx&chl=\\(.*?\\)\"/>"
    (query-replace-compile-replacement
     "[tex:\\,(url-unhex-string \\1)]" t) nil (point-min) (point-max)))
(global-set-key "\C-c\C-t\C-l" 'google-chart-api-to-latex)

;; リージョン内の行数と文字数をモードラインに表示する------------
;; d.hatena.ne.jp/sonota88/20110224/1298557375
;; 備考 - 改行も1文字としてカウントされる。
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;;(count-lines-region (region-beginning) (region-end)) ;; これだとエコーエリアがチラつく
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; ESS ----------------------------------------------------------
(require 'ess-site)
;; ウィンドウ分割設定
(defun ess-format-window ()
  (split-window-horizontally)
  (other-window 1)
  (split-window)
  (other-window 1)
  (define-key global-map (kbd "C-t") 'other-window)
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

;; auto-complete.el ---------------------------------------------
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (ac-config-default))

;; tabbar.el ----------------------------------------------------
;; 設定参考：http://hico-horiuchi.hateblo.jp/entry/20121208/1354975316
(require 'tabbar)
(tabbar-mode)
(global-set-key [(control tab)] 'tabbar-forward)  ; 次のタブ
(global-set-key [(control shift tab)] 'tabbar-backward) ; 前のタブ
;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)
;; グループを使わない
(setq tabbar-buffer-groups-function nil)
;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;; 色設定
(set-face-attribute ; バー自体の色
  'tabbar-default nil
   :background "white"
   :family "Inconsolata"
   :height 1.0)
(set-face-attribute ; アクティブなタブ
  'tabbar-selected nil
   :background "black"
   :foreground "white"
   :weight 'bold
   :box nil)
(set-face-attribute ; 非アクティブなタブ
  'tabbar-unselected nil
   :background "white"
   :foreground "black"
   :box nil)
;; 幅設定
(setq tabbar-separator '(1.5))

;; ;; Yet another incomplete ---------------------------------------
;; http://d.hatena.ne.jp/tarao/20101011/1286804507 
(require 'yaicomplete)
(yaicomplete-mode)

;; ;; hatena-diary -------------------------------------------------
;; (require 'hatena-diary)
;; (require 'hatena-markup-mode)
;; (setq hatena:d:major-mode 'hatena:markup-mode)
;; (require 'hatena-multi-mode)
;; (add-hook 'hatena:markup-mode-hook #'hatena:multi-mode)
;; ;; ショートカット
;; (global-set-key "\C-chl" 'hatena:d:list)
;; (global-set-key "\C-chdl" 'hatena:d:list-draft)
;; (global-set-key "\C-chn" 'hatena:d:new)
;; (global-set-key "\C-chdn" 'hatena:d:new-draft)

;; simple-hatena-mode -------------------------------------------
;; (setq load-path
;;       (cons "~/.emacs.d/elisp/simple-hatena-mode" load-path))
;; (require 'simple-hatena-mode)
;; (setq simple-hatena-default-id "Rion778")
;; (setq simple-hatena-bin "~/local/bin/hw.pl")
;; (setq simple-hatena-root "~/.s-hatena")
;; (add-hook 'simple-hatena-mode-hook
;;           '(lambda ()
;;              (require 'hatenahelper-mode)
;;              (hatenahelper-mode 1)
;;              (setq simple-hatena-use-timestamp-permalink-flag nil)
;;              ))

;; elpy ----------------------------------------------------------
(package-initialize)
(elpy-enable)
