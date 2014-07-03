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
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

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
(display-time-mode t)
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
;; ( add-hook 'text-mode-hook
;;           (require 'hatena-markup-mode)
;;           (setq text-mode 'hatena:markup-mode))

;; ===== elisp ==================================================
;; auto-install -------------------------------------------------
;; (when (require 'auto-install nil t)
;;   (setq auto-install-directory "~/.emacs.d/elisp/")
;;   (auto-install-update-emacswiki-package-name t)
;;   (auto-install-compatibility-setup))

;; ;; latex数式をgoogle chart apiを使った数式表現に変換-------------
;; (defun latex-to-google-chart-api ()
;;   (interactive)
;;   (replace-regexp "\\[tex:\\(.*?\\)\\]"
;;     (query-replace-compile-replacement
;;      "<img src=\"http://chart.apis.google.com/chart?cht=tx&chl=\\,(url-hexify-string \\1)\"/>" t) nil (point-min) (point-max)))
;; (global-set-key "\C-c\C-l\C-t" 'latex-to-google-chart-api)
;; ;; 逆変換
;; (defun google-chart-api-to-latex ()
;;   (interactive)
;;   (replace-regexp "<img src=\"http://chart.apis.google.com/chart\\?cht=tx&chl=\\(.*?\\)\"/>"
;;     (query-replace-compile-replacement
;;      "[tex:\\,(url-unhex-string \\1)]" t) nil (point-min) (point-max)))
;; (global-set-key "\C-c\C-t\C-l" 'google-chart-api-to-latex)


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
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
;; (ac-config-default)

;; tabber.el (http://sourceforge.net/projects/emhacks/) ---------
;; scratch buffer 以外をまとめてタブに表示する
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
;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
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

;; 外観変更
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

;; ;; Yet another incomplete ---------------------------------------
;; ;; http://d.hatena.ne.jp/tarao/20101011/1286804507 
;; (require 'yaicomplete)
;; (yaicomplete-mode)                      

;; hatena-diary -------------------------------------------------
(require 'hatena-diary)
(require 'hatena-markup-mode)
(setq hatena:d:major-mode 'hatena:markup-mode)
(require 'hatena-multi-mode)
(add-hook 'hatena:markup-mode-hook #'hatena:multi-mode)
;; ショートカット
(global-set-key "\C-chl" 'hatena:d:list)
(global-set-key "\C-chdl" 'hatena:d:list-draft)
(global-set-key "\C-chn" 'hatena:d:new)
(global-set-key "\C-chdn" 'hatena:d:new-draft)

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
