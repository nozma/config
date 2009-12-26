;; ----- 基本設定 -----{{{1
;; base {{{2
(setq fill-colmn 80)
;; 日本語設定
(set-language-environment "Japanese")
;; エンコード
(set-default-coding-systems 'utf-8)
;; C-hをバックスペースに割り当て
(define-key global-map "\C-H" 'backward-delete-char)
;; タイトルバーにバッファ名を表示
(setq frame-title-format "%b")
;; バックアップファイル保存先設定
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacsbackup"))
            backup-directory-alist))
;; 起動時の画面を表示させない
(setq inhibit-startup-message t)
;; バックアップファイルを過去5回分残す
(setq verison-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)
;(tool-bar-mode -1)              ;ツールバー非表示
;(menu-bar-mode -1)           ;メニューバー非表示

;; window設定 {{{2
(setq default-frame-alist
      (append (list '(foreground-color . "azure3")
            '(background-color . "black")
            '(border-color . "black")
            '(mouse-color . "white")
            '(cursor-color . "white")
            '(alpha . (85 85 40 40))
            '(width . 177)
            '(height . 45)
            '(top . 0)
            '(left . 0)
            )
              default-frame-alist))

;; ----- elisp ----- {{{1
;; outline-mode-setting;;{{{2
;; base 
(make-variable-buffer-local 'outline-level)
(setq-default outline-level 'outline-level)
(make-variable-buffer-local 'outline-heading-end-regexp)
(setq-default outline-heading-end-regexp "\n")
(make-variable-buffer-local 'outline-regexp)
(setq-default outline-regexp "[*\f]+")
(setq outline-minor-mode-prefix "\C-c\C-o")
;; text & outline 
(setq auto-mode-alist
 (cons ' ( "\\.txt\\'" . outline-mode) auto-mode-alist))
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
           '(lambda ()
              (reftex-mode 1)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
;; ESS {{{2 
;; 拡張子が.Rのファイルで自動的にR-mode
(setq auto-mode-alist
     (cons (cons "\\.R$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload 'R "ess-site" nil 'interactive)
(setq ess-ask-for-ess-directory nil)
(setq ess-pre-run-hook
      '((lambda ()
          (setq default-process-coding-system '(utf8 . utf8))
          )))
;; Rへのパス
(setq-default inferior-R-program-name "/Library/Frameworks/R.framework/Resources/bin/R")
;; M-x align
(require 'align)
(add-to-list 'align-rules-list
             '(ess-assignment-operator
                (regexp . "\\(\\s-*\\)<-[^#\t\n]")
                (repeat . nil)
                (modes  . '(ess-mode))))
;; ウィンドウ分割設定
(defun ess:format-window-1 ()
  (split-window-horizontally)
  (other-window 1)
  (split-window)
  (other-window 1)
  )
(add-hook 'ess-pre-run-hook 'ess:format-window-1)

;; YaTeX {{{2
;; .texファイルで自動的にyatex-mode
(setq auto-mode-alist 
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; TeXShopでプレビュー
(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8"
      dvi2-command "open -a TexShop")
;; 漢字コード
(setq YaTeX-kanji-code 4)
;; PATH
(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH"
        (concat '"/usr/local/bin:" (getenv "PATH")))
