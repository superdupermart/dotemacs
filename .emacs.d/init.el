;; ;; cedet =====================================================
;; (load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;; (global-ede-mode t)
;; (semantic-load-enable-code-helpers)
;; (global-srecode-minor-mode 1)

;; ;; ecb =======================================================
;; (add-to-list 'load-path "~/.emacs.d/ecb-snap/")
;; (require 'ecb)
;; (setq stack-trace-on-error t)

;; (define-key ecb-mode-map (kbd "M-1") 'ecb-goto-window-directories)
;; (define-key ecb-mode-map (kbd "M-2") 'ecb-goto-window-sources)
;; (define-key ecb-mode-map (kbd "M-3") 'ecb-goto-window-methods)
;; (define-key ecb-mode-map (kbd "M-4") 'ecb-goto-window-history)
;; (define-key ecb-mode-map (kbd "M-5") 'ecb-goto-window-compilation)
;; (define-key ecb-mode-map (kbd "M-0") 'ecb-goto-window-edit1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup-files"))))
 '(blink-cursor-mode nil)
 '(cua-enable-cua-keys (quote shift))
 '(cua-mode t nil (cua-base))
 '(current-language-environment "Korean")
 '(custom-enabled-themes (quote (adwaita)))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-excluded-directories-regexps (quote ("^\\.$" "^\\.\\.$")))
 '(ecb-ignore-pop-up-frames (quote always))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(ecb-source-path (quote (("~/dev/" "~/dev/"))))
 '(ecb-sources-sort-method (quote extension))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-do-not-leave-window-after-select (quote (ecb-methods-buffer-name ecb-sources-buffer-name ecb-directories-buffer-name ecb-history-buffer-name)))
 '(ecb-tree-make-parent-node-sticky nil)
 '(ecb-use-recursive-edit t)
 '(ecb-windows-width 0.2)
 '(geiser-racket-binary "/Applications/Racket_v5.2/bin/racket")
 '(geiser-racket-collects (quote ("/Applications/Racket_v5.2/collects")))
 '(ido-mode (quote both) nil (ido))
 '(mouse-wheel-progressive-speed nil)
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; macosx 에서 emacs 를 쓸때 bash의 PATH 와 eshell 의 PATH 를 동일하게 맞춰준다.
(add-hook 'eshell-mode-hook
          'lambda nil
          (let ((bashpath (shell-command-to-string "/bin/bash -l -c 'printenv PATH'")))
            (let ((pathlst (split-string bashpath ":")))
              (setq exec-path pathlst))
            (setq eshell-path-env bashpath)
            (setenv "PATH" bashpath)))

(add-to-list 'load-path "~/.emacs.d/")

(prefer-coding-system 'utf-8)

(when (<= emacs-major-version 23)
  ;; emacs 23 설정 코드
  
  (set-background-color "#F1F1F1")
  
  ;; 테마 설정
  ;; (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
  ;; (add-to-list 'load-path "~/.emacs.d/color-theme-adwaita")
  ;; (require 'color-theme)
  ;; (require 'color-theme-tomorrow)
  ;; (require 'color-theme-gnome-3-adwaita)
  ;; (color-theme-initialize)
  ;; (load-file "~/.emacs.d/color-theme-railscasts.el")
  ;; (color-theme-railscasts)
  ;; ;(color-theme-tomorrow)
  )
(when (>= emacs-major-version 24)
  ;; emacs 24 설정 코드
  
  ;; elpa 시스템 많이 부족하다. 좀 더 발전하면 써야겠다.
  (require 'package)
  (add-to-list 'package-archives 
  	       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;; 	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (defvar my-packages '(starter-kit
                        starter-kit-bindings
                        starter-kit-lisp
                        clojure-mode
			nrepl)
    "A list of packages to ensure are installed at launch.")
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  )

;(setenv "PATH" "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin")

(defun irc-connect ()
  "connect to HanIRC server"
  (interactive)
  (erc :server "irc.hanirc.org"
       :port 6667
       :nick "irus"
       :full-name "irus")
  (erc :server "irc.freenode.net"
       :port 6667
       :nick "irus"))

(setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#wiki" "#lisp")
	    ("hanirc.org" "#linux" "#ubuntu")))

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key [?\S- ] 'toggle-input-method)
(global-set-key (kbd "S-SPC") 'toggle-input-method)

(windmove-default-keybindings 'meta)

(global-auto-revert-mode t)

;; 검색해 되돌아 오기(C--)
(defun memory-and-search ()
  (interactive)
  (when buffer-file-name
    (bookmark-set "search-point"))
  (isearch-forward))

(defun memory-and-search-backward ()
  (interactive)
  (when buffer-file-name
    (bookmark-set "search-point"))
  (isearch-backward))

(defun back-to-search-point ()
  (interactive)
  (bookmark-jump "search-point"))

(global-set-key (kbd "C-s") 'memory-and-search)
(global-set-key (kbd "C-r") 'memory-and-search-backward)
(global-set-key (kbd "C--") 'back-to-search-point)

;; scroll one line at a time (less "jumpy" than defaults)
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;(setq scroll-step 1) ;; keyboard scroll one line at a time

;; ;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 85))
;; (add-to-list 'default-frame-alist '(alpha 85 85))

;; Command 키를 Meta 키로 사용
(setq mac-command-modifier 'meta)

(setq ring-bell-function
      (lambda nil
	(message"")))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key [home] 'smart-beginning-of-line)

(set-face-font 'default "Monaco-14")
;(set-face-font 'default "Dejavu Sans Mono-14")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  '("NanumGothicCoding" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  '("NanumGothicCoding" . "iso10646-1"))
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
;;                   '("Malgun Gothic" . "iso10646-1"))
;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
;;                   '("Malgun Gothic" . "iso10646-1"))
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
;;                   '("NanumMyeongjo" . "iso10646-1"))
;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
;;                   '("NanumMyeongjo" . "iso10646-1"))
(set-fontset-font "fontset-default" 'kana
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'han
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
;(global-set-key (kbd "<f8>") 'switch-to-minibuffer-window)

;; for Scheme
;(require 'quack)
(load-file "~/.emacs.d/geiser/elisp/geiser.el")

;; for Lisp
(add-to-list 'load-path "~/.emacs.d/slime/")
(require 'slime-autoloads)
;(require 'slime)
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path "~/.emacs.d/slime/contrib")
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
    (define-key slime-repl-mode-map (kbd "M-RET") 'electrify-return-if-match)
    (setq slime-net-coding-system 'utf-8-unix)))
;(setq inferior-lisp-program "clisp")
;(setq inferior-lisp-program "sbcl")
(setq inferior-lisp-program "/usr/local/share/ccl/dx86cl64 -K utf-8")
;; (setq slime-lisp-implementations
;;       `((sbcl ("/usr/local/bin/sbcl"))))
;;        (clisp ("/opt/local/bin/clisp"))))

(add-hook 'lisp-mode-hook
           (lambda ()
             (cond ((not (featurep 'slime))
                    (require 'slime) 
                    (normal-mode)))))

;(define-key slime-mode-map (kbd "C-c C-n") 'slime-switch-to-output-buffer)

;(setq default-input-method "korean-hangul")

;; (setq inferior-lisp-program "sbcl")
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-tramp slime-asdf))

;;; Function key binding
;; (global-set-key [f2] 'slime)
;; (global-set-key [f3] 'slime-repl-clear-buffer)
;; (global-set-key [f4] 'slime-repl-quit)

;; (global-set-key [f5] 'comment-region)
;; (global-set-key [f6] 'uncomment-region)

;; (global-set-key [f7] 'previous-buffer)
;; (global-set-key [f8] 'next-buffer)

;; (global-set-key [f9] 'goto-line)
;; (global-set-key [f12] 'shell)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'geiser-repl-mode-hook      (lambda () (paredit-mode +1)))

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))
;; Using local-set-key in a mode-hook is a better idea.
;(global-set-key (kbd "M-RET") 'electrify-return-if-match)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode t)
	    (turn-on-eldoc-mode)
	    (eldoc-add-command
	     'paredit-backward-delete
	     'paredit-close-round)
	    (local-set-key (kbd "RET") 'electrify-return-if-match)
	    (eldoc-add-command 'electrify-return-if-match)
	    (show-paren-mode t)))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (paredit-mode t)
	    (turn-on-eldoc-mode)
	    (eldoc-add-command
	     'paredit-backward-delete
	     'paredit-close-round)
	    (local-set-key (kbd "RET") 'electrify-return-if-match)
	    (eldoc-add-command 'electrify-return-if-match)
	    (show-paren-mode t)))

;; ; slime-repl 에서는 작동을 안하네. 아아악.. 왜 안되냐.
;; ; 아마도 RET 와 M-RET 를 slime 메뉴바에서 쓰고 있기 때문인것 같다.
;; ; 확실히는 모르겠지만 추정해보면 그렇다. 임시로 M-] 에 할당해놓았다.
;; (add-hook 'slime-repl-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "M-]") 'electrify-return-if-match)))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (local-set-key (kbd "RET") 'electrify-return-if-match)))

; geiser-repl 에서는 잘됨.
(add-hook 'geiser-repl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-RET") 'electrify-return-if-match)))

;; run-scheme
(setq scheme-program-name "/Applications/MIT_GNU_Scheme.app/Contents/Resources/mit-scheme")
(require 'xscheme)

;(setq scheme-program-name "/Applications/Racket_v5.2/bin/racket")



; fullscreen for mac ========================================
(require 'maxframe)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (restore-frame)
    (maximize-frame)))

(global-set-key (kbd "M-RET") 'my-toggle-fullscreen)



; markdown mode =============================================
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))































;; (add-to-list 'load-path "~/.emacs.d/zoom/")
;; (require 'zoom-frm)

;; (defun djcb-zoom (n)
;;   "with positive N, increase the font size, otherwise decrease it"
;;   (set-face-attribute 'default (selected-frame) :height 
;; 		      (+ (face-attribute 'default :height) 
;; 			 (* (if (> n 0) 1 -1) 10))))

;; (global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
;; (global-set-key (kbd "C-=")      '(lambda nil (interactive) (djcb-zoom 1)))
;; ;(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
;; (global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
;; ;(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))



;; ;; C-= 를 누르면 폰트를 200으로 키우고 한번 더 누르면 원래대로 돌아가도록 프로그래밍 하자.
;; ;; 프레임도 같이 늘리고 다시 줄이도록 해주면 좋을텐데...
;; (defun big-font ()
;;   "set big font"
;;   (set-face-attribute 'default (selected-frame) :height
;; 		      200))
;; (global-set-key (kbd "C-=")      '(lambda nil (interactive) (big-font)))
