;;; lotto-check.el --- Simple API for Korean Lotto 645.   -*- emacs-lisp -*-
;;; Copyright (C) 2010, 2011  Sang-gi Lee <kaisyu@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version 0.2.1
;; Author: Sang-gi Lee <kaisyu@gmail.com>

;; Requirements:
;; * Emacs 22+

;; Install:
;; * append following lines to your start-up script file (e.g. .emacs)
;;   (add-to-list 'load-path "<lotto-check.el path>")
;;   (require 'lotto-check)

;; Usage:
;; * interactive functions
;;   - M-x lotto-start                : show lotto message buffer
;;   - M-x lotto-retrieve-numbers-i   : retrieve a specific lotto info
;;   - M-x lotto-check-numbers-list-i : check lotto numbers
;;   - M-x lotto-save-db-to-file-i    : save lotto database to the local file(`lotto-database-file')
;;   - M-x lotto-load-db-to-file-i    : load lotto database from the local file(`lotto-database-file')
;; * API functions
;;   - lotto-retrieve-numbers   : retrieve a specific lotto info
;;   - lotto-check-numbers-list : check lotto numbers
;;   - lotto-save-db-to-file    : save lotto database to the local file(`lotto-database-file')
;;   - lotto-load-db-from-file  : load lotto database from the local file(`lotto-database-file')


(eval-when-compile (require 'cl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst +lotto-database-version+
  '(2 0)
  "Version of the local cache database")


(defvar *lotto-database*
  nil
  "Local cache database for Lotto info")


(defconst +lotto-message-buffer+
  "*lotto-check-messages*"
  "Message buffer for lotto-check module")


(defconst +lotto-msg-window-height+
  15
  "Height of the Lotto message window")


(defconst +http-retrieved-page-contents-buffer+
  "*http-retrieved-page-contents*"
  "Buffer for the retrieved page contents by http")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lotto data sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst +lotto-data-source-lotto-k+
  (list
   (list 'url
         (lambda (gno)
           (format "http://lotto.kaisyu.com/api?method=get&type=emacs&gno=%d"
                   (if gno gno 0))))
   (list 'retr
         (lambda (buf gno)
           (set-buffer buf)
           (goto-char (point-min))
           (re-search-forward "^(.+$")
           (read-from-whole-string (match-string 0)))))
  "Lotto Data Source: Lotto-K")


(defconst +lotto-data-source-naver+
  (list
   (list 'url
         (lambda (gno)
           (if (and gno (> gno 0))
               (format "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%d%%C8%%B8%%B7%%CE%%B6%%C7" gno)
             "http://search.naver.com/search.naver?sm=tab_hty&where=nexearch&query=%B7%CE%B6%C7")))
   (list 'retr
         (lambda (buf gno)
           (let ((obj nil)
                 (nums nil))
             (set-buffer buf)
             ;; nums + bnum
             (goto-char (point-min))
             (dotimes (i 7)
               (re-search-forward "ball\\([0-9]+\\)\\.gif")
               (push (string-to-number (match-string 1)) nums))
             (aput 'obj 'bnum (pop nums))
             (aput 'obj 'nums (reverse nums))
             ;; gno, gdate
             (goto-char (point-min))
             (re-search-forward "<em>\\([0-9]+\\)[^(]+(\\([0-9]\\{4\\}\\.[0-9]\\{2\\}\\.[0-9]\\{2\\}\\)[^)]+)")
             (aput 'obj 'gno (string-to-number (match-string 1)))
             (aput 'obj 'gdate (replace-regexp-in-string "\\." "-" (match-string 2)))
             obj))))
  "Lotto Data Source: Naver")


(defconst +lotto-data-source-daum+
  (list
   (list 'url
         (lambda (gno)
           (if (and gno (> gno 0))
               (format "http://search.daum.net/search?q=%%B7%%CE%%B6%%C7%d" gno)
             "http://search.daum.net/search?q=%B7%CE%B6%C7")))
   (list 'retr
         ;; FIXME: `date-to-time' error on running the `url-retrieve-synchronously' function
         (lambda (buf gno)
           (let ((obj nil)
                 (nums nil))
             (set-buffer buf)
             ;; nums + bnum
             (goto-char (point-min))
             (dotimes (i 7)
               (re-search-forward "ball_\\([0-9]+\\)\\.gif")
               (push (string-to-number (match-string 1)) nums))
             (aput 'obj 'bnum (pop nums))
             (aput 'obj 'nums (reverse nums))
             ;; gno, gdate
             (goto-char (point-min))
             (re-search-forward " \\([0-9]+\\)[^(]+(\\([0-9]+\\)[^0-9]+\\([0-9]+\\)[^0-9]+\\([0-9]+\\)[^)]+)</em>")
             (aput 'obj 'gno (string-to-number (match-string 1)))
             (aput 'obj 'gdate (concat (match-string 2) "-" (match-string 3) "-" (match-string 4)))
             obj))))
  "Lotto Data Source: Daum")


(defconst +lotto-data-source-nate+
  (list
   (list 'url
         (lambda (gno)
           (if (and gno (> gno 0))
               (format "http://search.nate.com/search/all.html?q=%%B7%%CE%%B6%%C7%d" gno)
             "http://search.nate.com/search/all.html?q=%B7%CE%B6%C7")))
   (list 'retr
         (lambda (buf gno)
           (let ((obj nil)
                 (nums nil)
                 (gnos nil))
             (set-buffer buf)
             ;; nums + bnum
             (goto-char (point-min))
             (dotimes (i 7)
               (re-search-forward "ball\\([0-9]+\\)\\.gif")
               (push (string-to-number (match-string 1)) nums))
             (aput 'obj 'bnum (pop nums))
             (aput 'obj 'nums (reverse nums))
             ;; gno
             (goto-char (point-min))
             (while (re-search-forward "txt_num_count\\([0-9]\\)\\.gif" nil t)
               (push (match-string 1) gnos))
             (aput 'obj 'gno (string-to-number (mapconcat 'identity (reverse gnos) "")))
             ;; gdate
             (goto-char (point-min))
             (setq gnos nil)
             (while (re-search-forward "txt_num_date\\([0-9]\\)\\.gif\\|txt_year\\.gif\\|txt_month\\.gif" nil t)
               (push (if (null (match-string 1)) "-" (match-string 1)) gnos))
             (push " GMT+9" gnos)
             (aput 'obj 'gdate
                   (format-time-string
                    "%Y-%m-%d"
                    (date-to-time (mapconcat 'identity (reverse gnos) ""))))
             obj))))
  "Lotto Data Source: Nate")


(defconst +lotto-data-source-645lotto+
  (list
   (list 'url
         (lambda (gno)
           (if (and gno (> gno 0))
               (format "http://www.645lotto.net/resultall/%d.asp" gno)
             "http://www.645lotto.net/resultall/dummy.asp")))
   (list 'retr
         (lambda (buf gno)
           (let ((obj nil)
                 (nums nil))
             (set-buffer buf)
             ;; nums + bnum
             (goto-char (point-min))
             (dotimes (i 7)
               (re-search-forward "Ball[\\\t ]:[\\\t ]\\\"\\([0-9]+\\)\\\"")
               (push (string-to-number (match-string 1)) nums))
             (aput 'obj 'bnum (pop nums))
             (aput 'obj 'nums (reverse nums))
             ;; gno
             (goto-char (point-min))
             (re-search-forward "GRWNO[\\\t ]:[\\\t ]\\\"\\([0-9]+\\)\\\"")
             (aput 'obj 'gno (string-to-number (match-string 1)))
             ;; gdate
             (goto-char (point-min))
             (re-search-forward "GRWDate[\\\t ]:[\\\t ]\\\"\\(.+\\)\\\"")
             (aput 'obj 'gdate (replace-regexp-in-string "\\." "-" (match-string 1)))
             obj))))
  "Lotto Data Source: 645Lotto")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup lotto nil
  "Simple API for Korean Lotto 645"
  :prefix "lotto-"
  :version "22.0"
  :group 'applications)


(defcustom lotto-info-data-source-custom nil
  "a custom data source to retrieve lotto info.

To enable this variable, you must set `lotto-info-data-source' to `lotto-info-data-source-custom'."
  :type '(alist :value-type function)
  :group 'lotto)


(defcustom lotto-info-data-source +lotto-data-source-lotto-k+
  "a data source to retrieve lotto info"
  :type '(alist :value-type function)
  :options (list +lotto-data-source-lotto-k+
                 +lotto-data-source-naver+
                 +lotto-data-source-daum+
                 +lotto-data-source-nate+
                 +lotto-data-source-645lotto+
                 lotto-info-data-source-custom)
  :group 'lotto)


(defcustom lotto-database-file "~/.lotto-database"
  "a file to store lotto database"
  :type 'file
  :group 'lotto)


(defcustom lotto-use-buffer-for-message t
  "use an own buffer for messages"
  :type 'boolean
  :group 'lotto)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lotto-mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst +lotto-keywords+
  '(("Game [0-9]+:" . font-lock-keyword-face)
    ("Try #[0-9]+" . font-lock-keyword-face)
    ("Rank:\\|Matched Numbers:" . font-lock-keyword-face)
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    ("OK:" . font-lock-function-name-face)
    ("Error:" . font-lock-warning-face))
  "Keywords for lotto-mode")


(define-derived-mode lotto-mode fundamental-mode
  "lotto-mode"
  "Major mode for Lotto Info"

  ;; code for syntax highlighting
  (setq font-lock-defaults '(+lotto-keywords+))

  ;; make the buffer read-only
  (setq buffer-read-only t)

  ;; key bindings
  (local-set-key (kbd "Q") 'lotto-kill-message-buffer)
  (local-set-key (kbd "q") 'lotto-hide-message-buffer)
  (local-set-key (kbd "!") 'lotto-clear-message-buffer)
  (local-set-key (kbd "g") 'lotto-retrieve-numbers-i)
  (local-set-key (kbd "r") 'lotto-retrieve-numbers-i)
  (local-set-key (kbd "c") 'lotto-check-numbers-list-i)
  (local-set-key (kbd "l") 'lotto-load-db-from-file-i)
  (local-set-key (kbd "s") 'lotto-save-db-to-file-i)
  (local-set-key (kbd "h") 'lotto-display-help-message))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lotto-get-database-version ()
  "return the version of the local cache db format"
  (when (hash-table-p *lotto-database*)
    (gethash 'version *lotto-database*)))


(defun lotto-save-db-to-file ()
  "save the contents of `*lotto-database*' to the local file(`lotto-database-file')"
  (unless (hash-table-p *lotto-database*)
    (return))
  (with-temp-buffer
    (insert (format "%s" *lotto-database*))
    (when (file-writable-p lotto-database-file)
      (write-region (point-min)
                    (point-max)
                    lotto-database-file)
      t)))


(defun lotto-initialize-db ()
  "initialize `*lotto-database*'"
  (setq *lotto-database* (make-hash-table))
  (puthash 'version +lotto-database-version+ *lotto-database*)
  nil)


(defun lotto-load-db-from-file ()
  "load the contents of `*lotto-database*' from the local file(`lotto-database-file')"
  (with-temp-buffer
    (cond ((file-readable-p lotto-database-file)
           (insert-file-contents lotto-database-file)
           (goto-char (point-min))
           (setq *lotto-database* (read-from-whole-string (buffer-string)))
           (let ((ver (lotto-get-database-version)))
             ;; check db version
             (if (or (not ver) (> (car +lotto-database-version+) (car ver)))
                 (lotto-initialize-db)
               t)))
          (t
           ;; initialize db
           (lotto-initialize-db)
           nil))))


(defun lotto-call-url (data-src gno)
  (funcall (cadr (assoc 'url data-src))
            gno))


(defun lotto-call-retr (data-src buf gno)
  (funcall (cadr (assoc 'retr data-src))
           buf gno))


(defun lotto-get-value (info key)
  (if info
      (cdr (assoc key info))
    nil))


(defun lotto-do-retrieve (data-src gno)
  "retrieve lotto info
DATA-SRC: a data source
GNO: game no.
return: object. (alist)"
  (let ((buf1 (url-retrieve-synchronously
               (lotto-call-url data-src gno)))
        (obj nil))
    (ignore-errors
      (unwind-protect
          (setq obj (lotto-call-retr data-src buf1 gno))
        (kill-buffer buf1))
      obj)))


(defun lotto-retrieve-numbers-from-local-db (gno)
  "retrieve lotto numbers from local db
GNO: game no.
return: lotto info. (alist) OR nil if the info does not exist on the local db.

ex) (lotto-retrieve-numbers-from-local-db 395)
    => ((bnum . 7)
        (gno . 395)
        (gdate . \"2010-06-26\")
        (nums 11 15 20 26 31 35))"
  (when (hash-table-p *lotto-database*)
    (gethash gno *lotto-database*)))


(defun lotto-retrieve-numbers (gno)
  "retrieve lotto numbers
GNO: game no.
return: lotto info. (alist)

ex) (lotto-retrieve-numbers 395)
    => ((bnum . 7)
        (gno . 395)
        (gdate . \"2010-06-26\")
        (nums 11 15 20 26 31 35))"
  (let ((lval (lotto-retrieve-numbers-from-local-db gno)))
    (or lval
        (progn
          (setq lval (lotto-do-retrieve lotto-info-data-source gno))
          (if lval
              (puthash (lotto-get-value lval 'gno) lval *lotto-database*))))))


(defun lotto-check-numbers (lotto-nums my-nums)
  "check the given lotto numbers
LOTTO-NUMS: lotto numbers (6 numbers and a bonus number)
MY-NUMS: my numbers to check (6 numbers)
return: result. (alist)

ex) (lotto-check-numbers '((nums 11 15 16 18 31 34) (bnum . 44)) '(11 15 20 26 31 35))
    => ((rank . 5)
        (matched . (11 15 31)))"
  (let* ((intsec (delq
                  nil
                  (mapcar
                   (lambda (x) (car (member x (lotto-get-value lotto-nums 'nums))))
                   my-nums)))
         (ilen (length intsec)))
    (list
     (cons 'rank
           (cond ((= ilen 6) 1)
                 ((= ilen 5)
                  (if (member (lotto-get-value lotto-nums 'bnum) my-nums)
                      (progn
                        (setq intsec
                              (append intsec (list (lotto-get-value lotto-nums 'bnum))))
                        2)
                    3))
                 ((> ilen 2)
                  (- 8 ilen))
                 (t 0)))
     (cons 'matched intsec))))


(defun lotto-check-numbers-list (gno my-num-list)
  "check the given list of the lotto numbers
GNO: game no.
MY-NUM-LIST: a list of numbers to check
return: result of check. ((alist) ...)

ex) (lotto-check-numbers-list 395 '((1 2 3 4 5 6) (11 15 20 28 32 36)))
    => (((rank . 0)
         (matched . nil))
        ((rank . 5)
         (matched . (11 15 20))))"
  (let ((lotto-nums (lotto-retrieve-numbers gno))
        (my-list (if (consp (car my-num-list))
                     my-num-list
                   (list my-num-list)))
        (result ()))
    (dolist (nums my-list)
      (push (lotto-check-numbers lotto-nums nums)
            result))
    (reverse result)))


(defun lotto-retrieve-numbers-formatted (gno)
  "return a formatted string for the given game no.
GNO: game no.
return: a formatted lotto info string

ex) (lotto-retrieve-numbers-formatted 430)
    => \"Game 430: 1 3 16 18 30 34 and Bonus Number is 44.\""
  (let ((info (lotto-retrieve-numbers gno)))
    (if info
        (format "Game %d: %s and Bonus Number is %d."
                (lotto-get-value info 'gno)
                (substring
                 (format "%s" (lotto-get-value info 'nums))
                 1 -1)
                (lotto-get-value info 'bnum))
      (format "Game %d: not exist yet."
              gno))))


(defun lotto-create-msg-window ()
  "create a window for the `*lotto-check-messages*' buffer"
  (let ((buf (get-buffer-create +lotto-message-buffer+))
        (win (split-window-vertically)))
    (other-window 1)
    (set-window-text-height nil +lotto-msg-window-height+)
    (switch-to-buffer buf)
    (lotto-mode)
    win))


(defun lotto-get-or-create-lotto-msg-window (&optional do-not-create)
  "find the window for the`*lotto-check-messages*' buffer
or create a window for the buffer
DO-NOT-CREATE: If its value is non NIL, this function does not create a new window.
return: the exist window or a new window (DO-NOT-CREATE is NIL)
        NIL or a new window (DO-NOT-CREATE is non NIL)"
  (let ((wlist (window-list))
        (lwin nil))
    (dolist (w wlist)
      (when (string= +lotto-message-buffer+
             (buffer-name (window-buffer w)))
        (setq lwin w)
        (return)))
    (if (and (not do-not-create) (null lwin))
        (lotto-create-msg-window)
      lwin)))


(defun lotto-message (msg &optional to-buf)
  "display the given message
MSG: a message to display
TO-BUF: whether to display the message on the `*lotto-check-messages*' buffer

* If the value of either `TO-BUF' or `lotto-use-buffer-for-message' is non NIL,
  the contents of `MSG' will be displayed on the `*lotto-check-messages*' buffer."
  (if (or to-buf lotto-use-buffer-for-message)
      (let ((win (lotto-get-or-create-lotto-msg-window)))
        (set-buffer (window-buffer win))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert msg)
          (insert "\n")
          (goto-char (point-max))))
    (message msg)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lotto-start ()
  "create or show a lotto-mode bu"
  (interactive)
  (let ((buf (get-buffer +lotto-message-buffer+))
        (win (lotto-get-or-create-lotto-msg-window)))
    (if buf
        (select-window win)
      (lotto-display-greetings))))


(defun lotto-kill-message-buffer ()
  "kill the `*lotto-check-messages*' buffer"
  (interactive)
  (let ((buf (get-buffer +lotto-message-buffer+)))
    (when buf
      (let ((win (lotto-get-or-create-lotto-msg-window t)))
        (if win
            (delete-window win)))
      (kill-buffer buf))))


(defun lotto-clear-message-buffer ()
  "clean up the `*lotto-check-messages*' buffer"
  (interactive)
  (let ((buf (get-buffer +lotto-message-buffer+)))
    (when buf
      (set-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (lotto-display-greetings)))))


(defun lotto-display-greetings ()
  "display greeting messages"
  (interactive)
  (lotto-message "Welcome to Lotto-mode!")
  (lotto-message "`h' for help messages."))


(defun lotto-display-help-message ()
  "display help messages"
  (interactive)
  (let ((msg
         '("====================================================="
           " Key Shortcuts"
           "-----------------------------------------------------"
           " h      - display this messages"
           " g or r - retrieve lotto numbers"
           " c      - check the given list of the lotto numbers"
           " l      - load lotto info from the local file"
           " s      - save lotto info to the local file"
           " !      - clean up the lotto message buffer"
           " q      - close the lotto message window"
           " Q      - kill the lotto message window & the buffer"
           "=====================================================")))
    (dolist (m msg)
      (lotto-message m))))


(defun lotto-retrieve-numbers-i (gno)
  "retrieve lotto numbers and then
show it on the `*lotto-check-messages*' buffer
        or the `*Messages*' buffer"
  (interactive "ngame no: ")
  (lotto-message (lotto-retrieve-numbers-formatted gno)))


(defun lotto-check-numbers-list-i (gno my-num-list)
  "check the given list of the lotto numbers and then
show it on the `*lotto-check-messages*' buffer
        or the `*Messages*' buffer"
  (interactive "ngame no: \nxyour numbers. ex] (1 2 3 4 5 6): ")
  (if (not (consp my-num-list))
      (progn
        (lotto-message "Error: The format of your numbers is invalid.")
        (lotto-message "       Valid examples: (1 2 3 4 5 6) or ((1 2 3 4 5 6) ...)"))
    (let ((results (lotto-check-numbers-list gno my-num-list))
          (msgs nil))
      (push "---------------------------------------------------------------------------" msgs)
      (push (lotto-retrieve-numbers-formatted gno) msgs)
      (do ((cnt 1 (1+ cnt))
           (lst results (cdr lst))
           (mylst (if (consp (car my-num-list))
                      my-num-list
                    (list my-num-list))
                  (cdr mylst)))
          ((or (null lst) (null mylst)))
        (push (format "Try #%d %-19s => Rank: %s, Matched Numbers: %s"
                      cnt
                      (car mylst)
                      (if (= (lotto-get-value (car lst) 'rank) 0)
                          "-"
                        (int-to-string (lotto-get-value (car lst) 'rank)))
                      (if (null (lotto-get-value (car lst) 'matched))
                          "None"
                        (substring
                         (format "%s" (lotto-get-value (car lst) 'matched))
                         1 -1)))
              msgs))
      (push "---------------------------------------------------------------------------" msgs)
      (lotto-message (mapconcat 'identity (reverse msgs) "\n")))))


(defun lotto-save-db-to-file-i ()
  "save the contents of `*lotto-database*' to the local file(`lotto-database-file')"
  (interactive)
  (if (lotto-save-db-to-file)
      (lotto-message (format "OK: Lotto DB has been successfully saved. (%s)" lotto-database-file))
    (lotto-message "ERROR: Saving Lotto DB failed!!")))


(defun lotto-load-db-from-file-i ()
  "load the contents of `*lotto-database*' from the local file(`lotto-database-file')"
  (interactive)
  (if (lotto-load-db-from-file)
      (lotto-message (format "OK: Lotto DB has been successfully loaded. (%s)" lotto-database-file))
    (lotto-message "ERROR: Loading Lotto DB failed!!")))


(defun lotto-hide-message-buffer ()
  "hide the window for the `*lotto-check-messages*' buffer"
  (interactive)
  (let ((win (lotto-get-or-create-lotto-msg-window t)))
    (if win
        (delete-window win))))


(defun http-retrieve-page-contents (url)
  "retrieve data from the given URL"
  (interactive "surl: ")
  (save-excursion
    (let ((buf1 (url-retrieve-synchronously url)))
      (set-buffer (get-buffer-create +http-retrieved-page-contents-buffer+))
      (erase-buffer)
      (insert-buffer-substring buf1)
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (ignore-errors (decode-coding-region (point-min) (point-max) (intern (downcase (match-string 1)))))
      (kill-buffer buf1)
      (switch-to-buffer +http-retrieved-page-contents-buffer+))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (eval load)
  (unless *lotto-database*
    (lotto-load-db-from-file)))


(add-hook 'kill-emacs-hook 'lotto-save-db-to-file)


(provide 'lotto-check)
