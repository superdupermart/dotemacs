;;; lotto-gen.el --- Simple API for Korean Lotto 645.   -*- emacs-lisp -*-
;;; Copyright (C) 2010  Sang-gi Lee <kaisyu@gmail.com>
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

;; Version 0.1
;; Author: Sang-gi Lee <kaisyu@gmail.com>

;; Requirements:
;; * Emacs 22+

;; Install:
;; * append following lines to your start-up script file (e.g. .emacs)
;;   (add-to-list 'load-path "<lotto-gen.el path>")
;;   (require 'lotto-gen)



;;------------------------------------------------
;; method 1: simple random numbers
;;------------------------------------------------


(defun lotto-gen-random ()
  ;(random t)
  (let ((lst ())
        (i 0)
        (val 0))
    (while (< i 6)
      (setq val (1+ (random 45)))
      (unless (member val lst)
        (setq lst (cons val lst))
        (setq i (1+ i))))
    lst))


(defun lotto-gen-1 ()
  (let ((lst (lotto-gen-random)))
    (list (sort (lotto-gen-random) #'<)
          (apply #'+ lst))))


(defun lotto-gen-1-i ()
  (interactive)
  (message (lotto-gen-1)))



;;------------------------------------------------
;; method 2: mix numbers by random sequence
;;------------------------------------------------


(defun lotto-gen-random-comp (a b)
  (if (= (random 2) 1) t nil))


(defun lotto-gen-take-random (lst n mixcnt)
  (cond ((null lst) ())
        ((<= n 0) ())
        (t
         (let ((nlst 
                (dotimes (cnt mixcnt lst)
                  (setq lst (sort lst #'lotto-gen-random-comp)))))
           (cons (car nlst) (lotto-gen-take-random (cdr nlst) (1- n) mixcnt))))))


(defun lotto-gen-seq (mixcnt)
  (let ((lst (loop for i from 1 to 45 collect i)))
    (lotto-gen-take-random lst 6 mixcnt)))


(defun lotto-gen-2 (try &optional mixcnt)
  (or mixcnt (setq mixcnt 1000))
  (if (<= try 0)
      nil
      (cons
       (let ((lst (lotto-gen-seq mixcnt)))
         (list (sort lst #'<) (apply #'+ lst)))
       (lotto-gen-2 (1- try) mixcnt))))


(defun lotto-gen-2-i (try &optional mixcnt)
  (interactive "nhow many games? \nnmixing count: ")
  ;(setq *random-state* (make-random-state t))
  (message (lotto-gen-2 try mixcnt)))



;;------------------------------------------------


(provide 'lotto-gen)
