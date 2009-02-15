;; gossip-sim - synchronous data flow simulations
;;  
;; Copyright (C) 2000, 2001, 2002  Marius Vollmer
;;  
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;  
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(read-set! keywords 'prefix)

(define-module (gossip sim-util)
  :use-module (oop goops)
  :use-module (ice-9 debug)
  :use-module (ice-9 format))

(export do-list
	define-struct copy-struct
	call-with-error-catching with-error-catching)

(define-macro (do-list v l . body)
  `(for-each (lambda (,v) ,@body) ,l))

(define (peek . stuff)
  (display ";;; " (current-error-port))
  (write stuff (current-error-port))
  (newline (current-error-port))
  (car (last-pair stuff)))

(set! pk peek)

(defmacro define-struct (name base-and-options . fields)
  (define (field-name f)
    (if (list? f) (car f) f))
  (define field-defaulted? list?)
  (define field-default cadr)
  (define (accessors)
    (map (lambda (f)
	   (symbol-append name '- (field-name f)))
	 fields))
  (define (field->slot f)
    (let ((fn (field-name f)))
      ;; XXX - keep synchronized with copy-struct below
      `(,fn :accessor ,(symbol-append name '- fn)
	    :init-keyword ,(symbol->keyword fn)
	    ,@(if (field-defaulted? f) 
		  `(:init-value ,(field-default f)) 
		  '()))))
  (let loop ((base '())
	     (bo base-and-options))
    (if (or (null? bo) (keyword? (car bo)))
	`(begin
	   (define-class ,name ,(reverse! base) 
	     ,@(map field->slot fields)
	     ,@bo)
	   (define (,(symbol-append name '?) x) (is-a? x ,name))
	   (export ,name ,@(accessors) ,(symbol-append name '?)))
	(loop (cons (car bo) base) (cdr bo)))))

(define (copy-struct str)
  (let* ((slots (class-slots (class-of str)))
	 (init-args (let loop ((res '())
			       (slots slots))
		      (if (null? slots)
			  res
			  (let ((s (car slots)))
			    ;; XXX - hard coded slot definition layout
			    (loop (list* (list-ref s 4)
					 ((list-ref s 2) str)
					 res)
				  (cdr slots)))))))
    (apply make (class-of str) init-args)))

(define (call-with-error-catching thunk)
  (catch #t
	 (lambda ()
	   (lazy-catch #t
		       (lambda ()
			 (start-stack #t (thunk)))
		       (lambda args
			 (save-stack 1)
			 (apply throw args))))
	 (lambda key-and-args
	   (if (> (length key-and-args) 4)
	       (catch #t
		      (lambda ()
			(apply handle-system-error key-and-args))
		      (lambda (key . args)
			(display key)
			(display ": ")
			(write args)
			(newline)))
	       (apply throw key-and-args)))))

(defmacro with-error-catching body
  `(call-with-error-catching (lambda () ,@body)))

(define (display-sim-error fno msg-args)
  (let* ((stack (make-stack #t (+ 2 fno) 0)))
    ;(display-backtrace stack (current-error-port))
    (display-error stack (current-error-port) #f
		   (apply format #f msg-args) '() '())))

(define (sim-error surface? msg-args)
  (let* ((s (make-stack #t))
	 (len (stack-length s)))
    (let loop ((i 0))
      (cond
       ((= i len)
	(pk 'bad)
	(display-sim-error (1- len) msg-args))
       (else
	(let ((p (frame-procedure (stack-ref s i))))
	  (if (and p (surface? p))
	      (display-sim-error i msg-args)
	      (loop (1+ i)))))))))

(export sim-error)
