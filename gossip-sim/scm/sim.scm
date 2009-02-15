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

(read-enable 'positions)
(debug-enable 'backtrace)
(debug-enable 'debug)

(define-module (gossip sim)
  :use-module (gossip sim-config)
  :use-module (gossip sim-options)
  :use-module (gossip sim-net)
  :use-module (gossip sim-library)
  :use-module (gossip sim-schedule)
  :use-module (gossip sim-run)
  :use-module (ice-9 format)
  :use-module (ice-9 optargs))

(if sim-guile-readline-works
    (use-modules (ice-9 readline)))

(re-export sim-library-path
	   use-library
	   process-use-library
	   connect-signals
	   set-verbose
	   make-signal)

(export signals
	let-signals
	sig
	sim-run
	args
	arg
	set-exit-predicate
	exit-expression
	finished?
	find-comp
	make-block
	define-block
	block-lambda
	result
	set-epilog
	epilog
	results
	run
	sim-repl
	command-line-run)

;; Convenience macro to define many signals in one go.
;;
;;    (signals (NAME ARGS ...) ...)
;;
;; is equivalent to
;;
;;    (begin
;;      (define s1 (make-signal 'NAME ARGS ...))
;;      ...)
;;
;; The construct (NAME) can be abbreviated as just NAME.
;;
(define-macro (signals . sigs)
  `(begin ,@(map (lambda (s)
		   (if (list? s)
		       `(define ,(car s) (make-signal ',(car s) ,@(cdr s)))
		       `(define ,s (make-signal ',s))))
		 sigs)))

(define-macro (let-signals sigs . body)
  `(let () (signals ,@sigs) ,@body))

(define (sig . args)
  (if (null? args)
      (make-signal #f)
      (apply make-signal-head args)))

(define (find-comp-1 name comps)
  (let loop ((comps comps))
    (cond
     ((null? comps)
      #f)
     ((and (string? (comp-name (car comps)))
	   (string=? name (comp-name (car comps))))
      (car comps))
     (else
      (loop (cdr comps))))))

(define (find-comp name)
  (or (find-comp-1 name (comp-children (current-comp)))
      (error "component not found" name)))

(define (finished? c)
  (if (string? c)
      (comp-finished? (find-comp c))
      (comp-finished? c)))

(define (set-exit-predicate f)
  (set! (comp-finished-predicate (current-comp)) f))

(define-macro (exit-expression exp)
  (define (translate exp)
    (cond
     ((list? exp)
      (case (car exp)
	((and or not)
	 (cons (car exp) (map translate (cdr exp))))
	(else
	 (error "unrecognized exit expression operator" (car exp)))))
     ((or (symbol? exp) (string? exp))
      `(finished? ,exp))
     (else
      (error "unrecognized exit expression syntax" exp))))
  `(set-exit-predicate (lambda () ,(translate exp))))

(define (result c res)
  (if (string? c)
      (result (find-comp c) res)
      (cond ((keyword? res)
	     (comp-result c (keyword->symbol res)))
	    ((string? res)
	     (comp-result c (string->symbol res)))
	    (else
	     (comp-result c res)))))

(define (set-epilog proc)
  (set! (comp-epilog (current-comp)) proc))

(define-macro (epilog . body)
  `(set-epilog (lambda () ,@body)))

(define (results . res-val-pairs)
  (let loop ((rv res-val-pairs))
    (cond
     ((not (null? rv))
      (cond
       ((and (keyword? (car rv)) (not (null? (cdr rv))))
	(comp-set-result (current-comp) (keyword->symbol (car rv)) (cadr rv))
	(loop (cddr rv)))
       (else
	(error "improper keyword/value list" rv)))))))

(define (make-encapsulation-module)
  (let ((m (make-module 31)))
    (beautify-user-module! m)
    m))

(define (encapsulated-load file)
  (save-module-excursion
   (lambda ()
     (let ((m (make-encapsulation-module)))
       (set-current-module m)
       (module-use! m (resolve-module '(gossip sim)))
       (basic-load file)))))

(define (set-inspect-on-error flag)
  (set! inspect-on-error flag))

(define (sim-run-with-prefix prefix setup . args)
  (let ((net (make-net (lambda () (apply setup args)) prefix)))
    (cond ((and net (not (null? (net-comps net))))
	   (let ((sched (schedule net)))
	     (run-simulation net sched))))))

(define (sim-run setup . args)
  (apply sim-run-with-prefix #f setup args))

;;; High-level definition of new blocks

;; A `shortformal' is used for convenience with hierarchical blocks.
;; It has no type, and multiports are designated by << and >>,
;; respectively.  Generics and ports get a type of `#t' which means
;; `any type'.

(define (shortformal->preformal f)
  (case (car f)
    ((=)
     `(= ,(cadr f) #t ,(not (null? (cddr f))) ,@(cddr f)))
    ((< >)
     `(,(car f) ,(cadr f) #t))
    ((<<)
     `(< ,(cadr f) (multi #t)))
    ((>>)
     `(> ,(cadr f) (multi #t)))))

(define (make-block . args)
  (let-keywords args #f (name
			 (primitive #f)
			 (interface #f)
			 (instfunc #f))
    (cond
     (primitive
      (if interface
	  (error "must provide either :interface or :primitive"))
      (make-primitive-block name primitive))
     (interface
      (make-hierarchical-block name (map shortformal->preformal interface)
			       instfunc))
     (else
      (error "must provide either :interface or :primitive")))))

(define-macro (define-block name . rest)
  (if (list? name)
      `(define ,(car name) (block-lambda ,name ,@rest))
      `(define ,name (block-lambda ,@rest))))

(define-macro (block-lambda . args)
  (if (list? (car args))
      `(block-lambda :name ,(caar args) :interface ,(cdar args) ,@(cdr args))
      (let loop ((keys '())
		 (instargs '())
		 (body args))
	(cond
	 ((null? body)
	  `(make-block ,@keys))
	 ((not (keyword? (car body)))
	  `(make-block ,@keys :instfunc (lambda ,instargs ,@body)))
	 ((not (null? (cdr body)))
	  (let* ((key (car body))
		 (val (case key
			((:interface)
			 `',(cadr body))
			((:name)
			 (symbol->string (cadr body)))
			(else
			 (cadr body))))
		 (instargs (if (eq? key :interface)
			       (map cadr (cadr val))
			       instargs)))
	    (loop (cons* key val keys)
		  instargs
		  (cddr body))))
	 (else
	  (error "missing keyword value"))))))

(define (load-user-init file)
  (define (existing-file dir)
    (let ((path (in-vicinity dir file)))
      (if (and (file-exists? path)
	       (not (file-is-directory? path)))
	  path
	  #f)))
  (let ((path (or (existing-file (or (getenv "HOME") "/"))
                  (and (provided? 'posix) 
		       (existing-file (passwd:dir (getpw (getuid))))))))
    (if path (primitive-load path))))

(define (sim-repl)
  (let ((guile-user (resolve-module '(guile-user))))
    (module-use! guile-user (resolve-interface '(gossip sim)))
    (load-user-init ".gossip/simrc")
    (if sim-guile-readline-works
	(activate-readline))
    (set-repl-prompt! "gossip> ")
    (top-repl)))

(define args-fluid (make-fluid))

(define (call-with-args args thunk)
  (with-fluids ((args-fluid args))
    (thunk)))

(define-macro (with-args args . body)
  `(call-with-args ,args (lambda () ,@body)))

(define (args) 
  (fluid-ref args-fluid))

(define (arg n . def)
  (let ((a (args)))
    (if (< n (length a))
	(list-ref a n)
	(if (null? def)
	    (error "too few arguments for script")
	    (car def)))))

(define (run file . args)
  (with-args args
    (sim-run-with-prefix file encapsulated-load file)))

(define (read-from-string str)
  (call-with-input-string str read))
		        
(define (command-line-run argv)
  (if (< (length argv) 2)
      (sim-repl)
      (apply run (cadr argv) (map read-from-string (cddr argv)))))
