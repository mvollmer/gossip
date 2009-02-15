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

(define-module (gossip sim-library)
  :use-module (gossip sim-util)
  :use-module (gossip sim-net)
  :use-module (gossip sim-options))

;;; Implementation of Gossip libraries.

;; This is heavily dependent on the current module system, but so
;; what.

;; A library is a special kind of module.  It can not be accessed with
;; the regular module machnisms like `use-modules' because it handles
;; its interfaces differently, but other than that, it is an ordinary
;; module.  You need to use `use-library', etc, to get at library
;; modules.
;;
;; The special thing about libraries is that they are composed of
;; separate files that are loaded on demand (and are supposed to
;; contain block definitions, of course (but can contain anything
;; actually)).  In addition to that, they can automatically load
;; shared objects and make primitive blocks out of them.
;;
;; The public interface of libraries is done differently from normal
;; modules just because I think they could use a different approach.
;;
;; A library consists of one `library module' and several subordinate
;; `block modules'.  Whenever a Scheme file is loaded because a
;; specific block is need, a new block modules is constructed and the
;; Scheme code is evaluated inside this new module.  The expected
;; binding for the block is then copied into the library module.  You
;; can not export bindings from block modules.  A block module uses
;; the module (gossip sim) so that the usual facilities are present
;; that can also be used in simulation files.  It also uses the
;; library module itself so that a block module can use all blocks in
;; its library without explicitely importing them.
;;
;; Additionally, all files named "common.scm" that are found in the
;; library directories (see below) are loaded into the library module.
;; They are supposed to contain common utility definitions.  The
;; filenames "library.*" are reserved for future use (they are
;; intended for librtary meta-information, such as a map from
;; identifiers to files).
;;
;; When you import bindings from a library (using `use-library'), you
;; specify which bindings to import by giving a number of
;; `import-set's.  An import-set can be:
;;
;;   *                   - all bindings without renaming
;;   sym                 - only the stated symbol, without renaming
;;   (locname sym)       - only "sym" but renamed as "locname"
;;   (prefix sym sym...) - all stated syms, prefixed
;;   (prefix *)          - all bindings, prefixed
;;
;; [ "sym" is any symbol other than "*", so there is no way to import
;;   a binding named "*".  But you shouldn't do that anyway. ]
;;
;; A `use-library' clause constructs a new `interface' module and
;; copies the requested bindings into it.  Whenever a interface module
;; is constructed, the underlying library is first `refreshed'.  This
;; means that it wil check for newly available blocks and newer
;; versions of existing blocks, and make them available.  It will also
;; load "common.scm" again when it has changed.  The bindings in an
;; existing interface module will never change, even when the library
;; is refreshed in the mean time.  You have to re-import the library
;; if you really want to pick up the changes.  The block modules that
;; are contained in the library will see the change immediatly,
;; however.  A library starts out empty and only the first
;; `refreshing' will gather the blocks that are contained in it.
;;
;; The refreshing is done by looking into a list of directories for
;; files with extensions of ".block" or ".prim".  When a ".block" file
;; is found, it is loaded as Scheme code.  When a ".prim" file is
;; found, an corresponding primitive block is constructed by hand and
;; added to the library.
;;
;; The list of directories is found by looking into the directories on
;; the `library path' for directories that match the name of the
;; library.
;;
;; Block modules are only constructed on demand, when the first
;; binding is requested for them.
;;
;; The `refreshing' is meant to support interactive usage of
;; gossip-sim.  The goal is to start it only once and then use it as a
;; shell.  This means that we need to pick up changes to blocks as
;; they appear on disk.  Some trickery is necessary to make this work
;; with shared objects because you can only have one shared object of
;; any one name linked at a time.  The trickery is done in
;; "dynblock.cc".

(export sim-library-path push!
	use-library process-use-library
	find-library library-exports
	find-library-file
	load-block)

(define sim-library-path
  (let ((libpath (default-library-path)))
    (make-procedure-with-setter
     (lambda () libpath)
     (lambda (v) (set! libpath v)))))

(define-macro (push! obj loc)
  `(set! ,loc (cons ,obj ,loc)))

(define (find-library-dirs name)
  (if (string=? name "work")
      (list (getcwd)) ; XXX - can only load from absolute filenames yet
      (let loop ((dirs '())
		 (path (sim-library-path)))
	(if (null? path)
	    (reverse! dirs)
	    (let ((full-name (string-append (car path) "/" name)))
	      (if (file-exists? full-name)
		  (loop (cons full-name dirs) (cdr path))
		  (loop dirs (cdr path))))))))

(define (file-has-suffix f . sfxs)
  (or-map (lambda (s)
	    (let ((flen (string-length f))
		  (slen (string-length s)))
	      (and (> flen slen)
		   (string=? (substring f (- flen slen)) s))))
	  sfxs))

(define (file-sans-suffix f)
  (let ((pos (string-rindex f #\.)))
    (if pos
	(substring f 0 pos)
	f)))

(define (find-library-file name file)
  (or-map (lambda (d)
	    (let ((f (string-append d "/" file)))
	      (if (file-exists? f)
		  f
		  #f)))
	  (find-library-dirs name)))

(define (make-library-module name)
  (let* ((blocks #f))

    (define (binder mod sym define?)

      (define (refresh-dirs dirs)

	(define (refresh-block sym file)
	  (cond
	   ((hashq-ref blocks sym)
	    ;; Already in hash table -> already shadowed
	    => (lambda (shadowing-file)
		 (pk file 'is-shadowed-by shadowing-file)))
	   (else
	    (hashq-set! blocks sym file)
	    (cond
	     ((module-obarray-ref (module-obarray mod) sym)
	      ;; Already referenced -> reload definition right now,
	      ;; but not when it is already being loaded.
	      (if (not (autoload-in-progress? file))
		  (autoload-block mod sym file)))))))

	(define (refresh-file file)
	  (if (and (file-exists? file)
		   (not (autoload-in-progress? file)))
	      (autoload-file mod file)))

	(set! blocks (make-hash-table 31))
	(for-each 
	 (lambda (d)
	   (refresh-file (string-append d "/common.scm"))
	   (let ((ds (opendir d)))
	     (let loop ((f (readdir ds)))
	       (cond 
		((not (eof-object? f))
		 (if (file-has-suffix f ".block" ".prim")
		     (refresh-block (string->symbol (file-sans-suffix f))
				    (string-append d "/" f)))
		 (loop (readdir ds)))))))
	 dirs))
  
      (cond
       ((eq? define? 'exports)
	(let ((dirs (find-library-dirs (symbol->string name))))
	  (if (null? dirs)
	      (error "no such library" name 'in (sim-library-path)))
	  (refresh-dirs dirs))
	(append! (delq! '%module-public-interface
			(module-map (lambda (sym var) sym) mod))
		 (hash-fold (lambda (k v r) (cons k r)) '() blocks)))
       (define?
	 #f)
       ((and blocks (hashq-ref blocks sym))
	=> (lambda (file)
	     (let ((var (module-make-local-var! mod sym)))
	       (autoload-block mod sym file)
	       var)))
       (else
	#f)))

    (let ((l (make-module 67 '() binder)))
      (set-module-kind! l 'gossip-library)
      (set-module-name! l name)
      (set-module-public-interface! l l)
      (module-use! l the-scm-module)
      (module-use! l (resolve-interface '(gossip sim)))
      l)))

(define (library-exports lib)
  ((module-binder lib) lib #f 'exports))

(define (make-library-block-module l name)
  (let ((bm (make-module 7)))
    (set-module-kind! bm 'gossip-block-module)
    (set-module-name! bm name)
    (set-module-public-interface! bm bm)
    (module-use! bm the-scm-module)
    (module-use! bm (resolve-interface '(gossip sim)))
    (if l (module-use! bm l))
    bm))

(define autoloads-in-progress '())

(define (autoload-in-progress? file)
  (member file autoloads-in-progress))

(define (autoload-in-progress! file)
  (push! file autoloads-in-progress))

(define (autoload-done! file)
  (set! autoloads-in-progress (delete! file autoloads-in-progress)))

(define (call-with-autoloading file thunk)
  (dynamic-wind
      (lambda () (autoload-in-progress! file))
      thunk
      (lambda () (autoload-done! file))))

(define-macro (with-autoloading file . body)
  `(call-with-autoloading ,file (lambda () ,@body)))

(define file-dates (make-hash-table 31))

(define (file-date file)
  (stat:mtime (stat file)))

(define (newer? date1 date2)
  (> date1 date2))

(define (autoload file loader)
  (if (autoload-in-progress? file)
      (error "already loading" file))
  (let ((date-now (file-date file))
	(date-then (hash-ref file-dates file)))
    (cond
     ((or (not date-then) (newer? date-now date-then))
      (pk 'loading file)
      (hash-set! file-dates file date-now)
      (call-with-autoloading file loader)))))

(define (autoload-block libmod block file)
  (autoload file 
	    (lambda ()
	      (module-set! libmod block (load-block file block libmod)))))

(define (load-block file block libmod)
  (if (file-has-suffix file ".block")
      (let ((blockmod (make-library-block-module libmod block)))
	(save-module-excursion
	 (lambda ()
	   (set-current-module blockmod)
	   (primitive-load file)))
	(module-ref blockmod block))
      (make-primitive-block (symbol->string block) file)))

(define (autoload-file libmod file)
  (autoload file
	    (lambda ()
	      (save-module-excursion
	       (lambda ()
		 (set-current-module libmod)
		 (primitive-load file))))))

(define the-libraries (make-hash-table 13))

(define (find-library name)
  (let ((l (hashq-ref the-libraries name)))
    (or l
	(let ((l (make-library-module name)))
	  (hashq-set! the-libraries name l)
	  l))))

(define (make-library-interface-module lib import-sets)
  (let ((imports (make-hash-table 31)))

    (define (filter-with-set set sym)
      (cond
       ((eq? set '*)
	sym)
       ((symbol? set)
	(if (eq? set sym) sym #f))
       ((and (list? set) (= (length set) 2)
	     (not (eq? (cadr set) '*))
	     (symbol? (car set)))
	(if (eq? (cadr set) sym) (car set) #f))
       ((and (list? set) (= (length set) 2)
	     (eq? (cadr set) '*)
	     (symbol? (car set)))
	(symbol-append (car set) sym))
       ((and (list? set) (> (length set) 2)
	     (symbol? (car set)))
	(if (memq sym (cdr set)) (symbol-append (car set) sym) #f))
       (else
	(error "unsupported import syntax" set))))

    (define (filter-to-imports sym)
      (let loop ((sets import-sets))
	(cond
	 ((null? sets)
	  #f)
	 ((filter-with-set (car sets) sym)
	  => (lambda (extsym) (hashq-set! imports extsym sym)))
	 (else
	  (loop (cdr sets))))))

    (define (binder mod sym define?)
      (cond
       (define?
	 #f)
       ((hashq-ref imports sym)
	=> (lambda (libsym)
	     (let ((var (module-make-local-var! mod sym)))
	       (variable-set! var (module-ref lib libsym))
	       var)))
	     ;; (module-local-variable lib libsym)))
       (else
	#f)))

    (for-each filter-to-imports (library-exports lib))

    (let ((m (make-module 1 '() binder)))
      (set-module-kind! m 'gossip-interface)
      m)))

(define (process-use-library name import-sets)
  (module-use! (current-module) 
	       (make-library-interface-module (find-library name)
					      import-sets)))

(define-macro (use-library name . import-sets)
  `(process-use-library ',name ',import-sets))
