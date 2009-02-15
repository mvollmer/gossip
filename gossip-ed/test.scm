(use-modules (gossip canvas)
	     (gtk gtk)
	     (gtk gdk))

(let* ((win (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new #f 0))
       (zoom-a (gtk-adjustment-new 1 0.2 50.1 0.05 0.05 0.1))
       (zoom-s (gtk-hscale-new zoom-a))
       (scrolled-window (gtk-scrolled-window-new))
       (canvas (gossip-canvas-new))
       (root (gossip-canvas-root canvas))
       (popup #f))

  (define normal-fonts #("lucidasans-8"
			 "lucidasans-10"
			 "lucidasans-12"
			 "lucidasans-14"
			 "lucidasans-18"
			 "lucidasans-24"))

  (define bold-fonts #("lucidasans-bold-8"
		       "lucidasans-bold-10"
		       "lucidasans-bold-12"
		       "lucidasans-bold-14"
		       "lucidasans-bold-18"
		       "lucidasans-bold-24"))

  (define unzoomed 2)
  (define scale 1)
  (define min-expansion 0.0)

  (define (report-item-bounds item)
    (pk 'bounds
	(gossip-canvas-item-x1 item)
	(gossip-canvas-item-y1 item)
	(gossip-canvas-item-x2 item)
	(gossip-canvas-item-y2 item)))

  (define (make-label-box l x y w h)
    (let* ((g-i (gossip-canvas-item-new root 'GossipCanvasGroup
					#()))
	   (p-i (gossip-canvas-item-new g-i 'GossipCanvasPolygon
					(vector x y
						x (+ y h)
						(- x 10) (+ y h 20)
						(+ x 20) (+ y h)
						(+ x w) (+ y h)
						(+ x w) y)
					'outline_color "black"
					'width_pixels 0))
	   (l-i (gossip-canvas-item-new g-i 'GossipCanvasText
					(vector (+ x (/ w 2)) (+ y (/ h 2)))
					'text l
					`fill_color "black")))
      (gossip-canvas-text-set-zoom-fonts l-i bold-fonts
					 unzoomed scale min-expansion)
      g-i))

  (define (item-mover item)
    (lambda (dx dy)
      (gossip-canvas-item-move item dx dy)))

  (define (point-mover item point)
    (lambda (dx dy)
      (gossip-canvas-line-move-point item point dx dy)))

  (define (grid-mover grid mover)
    (lambda (dx dy)
      (let ((gdx (- dx (remainder dx grid)))
	    (gdy (- dy (remainder dy grid))))
	(if (not (and (zero? gdx) (zero? gdy)))
	    (mover gdx gdy)))))

  (define (togrid x g) (* g (round (/ x g)))) 

  (define (make-item-draggable item grid move enter leave)
    (let* ((last-x #f)
	   (last-y #f)
	   (dragging #f))
      (define (handler ev)
	(case (gdk-event-type ev)
	  ((enter-notify)
	   (if (and enter (not dragging)) (enter))
	   #t)
	  ((leave-notify)
	   (if (and leave (not dragging)) (leave))
	   #t)
	  ((button-press)
	   (case (gdk-event-button ev)
	     ((1)
	      (set! last-x (togrid (gdk-event-x ev) grid))
	      (set! last-y (togrid (gdk-event-y ev) grid))
	      (set! dragging #t)
	      #t)
	     (else
	      #f)))
	  ((button-release)
	   (case (gdk-event-button ev)
	     ((1)
	      (set! dragging #f)
	      #t)
	     (else
	      #f)))
	  ((motion-notify)
	   (if dragging
	       (let* ((x (togrid (gdk-event-x ev) grid))
		      (y (togrid (gdk-event-y ev) grid))
		      (dx (- x last-x))
		      (dy (- y last-y)))
		 (if (not (and (zero? dx) (zero? dy)))
		     (begin
		       (move dx dy)
		       (set! last-x x)
		       (set! last-y y)))
		 #t)
	       #f))
	  (else
	   #f)))
      (gtk-signal-connect item "event" handler)))

  (define (make-drag-line root . coords)
    (let* ((group (gossip-canvas-item-new root 'GossipCanvasGroup
					  #()))
	   (line (gossip-canvas-item-new group 'GossipCanvasLine
					 coords
					 'fill_color "black"))
	   (n-points (/ (length coords) 2)))
      (do ((i 0 (1+ i))
	   (c coords (cddr c)))
	  ((= i n-points))
	(let* ((x (car c))
	       (y (cadr c))
	       (handle (gossip-canvas-item-new group 'GossipCanvasRect
					       (vector (- x 3) (- y 3)
						       (+ x 3) (+ y 3))
					       'fill_color "red3")))
	  (make-item-draggable handle 5
			       (lambda (dx dy)
				 (gossip-canvas-item-move handle dx dy)
				 (gossip-canvas-line-move-point line i dx dy))
			       (lambda ()
				 (gossip-canvas-item-set handle #f 'fill_color "yellow"))
			       (lambda ()
				 (gossip-canvas-item-set handle #f 'fill_color "red3")))))
      group))

  (define (make-rubber-box root button notify)
    (let ((start-x #f) (start-y #f)
	  (end-x #f) (end-y #f)
	  (box #f))
      (define (box-coords)
	(vector (min start-x end-x) (min start-y end-y)
		(max start-x end-x) (max start-y end-y)))
      (define (handler ev)
	(case (gdk-event-type ev)
	  ((button-press)
	   (cond 
	    ((= (gdk-event-button ev) button)
	     (set! start-x (gdk-event-x ev))
	     (set! start-y (gdk-event-y ev))
	     (set! end-x start-x)
	     (set! end-y start-y)
	     (set! box (gossip-canvas-item-new root 'GossipCanvasRect
					       (box-coords)
					       'outline_color "yellow"))
	     #t)
	    (else
	     #f)))
	  ((button-release)
	   (cond
	     ((= (gdk-event-button ev) button)
	      (gtk-object-destroy box)
	      (set! box #f)
	      (notify start-x start-y end-x end-y)
	      #t)
	     (else
	      #f)))
	  ((motion-notify)
	   (cond (box
		  (set! end-x (gdk-event-x ev))
		  (set! end-y (gdk-event-y ev))
		  (gossip-canvas-item-set box
					  (box-coords))
		  #t)
		 (else
		  #f)))
	  (else
	   #f)))
      (gtk-signal-connect root "event" handler)))

  (define (scroll-to x y)
    (let ((zoom (gtk-adjustment-value zoom-a)))
      (gtk-adjustment-set-value
       (gtk-scrolled-window-get-hadjustment scrolled-window)
       (* zoom (+ x 1000)))
      (gtk-adjustment-set-value
       (gtk-scrolled-window-get-vadjustment scrolled-window)
       (* zoom (+ y 1000)))))

  (define (set-zoom z)
    (if (> z 0)
	(gtk-adjustment-set-value zoom-a z)))

  (define (inc-zoom dz)
    (set-zoom (* (gtk-adjustment-value zoom-a) dz)))

  (define (zoom-in)
    (inc-zoom 2))

  (define (zoom-out)
    (inc-zoom 0.5))

  (define (zoom-full)
    (set-zoom 0.2))

  (gtk-scale-set-digits zoom-s 3)
  (gtk-container-add win vbox)
  (gtk-box-pack-start vbox scrolled-window)
  (gtk-container-add scrolled-window canvas)
  (gtk-box-pack-start vbox zoom-s #f)
  (gtk-widget-set-usize canvas 300 300)
  (gtk-widget-show-all win)

  (gossip-canvas-set-scroll-region canvas -1000 -1000 1001 1001)
  (gossip-canvas-item-new root 'GossipCanvasRect
			  #(-1000 -1000 1000 1000)
			  'outline_color "white")

  (let ((r (gossip-canvas-item-new root 'GossipCanvasRect 
				   #(50 50 150 150)
				   'outline_color "black"
				   'fill_color "red"
				   'width_units 0)))
    (make-item-draggable r 1 (item-mover r) #f #f))

  (let ((i (gossip-canvas-item-new root 'GossipCanvasEllipse 
				   #(100 100 200 200)
				   'outline_color "black"
				   'fill_color "green"
				   'width_units 0)))
    (make-item-draggable i 10 (item-mover i) #f #f))

  (let ((i (make-label-box "Hello, World!" 150 150 100 30)))
    (make-item-draggable i 10 (item-mover i) #f #f))

  (make-drag-line root 0 0 20 20 40 20 60 80 120 90)

  (let ((i (gossip-canvas-item-new root 'GossipCanvasGrid #(10 10)
				   'color "black")))
    (gossip-canvas-item-lower-to-bottom i))

  (gossip-canvas-item-new root 'GossipCanvasText
			  #(50 -30)
			  'text "Use all three mouse buttons!"
			  'font "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
			  'fill_color "black")

  ;; This depends on the fact that the grid covers everything and thus
  ;; the root is also enlarged to cover everything.  Events work only
  ;; from within leaf items.
  (make-rubber-box root 2 
		   (lambda (x1 y1 x2 y2)
		     (set-zoom (min (/ (gossip-canvas-get-width canvas)
				       (abs (- x1 x2)))
				    (/ (gossip-canvas-get-height canvas)
				       (abs (- y1 y2)))))
		     (scroll-to (min x1 x2) (min y1 y2))))

  (gtk-signal-connect root "event"
		      (lambda (ev)
			(cond 
			 ((and (eq? (gdk-event-type ev) 'button-press)
			       (= (gdk-event-button ev) 3))
			  (gtk-menu-popup popup #f #f 3 0)))))

  (let ((menu (gtk-menu-new))
	(items `(("Zoom In" ,zoom-in)
		 ("Zoom Out" ,zoom-out)
		 ("Zoom Full" ,zoom-full))))
    (for-each (lambda (l)
		(let ((i (gtk-menu-item-new-with-label (car l))))
		  (gtk-menu-append menu i)
		  (gtk-signal-connect i "activate" (cadr l))))
	      items)
    (gtk-widget-show-all menu)
    (set! popup menu))

  (gtk-signal-connect zoom-a "value-changed"
		      (lambda ()
			(let ((val (gtk-adjustment-value zoom-a)))
			  (gossip-canvas-set-pixels-per-unit canvas val))))

  (gtk-standalone-main win))
