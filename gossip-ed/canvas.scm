(define-module (gossip canvas)
  :use-module (gtk dynlink))

(merge-compiled-code "gossip_canvas_init_glue" "libgossipcanvas")
