#lang racket
(require racket/draw)
(define (elliptic* x y)
  (make-rectangular (* (real-part x) (real-part y))
                    (* (imag-part x) (imag-part y))))
(struct linden-ctx (init rules target pos angle turn step thick fore palette frame) #:transparent)
(define (linden-mixin ctx width height)
  (mixin (dc<%>) (dc<%>)
    (super-new)
    (init-field [show #t])
    (field [state (linden-ctx-init ctx)]
           [fore (linden-ctx-fore ctx)]
           [frame (linden-ctx-frame ctx)]
           [pos (elliptic* (linden-ctx-pos ctx) (/ (make-rectangular width height)
                                                   (linden-ctx-target ctx)))]
           [thick (linden-ctx-thick ctx)]
           [angle (linden-ctx-angle ctx)]
           [step (* (linden-ctx-step ctx) (/ (max width height) (linden-ctx-target ctx)))])
    (rename-super [super-background set-background])
    (inherit draw-line draw-arc set-pen set-brush clear draw-point)
    (define (set-pos! dest)
      (when show
        (draw-point (roundpt (real-part pos)) (roundpt (imag-part pos)))
        (set-pen "red" thick 'solid)
        (draw-point (roundpt (real-part dest)) (roundpt (imag-part dest)))
        (refresh-pen))
      (set! pos dest))
    (define/public (reset-carrier)
      (set! angle (linden-ctx-angle ctx))
      (set-pos! (elliptic* (linden-ctx-pos ctx) (/ (make-rectangular width height)
                                                   (linden-ctx-target ctx)))))
    (define/public (refresh-pen)
      (set-pen (list-ref (linden-ctx-palette ctx)
                         (modulo fore (length (linden-ctx-palette ctx))))
               thick 'solid))
    (define/override (set-background col)
      (set! ctx (struct-copy linden-ctx ctx [palette (list-set (linden-ctx-palette ctx) 0 col)]))
      (clear)
      (let ([f frame]) (reset-state) (next f)))
    (set-brush "black" 'transparent)
    (refresh-pen) (super-background (car (linden-ctx-palette ctx))) (clear)
    (define/public (reset-pen)
      (set! fore (linden-ctx-fore ctx)) (set! thick (linden-ctx-thick ctx))
      (set! step (* (linden-ctx-step ctx) (/ (max width height) (linden-ctx-target ctx))))
      (refresh-pen))
    (define/public (reset-state)
      (set! frame (linden-ctx-frame ctx)) (set! state (linden-ctx-init ctx)))
    (define/public (get-context)
      (struct-copy linden-ctx ctx [frame frame]))
    (define/public (set-context c [clear? #f] [run? #f])
      (set! ctx c)
      (reset-carrier)
      (reset-state)
      (reset-pen)
      (when clear? (clear))
      (execute state)
      (when run? (next (linden-ctx-frame ctx))))
    (define/public (next [k 1] [ext (λ (c) #f)])
      (unless (< k 1)
        (set! frame (add1 frame))
        (let ([delete-next? #f])
          (set! state (string-join (map (λ (x) (cond [(hash-has-key? (linden-ctx-rules ctx) x)
                                                      (hash-ref (linden-ctx-rules ctx) x)]
                                                     [(char=? x #\\)
                                                      (if delete-next? "\\"
                                                          (and (set! delete-next? #t) ""))]
                                                     [delete-next? (set! delete-next? #f) ""]
                                                     [else (string x)]))
                                        (string->list state)) "")))
        (if (> k 1) (next (sub1 k) ext)
            (execute state ext))))
    (define/public (roundpt x)
      (exact-floor (+ x 0.5)))
    (define/public (do-arc [positive? #t])
      (let* ([a (* 2 pi angle)]
             [p (make-polar step a)]
             [center (+ pos p)]
             [tmp (+ center (* step -1-1i))])
        (set-pos! (+ center p))
        (if positive?
            (draw-arc (roundpt (real-part tmp)) (roundpt (imag-part tmp))
                      (* 2 step) (* 2 step) a (+ pi a))
            (draw-arc (roundpt (real-part tmp)) (roundpt (imag-part tmp))
                      (* 2 step) (* 2 step) (+ pi a) a))))
    (define/public (do-line dest)
      (draw-line (roundpt (real-part pos)) (roundpt (imag-part pos))
                 (roundpt (real-part dest)) (roundpt (imag-part dest)))
      (set-pos! dest))
    (define (exec c [ext (λ (c) #f)])
      (or (ext c)
          (case c
            [(#\F) (do-line (+ pos (make-polar step (* 2 pi angle))))]
            [(#\B) (do-line (- pos (make-polar step (* 2 pi angle))))]
            [(#\-) (set! angle (- angle (linden-ctx-turn ctx)))]
            [(#\+) (set! angle (+ angle (linden-ctx-turn ctx)))]
            [(#\U) (set-pen "black" 1 'transparent)]
            [(#\D) (refresh-pen)]
            [(#\^) (set! thick (min 255 (add1 thick)))]
            [(#\~) (set! thick (max 1 (sub1 thick)))]
            [(#\*) (set! step (* step 2))]
            [(#\/) (set! step (max 1 (/ step 2)))]
            [(#\@) (set! step (add1 step))]
            [(#\!) (set! step (max 1 (sub1 step)))]
            [(#\#) (reset-carrier)]
            [(#\%) (reset-pen)]
            [(#\$) (reset-state)]
            [(#\_) (clear)]
            [(#\>) (set! fore (add1 fore)) (refresh-pen)]
            [(#\<) (set! fore (sub1 fore)) (refresh-pen)]
            [(#\A) (do-arc #t)]
            [(#\C) (do-arc #f)]
            [else (void)])))
    (define (execute-list lst [ext (λ (c) #f)] [res1 void] [res2 void])
      (match lst
        ['() (res1) (res2)]
        [(list #\[ #\] rem ...)
         (execute-list rem ext res1 res2)]
        [(list #\( #\) rem ...)
         (execute-list rem ext res1 res2)]
        [(cons #\[ rem)
         (execute-list rem ext ((λ (a z res) (λ () (set! angle a) (set-pos! z) res))
                                angle pos res1) res2)]
        [(cons #\( rem)
         (execute-list rem ext res1
                       ((λ (c t s res) (λ () (set! fore c) (set! thick t) (set! step s) res))
                        fore thick step res2))]
        [(cons #\] rem)
         ((λ (res) (execute-list rem ext res res2)) (res1))]
        [(cons #\) rem)
         ((λ (res) (execute-list rem ext res1 res)) (res2))]
        [(cons c rem)
         (exec c ext) (execute-list rem ext res1 res2)]))
    (define/public (execute [str state] [ext (λ (c) #f)])
      (execute-list (string->list str) ext))
    (execute state)
    ))
(define (flremainder x y)
  (- x (* y (truncate (/ x y)))))
(define sema (make-semaphore))
(define (linden-bitmap-dc% ctx width height)
  (class ((linden-mixin ctx width height) bitmap-dc%)
    (super-new [bitmap (make-object bitmap% width height #f #t)])
    (rename-super [super-exec execute])
    (inherit-field pos fore step angle)
    (inherit roundpt get-argb-pixels set-argb-pixels do-line get-bitmap)
    (define (get-pixel-bytes x y)
      (let ([bs (make-bytes 4 255)])
        (get-argb-pixels x y 1 1 bs) bs))
    (define (color-bytes)
      (let ([c (make-object color% (list-ref (linden-ctx-palette ctx) fore))])
        (bytes (exact-floor (* 255 (send c alpha)))
               (send c red) (send c green) (send c blue))))
    (define (fill)
      (letrec ([x (roundpt (real-part pos))]
               [y (roundpt (imag-part pos))]
               [cb (color-bytes)]
               [bs (get-pixel-bytes x y)]
               [fill (λ (x y) (when (and (>= x 0) (>= y 0) (bytes=? bs (get-pixel-bytes x y)))
                                (set-argb-pixels x y 1 1 cb)
                                (fill (add1 x) y)
                                (fill (sub1 x) y)
                                (fill x (add1 y))
                                (fill x (sub1 y))))])
        (unless (bytes=? bs cb)
          (fill x y))))
    (define/override (execute [str ""] [ext (λ (c) #f)])
      (super-exec str (λ (c)
                        (or (ext c)
                            (case c
                              [(#\.) (fill)]
                              [(#\F #\B)
                               (let ([dest ((if (char=? c #\F) + -) pos (make-polar step (* 2 pi angle)))])
                                 (when (or (negative? (real-part dest)) (>= (real-part dest) width)
                                           (negative? (imag-part dest)) (>= (imag-part dest) height))
                                   (set! dest (make-rectangular (flremainder (real-part dest) (exact->inexact width))
                                                                (flremainder (imag-part dest) (exact->inexact height)))))
                                 (do-line dest))]
                              [else #f])))))
    ))
(define (make-linden init rules [target 1] [pos 0] [angle 0] [turn 1/4] [thick 1] [fore 1] [frame 0]
                     #:palette [palette '("white" "black")]
                     #:step [step 1/100])
  (linden-ctx init rules target pos angle turn step thick fore palette frame))
(define hilbert (make-linden "X"
                             (make-hash '((#\X . "+YF-XFX-FY+")
                                          (#\Y . "-XF+YFY+FX-")))
                             5 60/400+60/400i #:step 60/400))
(define moore (make-linden "LFL+F+LFL"
                           (make-hash '((#\L . "-RF+LFL+FR-")
                                        (#\R . "+LF-RFR-FL+")))
                           4 48/400+824/400i 180/360 #:step 48/400))
(define fern (make-linden "G"
                          (make-hash '((#\G . "F-[[G]+G]+F[+FG]-G")
                                       (#\F . "FF")))
                          7 0+7i -1/7 25/360 #:step 35/1600))
(define tree (make-linden "X"
                          (make-hash '((#\X . "FY[-FX]+FX")
                                       (#\Y . "YFY")))
                          7 3.5+7i -90/360 45/360 #:step 2/40))
(define koch (make-linden "F++F++F" (make-hash '((#\F . "F-F++F-F")))
                          4 120/400+440/400i 120/360 60/360 #:step 4/100))
(define dragon (make-linden "\\FX"
                            (make-hash '((#\X . "X-Y\\F-")
                                         (#\Y . "+\\FX+Y")))
                            14 1890/400+14/4i #:step 42/400))
(define penrose (make-linden "[7F]++[7F]++[7F]++[7F]++[7F]"
                             (make-hash '((#\6 . "8F++9F----7F[-8F----6F]++")
                                          (#\7 . "+8F--9F[---6F--7F]+")
                                          (#\8 . "-6F++7F[+++8F++9F]-")
                                          (#\9 . "--8F++++6F[+9F++++7F]--7F")
                                          (#\F . "")))
                             6 3+3i 0 36/360 #:step 6/40))
(send* (new (linden-bitmap-dc% hilbert 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 5) (get-bitmap))
(send* (new (linden-bitmap-dc% moore 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 4) (get-bitmap))
(send* (new (linden-bitmap-dc% fern 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 7) (get-bitmap))
(send* (new (linden-bitmap-dc% tree 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 7) (get-bitmap))
(send* (new (linden-bitmap-dc% koch 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 4) (get-bitmap))
(send* (new (linden-bitmap-dc% dragon 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 14) (get-bitmap))
(send* (new (linden-bitmap-dc% penrose 400 400)) (clear)
  (set-smoothing 'unsmoothed) (next 6) (get-bitmap))

(require 2htdp/universe file/gif)
(define w (new (linden-bitmap-dc% fern 400 400)))
(define sema2 (make-semaphore 1))
(send w set-smoothing 'unsmoothed)
(define path "./linden/")
(define ct 0)
(thread (λ () (send w next 6 (λ (c) (when (char=? c #\F) (semaphore-wait sema)
                                       (semaphore-post sema2)
                                      ) #f))))
(let ([bs (make-bytes (* 400 400 4))])
  (call-with-output-file (string-append path "fern.gif")
    (λ (out) 
      (let ([s (gif-start out 400 400 0 #f)])
        (for ([i (in-range 6049)])
          (semaphore-wait sema2)
          (let-values ([(bs cmap tr)
                        (and (send (send w get-bitmap) get-argb-pixels 0 0 400 400 bs)
                             (quantize bs))])
            (gif-add-control s 'any #f 2 tr)
            (gif-add-image s 0 0 400 400 #f cmap bs)
            (semaphore-post sema)
            (writeln i)))
        (gif-end s)))))