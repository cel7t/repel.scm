#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 match)) ; for create-svg
(use-modules (srfi srfi-1)) ; for repel

;; definitions (updated when command line arguments are parsed)
(define canvas '())
(define points '())
(define max-iter 1000)
(define learn-rate 0.5)
(define min-pad 2)
(define radius 2)
(define font-size 12)
(define init-dst 5)

(define max-disp 0)

;; adjust the text
(define (adjust-text point-lst canvas)
  ;; generate a list of "rects"
  ;; each rect has an x, y plus a length
  (define rects
    (let loop [(rects '())
               (points point-lst)]
      (match points
        [() (reverse rects)]
        ;; ignore fixed rects for now
        [(((x . y) . (:fixed . some)) . rest)
         (loop rects rest)]
        [(((x . y) . label) . rest)
         (loop (cons `((,(string->number x) . ,(string->number y)) .
                       ,(* (string-length label) font-size))
                     rects)
               rest)]
        [strange (error "Strange input to adjust-text" strange)])))

  ;; move all rects, add font height
  (set! rects
    (map (match-lambda
           [((x . y) . length)
            `((,(- x init-dst) . ,(+ y init-dst)) .
              (,length . ,font-size))]
           [strange (error "Strange rect" strange)])
         rects))

  ;; IMPORTANT: overlap? assumes that rects is up-to-date
  ;; this means that rects should be updated with each iteration
  (define (overlap? rect)
    (match-let
        [(((rect-x . rect-y) . (rect-w . rect-h)) rect)]
      (and
       ;; check with the points
       (let loop [(points point-lst)]
         (match points
           [() #t]
           [(((x . y) . label) . rest)
            (let [(closest-x (max rect-x (min x (+ rect-x rect-w))))
                  (closest-y (max rect-y (min y (+ rect-y rect-h))))]
              (if (>= (sqrt (abs (- (expt (- closest-x x) 2)
                                    (expt (- closest-y y) 2))))
                      3)
                  (loop rest)
                  #f))]
           [strange (error "Strange point" strange)]))
       ;; check with other rects
       (let loop [(rect-lst rects)]
         (match rect-lst
           [() #t]
           [(((x . y) . (w . h)) . rest)
            ;; is it our rect?
            (if (equal? `((,x . ,y) . (,w . ,h)) rect)
                (loop rest)
                ;; not our rect!
                (if (not (or (<= (+ rect-x rect-w) x)
                             (>= rect-x (+ x w))
                             (<= (+ rect-y rect-h) y)
                             (>= rect-y (+ y h))))
                    (loop rest)
                    #f))]
           [strange (error "Strange rect" strange)])))))

  (define (repel rect point points)
    (let [(cw (string->number (car canvas)))
          (ch (string->number (cdr canvas)))]
      (match-let [(((x . y) . (w . h)) rect)
                  ((px . py) point)]
        (let [(rcx (+ x (/ w 2)))
              (rcy (+ y (/ h 2)))]

          ;; (define (point-repel pt force) force)
          (define (point-repel pt force)
            ;; (format #t "Point repel with force ~a~%" force)
            (let* [(vx (car pt))
                   (vy (cdr pt))
                   (fx (car force))
                   (fy (cdr force))
                   (dx (- rcx vx))
                   (dy (- rcy vy))
                   (dst (sqrt (+ (expt dx 2) (expt dy 2))))]
              (if (< dst min-pad)
                  (let [(overlap (- min-pad dst))]
                    `(,(+ fx (* overlap (/ dx (+ dst 1e-6)))) .
                      ,(+ fy (* overlap (/ dy (+ dst 1e-6))))))
                  (if (equal? point `(,vx . ,vy))
                      `(,(- fx (* dx 0.1)) .
                        ,(- fy (* dy 0.1)))
                      force))))

          ;; (define (rect-repel rect force) force)
          (define (rect-repel other-rect force)
            ;; (format #t "Rect repel with force ~a~%" force)
            (if (equal? rect other-rect)
                force
                (match-let [(((ox . oy) . (ow . oh)) other-rect)
                            ((fx . fy) force)]
                  (let [(ovx (- (min (+ x w) (+ ox ow)) (max x ox)))
                        (ovy (- (min (+ y h) (+ oy oh)) (max y oy)))]
                    (if (and (> ovx 0)
                             (> ovy 0))
                        `(,(+ fx
                              (* ovx 10 (if (< x ox) 1 -1)))
                          . ,(+ fy
                                (* ovy 10 (if (< y oy) 1 -1))))
                        force)))))

          ;; (define (boundary-repel force) force)
          (define (boundary-repel force)
            ;; (format #t "Boundary repel with force ~a~%" force)
            (match-let [((fx . fy) force)]
              `(,(+ fx
                    (if (< x 0)
                        (* 10 (- x))
                        0)
                    (if (> (+ x w) cw)
                        (* 10 (- cw (+ x w)))
                        0)) .
                        ,(+ fy
                            (if (< y 0)
                                (* 10 (- y))
                                0)
                            (if (> (+ y h) ch)
                                (* 10 (- ch (+ y h)))
                                0)))))
          
          (let* [(force
                  (boundary-repel
                   (fold rect-repel
                         (fold point-repel `(0 . 0) points)
                         rects)))]
            ;; (format #t "Force: ~a~%" force)
            (match-let [((fx . fy) force)]
              (let [(nx (max 0 (min (+ x (* learn-rate fx)) (- cw w))))
                    (ny (max 0 (min (+ y (* learn-rate fy)) (- ch h))))]
                (set! max-disp
                  (max max-disp
                       (sqrt (+ (expt (- nx x) 2)
                                (expt (- ny y) 2)))))
                `((,nx . ,ny) . (,w . ,h)))))))))
  
  (define (physics-sim)
    ;; (format #t "Starting sim~%")
    (let [(points (map (match-lambda
                         [((x . y) . label)
                          (cons (string->number x)
                                (string->number y))]
                         [strange (error "Strange point" strange)])
                       point-lst))]
      (let main-loop [(itr 0)]
        (if (or (> itr max-iter)
                (and (not (= itr 0))
                     (< max-disp 1e-3)))
            rects
            (begin
              ;; (format #t "Rects: ~a~%" rects)
              ;; (format #t "Points: ~a~%" points)
              (set! rects
                (map (lambda (rect point) (repel rect point points))
                     rects
                     points))
              (main-loop (+ itr 1)))))
      rects))
  
  (physics-sim)
  ;; (format #t "Rect: ~a~%" rects)

  (let loop [(new-rect '())
             (last-rect rects)]
    (match last-rect
      [() (reverse new-rect)]
      [(((x . y) . (w . h)) . rest)
       (loop (cons `((,(number->string x) . ,(number->string y)) .
                     (,(number->string w) . ,(number->string h)))
                   new-rect)
             rest)])))

;; svg output
(define (create-svg points canvas)
  (let [(header
         (let [(width (car canvas))
               (height (cdr canvas))]
           (string-append
            "<svg width='" width
            "' height='" height
            "' style='background-color:white;'>\n"
            "<rect width='" width
            "' height='" height
            "' fill='white'/>")))
        (definitions
          "<defs><marker id='arrowhead' markerWidth='10' markerHeight='7' refX='10' refY='3.5' orient='auto'>\n<polygon points='0 0, 10 3.5, 0 7' fill='black' /></marker></defs>")
        (footer "</svg>")
        (body
         (apply string-append
                (map
                 (lambda (point rect)
                   (match-let
                       [(((x . y) . label) point)
                        (((ax . ay) . (aw . ah)) rect)]
                     (string-append
                      ;; circle
                      "<circle cx='" x
                      "' cy='" y
                      "' r='" (number->string radius)
                      "' fill='blue'/>"
                      ;; arrow
                      "<line x1='" x
                      "' y1='" y
                      "' x2='" ax
                      "' y2='" ay
                      "' stroke='black' stroke-width='1' marker-end='url(#arrowhead)' />"
                      ;; label
                      "<text x='" ax
                      "' y='" ay
                      "' font-size='" (number->string font-size)
                      "' fill='black' font-family='monospace'>"
                      label
                      "</text>")))
                 ;; TEMPORARY removal of fixed points
                 ;; points
                 (let loop [(points-clean '())
                            (pts points)]
                   (match pts
                     [() (reverse points-clean)]
                     [(((x . y) . (:fixed . some)) . rest)
                      (loop points-clean rest)]
                     [(other . rest)
                      (loop (cons other points-clean) rest)]))
                 (adjust-text points canvas))))]
    (string-append header definitions body footer)))

;; get inputs
(define parse-inputs
  (match-lambda
    ;; custom values
    [(_ "custom" max-iterations learning-rate minimum-pad point-radius font-size-px initial-distance . rest)
     (begin
       (set! max-iter (string->number max-iterations))
       (set! learn-rate (string->number learning-rate))
       (set! min-pad (string->number minimum-pad))
       (set! radius (string->number point-radius))
       (set! font-size (string->number font-size-px))
       (set! init-dst (string->number initial-distance))
       (parse-inputs (cons 'dummy rest)))]
    [(_ width height . rest)
     (begin
       (set! canvas `(,width . ,height))
       (set! points
         (let loop [(points '())
                    (lst rest)]
           (match lst
             [() (reverse points)]
             [(x y label "fixed" fixed-x fixed-y . rest)
              (loop (cons `((,x . ,y) . (:fixed ,label (,fixed-x . ,fixed-y))) points)
                    rest)]
             [(x y label . rest)
              (loop (cons `((,x . ,y) . ,label) points)
                    rest)]
             [strange (error "Strange List: " strange)]))))]
    [strange (error "Malformed Arguments: " strange)]))

(parse-inputs (command-line))

(format #t "~a~%" (create-svg points canvas))