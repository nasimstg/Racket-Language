#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;; Define the size of the toolbar and the canvas
(define toolbar-width 40)
(define toolbar-height 200)
(define canvas-width 400)
(define canvas-height 400)


;; A World is a structure:
;; - shapes: (listof Shape), represents all the shapes drawn so far
;; - tool: symbol, represents the currently selected tool
(struct World [shapes tool start end])

;; A Shape is one of:
;; - Circle
;; - Rectangle
;; interp: represents a shape image
;; Required methods:
;; render : Shape -> Image

(struct Shape [render])


;; render : Shape -> Image
;; "dispatch" function for Shape values
(define (render shape)
  (cond [(circ? shape) (render-circ shape)]
        [(rect? shape) (render-rect shape)]
        [(square? shape) (render-square shape)]
        [(tri? shape) (render-tri shape)]
        [else (error "Unknown shape type")]))

;; A Circle is a Shape constructed with:
;;   (circ render : Circle -> Image
;;              r : nonnegative-number
;;            col : Color)
;; interp:
;; - render is a function that converts the circ into a (2htdp/image) Image
;; - r is the circle radius
;; - col is the (2htdp/image) Color
(struct circ Shape [pos r col]) ;; include pos in the structure

;; A Rectangle is a Shape constructed with:
(struct rect Shape [pos width height col])

;; A Square is a Shape constructed with:
(struct square Shape [pos side col])

;; A Triangle is a Shape constructed with:
(struct tri Shape [pos side col])

;; Compute the distance between two points
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p2) (posn-x p1))) (sqr (- (posn-y p2) (posn-y p1))))))

;; Create a rectangle with the given two points as the top-left and bottom-right corners
(define (create-rect p1 p2)
  (let* ([midpoint (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
                              (/ (+ (posn-y p1) (posn-y p2)) 2))]
         [top-left (make-posn (min (posn-x p1) (posn-x p2)) (min (posn-y p1) (posn-y p2)))]
         [bottom-right (make-posn (max (posn-x p1) (posn-x p2)) (max (posn-y p1) (posn-y p2)))]
         [width (abs (- (posn-x top-left) (posn-x bottom-right)))]
         [height (abs (- (posn-y top-left) (posn-y bottom-right)))]
         [color "black"])
    (rect midpoint width height color)))

;; Create a circle with the given two points as the diameter
(define (create-circ p1 p2)
  (let* ([midpoint (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
                              (/ (+ (posn-y p1) (posn-y p2)) 2))]
         [radius (/ (distance p1 p2) 2)])
    (circ midpoint radius "black")))

;; Create a square with the given two points as the top-left and bottom-right corners
(define (create-square p1 p2)
  (let* ([midpoint (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
                              (/ (+ (posn-y p1) (posn-y p2)) 2))]
         [side (max (abs (- (posn-x p1) (posn-x p2))) (abs (- (posn-y p1) (posn-y p2))))] ;; The side is the maximum distance
         [color "black"])
    (square midpoint side color)))

;; Create a triangle with the given two points as the top-left and bottom-right corners
(define (create-tri p1 p2)
  (let* ([midpoint (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
                              (/ (+ (posn-y p1) (posn-y p2)) 2))]
         [side (max (abs (- (posn-x p1) (posn-x p2))) (abs (- (posn-y p1) (posn-y p2))))] ;; The side is the maximum distance
         [color "black"])
    (tri midpoint side color)))

;; Move the shape by the given x and y distances
(define (move-shape shape dx dy)
  (match shape
    [(rect render pos w h color) (rect render (make-posn (+ (posn-x pos) dx) (+ (posn-y pos) dy)) w h color)]
    [(circ render pos r color) (circ render (make-posn (+ (posn-x pos) dx) (+ (posn-y pos) dy)) r color)]
    [(square render pos side color) (square render (make-posn (+ (posn-x pos) dx) (+ (posn-y pos) dy)) side color)]
    [(tri render pos side color) (tri render (make-posn (+ (posn-x pos) dx) (+ (posn-y pos) dy)) side color)]))

;; Handle mouse clicks in the canvas
(define (handle-click world x y event)
  (cond 
    [(string=? event "move")
     (if (or (< x 0) (> x canvas-width) (< y 0) (> y canvas-height))
         (World (World-shapes world) (World-tool world) (World-start world) (World-end world))
         world)]
    [(string=? event "button-down")
     (if (< x toolbar-width)
         (cond [(< y 40) (World (World-shapes world) 'pointer #f #f)]
               [(< y 80) (World (World-shapes world) 'circle #f #f)]
               [(< y 120) (World (World-shapes world) 'rectangle #f #f)]
               [(< y 160) (World (World-shapes world) 'square #f #f)]
               [(< y 200) (World (World-shapes world) 'triangle #f #f)]
               [else world])
         (World (World-shapes world) (World-tool world) (make-posn x y) #f))]
    [(string=? event "drag")
     (if (and (World-start world) (not (< x toolbar-width)))
         (if (eq? (World-tool world) 'pointer)
             (let ([dx (- x (posn-x (World-start world)))]
                   [dy (- y (posn-y (World-start world)))])
               (World (map (lambda (shape) (move-shape shape dx dy)) (World-shapes world)) (World-tool world) (make-posn x y) #f))
             (World (World-shapes world) (World-tool world) (World-start world) (make-posn x y)))
         world)]
    [(string=? event "button-up")
     (if (and (World-start world) (World-end world) (not (< x toolbar-width)))
         (match (World-tool world)
           ['pointer (World (World-shapes world) 'pointer #f #f)]
           ['circle (World (cons (create-circ (World-start world) (World-end world)) (World-shapes world)) 'circle #f #f)]
           ['rectangle (World (cons (create-rect (World-start world) (World-end world)) (World-shapes world)) 'rectangle #f #f)]
           ['square (World (cons (create-square (World-start world) (World-end world)) (World-shapes world)) 'square #f #f)]
           ['triangle (World (cons (create-tri (World-start world) (World-end world)) (World-shapes world)) 'triangle #f #f)])
         world)]
    [else world]))

;; Render the current state of the world as an image
(define (draw-world world)
  (define scene (empty-scene canvas-width canvas-height))
  (define scene-with-shapes
    (foldl (lambda (shape scene)
             (place-image (Shape-render shape) (posn-x (Shape-pos shape)) (posn-y (Shape-pos shape)) scene))
           scene
           (World-shapes world)))
  (if (and (World-start world) (World-end world))
      (let* ([start (World-start world)]
             [end (World-end world)]
             [midpoint (make-posn (/ (+ (posn-x start) (posn-x end)) 2)
                                 (/ (+ (posn-y start) (posn-y end)) 2))])
        (place-image (Shape-render (match (World-tool world)
                                     ['circle (create-circ start end)]
                                     ['rectangle (create-rect start end)]
                                     ['square (create-square start end)]
                                     ['triangle (create-tri start end)]))
                     (posn-x midpoint)
                     (posn-y midpoint)
                     scene-with-shapes))
      scene-with-shapes))

;; Create the toolbar
(define (create-toolbar world)
  (define pointer-button (overlay (text "P" 18 (if (eq? (World-tool world) 'pointer) "red" "black")) (rectangle toolbar-width 40 "outline" "black")))
  (define circle-button (overlay (text "C" 18 (if (eq? (World-tool world) 'circle) "red" "black")) (rectangle toolbar-width 40 "outline" "black")))
  (define rectangle-button (overlay (text "R" 18 (if (eq? (World-tool world) 'rectangle) "red" "black")) (rectangle toolbar-width 40 "outline" "black")))
  (define square-button (overlay (text "S" 18 (if (eq? (World-tool world) 'square) "red" "black")) (rectangle toolbar-width 40 "outline" "black")))
  (define triangle-button (overlay (text "T" 18 (if (eq? (World-tool world) 'triangle) "red" "black")) (rectangle toolbar-width 40 "outline" "black")))

  (place-images (list pointer-button circle-button rectangle-button square-button triangle-button)
                (list (make-posn (/ toolbar-width 2) 20)
                      (make-posn (/ toolbar-width 2) 60)
                      (make-posn (/ toolbar-width 2) 100)
                      (make-posn (/ toolbar-width 2) 140)
                      (make-posn (/ toolbar-width 2) 180))
                (empty-scene toolbar-width toolbar-height)))

;; Run the big-bang interactive program
(big-bang (World '() 'pointer #f #f)
          [to-draw (lambda (world) (place-image (create-toolbar world) 20 100 (draw-world world)))]
          [on-mouse handle-click])
