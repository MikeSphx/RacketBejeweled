;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Bejeweled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define GRID-WIDTH 8)
(define GRID-HEIGHT 8)

(define SPACE-LENGTH 40)

; DATA DEFINITIONS

; A Piece is a (make-piece Number Number Number)
(define-struct piece (x y type))
; Blue Circle    "BC"  0
; Blue Diamond    "BG"  1
; Purple Triangle "PT"  2
; Red Square      "RS"  3
; Yellow Diamond  "YD"  4
; Orange Hexagon  "OH"  5
; Green Hexagon   "GH"  6
(define test-wc (make-piece 0 0 0))
(define test-bg (make-piece 0 0 1))
(define test-pt (make-piece 0 0 2))
(define test-rs (make-piece 0 0 3))
(define test-yd (make-piece 0 0 4))
(define test-oh (make-piece 0 0 5))
(define test-gh (make-piece 0 0 6))

; A PSet is one of:
; - empty
; - (cons Piece Pset)
(define test-list1 empty)
(define test-list2 (list test-wc))
(define test-list3 (list test-wc test-bg))
(define test-list4 (list test-wc test-bg test-pt test-rs))

; x->+ yV-
; (0,1) (1,1) (2,1) (3,1) (4,1) (5,1) (6,1) (7,1)
; (0,0) (1,0) (2,0) (3,0) (4,0) (5,0) (6,0) (7,0)

; A World is a (make-world Pset Number)
(define-struct world (pile score))

#;(define test-world1 (make-world (generate-pile empty) 0))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

; GENERATE WORLD

; generate-pile : Pset -> Pset
; creates a random set of pieces to fill the board
#;(define (generate-pile ps)
  (cond [(empty? ps) (generate-new-pile empty (make-piece 0 0 (random 7)))]))


; generate-new-pile : Pset Posn -> Pset
; creates a brand new, randomized bejeweled board
(define (generate-new-pile ps last)
  (if (pile-full? ps)
      ps
      (generate-new-pile (add-piece ps last) (update-posn last))))

; extract-groups : Pset -> Pset
; makes sure that no groups of three exist on the board
#;(define (extract-groups ps)
  (map switch-if-grouped ps))

; get-piece : Pset Posn -> Pset


; update-posn : Posn -> Posn
; increments the posn to represent the location of the piece placed
(define (update-posn p)
  (if (= (posn-x p) (- GRID-WIDTH 1))
      (make-posn 0 (+ (posn-y p) 1))
      (make-posn (+ (posn-x p) 1) (posn-y p))))

(check-expect (update-posn (make-posn 0 0)) (make-posn 1 0))
(check-expect (update-posn (make-posn 7 0)) (make-posn 0 1))

; add-piece : Pset Posn -> Pset
; adds a new piece to the given Pset
(define (add-piece ps last)
  (cons (new-piece last) ps))

(check-random (add-piece (list test-wc) (make-posn 0 0))
              (list (make-piece 1 0 (random 7)) test-wc))
(check-random (add-piece (list test-wc) (make-posn 7 0))
              (list (make-piece 0 1 (random 7)) test-wc))

; new-piece : Posn -> Piece
; generates a new piece based on the last one created
(define (new-piece last)
  (if (= (posn-x last) (- GRID-WIDTH 1))
      (make-piece 0 (+ (posn-y last) 1) (random 7))
      (make-piece (+ (posn-x last) 1) (posn-y last) (random 7))))

(check-random (new-piece (make-posn 0 0)) (make-piece 1 0 (random 7)))
(check-random (new-piece (make-posn 7 0)) (make-piece 0 1 (random 7)))

; pile-full? : Pset -> Boolean
; does the pile cover the entire board?
(define (pile-full? ps)
  (= (length ps) (* GRID-WIDTH GRID-HEIGHT)))

(check-expect (pile-full? test-list1) false)
(check-expect (pile-full? test-list2) false)
(check-expect (pile-full? test-list3) false)

; test-pile-full? : Pset -> Boolean
; tests if pile-full? would return true when full
(define (test-pile-full? ps)
  (= (length ps) (* 2 2)))

(check-expect (test-pile-full? test-list1) false)
(check-expect (test-pile-full? test-list4) true)

; add-piece : Pset -> Pset
; adds a new random piece to the given piece set

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

; DRAW WORLD

(define BG (empty-scene 320 320))

; checker-board : Image -> Image

#;(define (checker-board img)
  (checker-board-help img 0 0))


; checker-board-help : Image Number Number -> Image

 

; draw-world : World -> Image
; takes in a world and shows it to the player
(define (draw-world w)
  (draw-pile (world-pile w) BG))

; draw-pile : Pset Image -> Image
; takes in a pile and draws it on top of the other image
(define (draw-pile ps img)
  (cond [(empty? ps) img]
        [(cons? ps) (place-image (pick-piece (piece-type (first ps)))
                                 (process-x (piece-x (first ps)))
                                 (process-y (piece-y (first ps)))
                                 (draw-pile (rest ps) img))]))

; process-x : Number -> Number
; translates a grid coord to a actual pixel coord
(define (process-x num)
  (+ (* SPACE-LENGTH num) (/ SPACE-LENGTH 2)))

; process-y : Number -> Number
;
(define (process-y num)
  (- (* SPACE-LENGTH GRID-HEIGHT) (+ (* SPACE-LENGTH num) (/ SPACE-LENGTH 2))))

; pick-piece : Number -> Image
; returns an image based on the number given
(define (pick-piece num)
  (cond [(= num 0) (circle (- (/ SPACE-LENGTH 2) (/ SPACE-LENGTH 8))
                           "solid" "blue")]
        [(= num 1) (triangle (- SPACE-LENGTH (/ SPACE-LENGTH 4))
                           "solid" "blue")]
        [(= num 2) (triangle (- SPACE-LENGTH (/ SPACE-LENGTH 4))
                           "solid" "purple")]
        [(= num 3) (square (- SPACE-LENGTH (/ SPACE-LENGTH 4))
                           "solid" "red")]
        [(= num 4) (square (- SPACE-LENGTH (/ SPACE-LENGTH 4))
                           "solid" "yellow")]
        [(= num 5) (circle (- (/ SPACE-LENGTH 2) (/ SPACE-LENGTH 8))
                           "solid" "orange")]
        [(= num 6) (circle (- (/ SPACE-LENGTH 2) (/ SPACE-LENGTH 8))
                           "solid" "green")]))

(define PILE (generate-new-pile (list (make-piece 0 0 (random 7))) (make-posn 0 0)))
PILE
(define WORLD (make-world (generate-new-pile PILE (make-posn 0 0)) 0))
(draw-world WORLD)

; try running the code
; to-do
; prevent generate from making groups of 3



























