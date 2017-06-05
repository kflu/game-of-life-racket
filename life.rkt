;; Conway's game of life
;; https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
;; 
;; It draws and updates the board on the terminal console. It requires the
;; terminal to be capable of handling basic ANSI control sequences. For *nix
;; terminals this is usually not a problem. For windows, it works at least for
;; Windows 10 regular command line window.

#lang racket

(provide (all-defined-out))

;; the board is a 2D matrix with `0` being dead and `1` being alive.
(define (make-matrix m n) (for/vector ([i (in-range 0 m)]) (make-vector n 0)))
(define (mat-ref mat x y) (vector-ref (vector-ref mat x) y))
(define (mat-set! mat x y v) (vector-set! (vector-ref mat x) y v))

(define (mat-dimension mat) 
  (cons (vector-length mat)
        (vector-length (vector-ref mat 0))))

(define (in-range? low high v)
  (and 
    (low . <= . v)
    (v . < . high)))

;; -------------------------------------------------------
;; `get-now` and `set-next!` that truncate at the boundary
;; -------------------------------------------------------
;  (define (get-now board x y)
;    (match-define (cons m n) (mat-dimension board))
;    (if (and (in-range? 0 m x)
;             (in-range? 0 n y))
;      (bitwise-and (mat-ref board x y) 1) 
;      0))
;  
;  (define (set-next! board x y v)
;    (match-define (cons m n) (mat-dimension board))
;    (if (and (in-range? 0 m x)
;             (in-range? 0 n y))
;      (let* ([v (arithmetic-shift v 1)]
;             [cur (mat-ref board x y)]
;             [res (bitwise-ior cur v)])
;        (mat-set! board x y res))
;      (void)))

;; ------------------------------------------------
;; `get-now` and `set-next!` with circular boundary
;; ------------------------------------------------
(define (get-now board x y)
  (match-define (cons m n) (mat-dimension board))
  (let ([x (modulo (+ x m) m)]
        [y (modulo (+ y n) n)])
    (bitwise-and (mat-ref board x y) 1)))

(define (set-next! board x y v)
  (match-define (cons m n) (mat-dimension board))
  (let ([x (modulo (+ x m) m)]
        [y (modulo (+ y n) n)])
    (let* ([v (arithmetic-shift v 1)]
           [cur (mat-ref board x y)]
           [res (bitwise-ior cur v)])
      (mat-set! board x y res))))

(define (count-surround board x y)
  (+
    (get-now board (- x 1) (- y 1))
    (get-now board (- x 1) y)
    (get-now board (- x 1) (+ y 1))
    (get-now board x       (+ y 1))
    (get-now board (+ x 1) (+ y 1))
    (get-now board (+ x 1) y)
    (get-now board (+ x 1) (- y 1))
    (get-now board x       (- y 1))))

(define (update-board board)
  (match-define (cons m n) (mat-dimension board))

  ; This is a two stage update. The first stage figures out the state for a
  ; cell after the update. The second stage actually sets the state. bit 0 is
  ; the cell state for current iteration; bit 1 is the cell state for the next
  ; iteration.

  ; stage 1: set next state for each cell
  (for* ([i (in-range 0 m)]
         [j (in-range 0 n)])
        (define surround (count-surround board i j))
        (cond
          [(= 1 (get-now board i j)) 
           (if (or (surround . < . 2)
                   (surround . > . 3))
             (set-next! board i j 0)
             (set-next! board i j 1))]
          [else (if (= surround 3) 
                     (set-next! board i j 1) 
                     (set-next! board i j 0))]))

  ; stage 2: move next state to current
  (for* ([i (in-range 0 m)]
         [j (in-range 0 n)])
        (define v (mat-ref board i j))
        (mat-set! board i j (arithmetic-shift v -1))))

(module* main #f

    (define SLEEP 0.2)

    ;; Clears the screen and put the cursor at (0,0) by using ANSI control sequence.
    ;; https://msdn.microsoft.com/en-us/library/windows/desktop/mt638032(v=vs.85).aspx
    (define (clear-screen) 
      (display "\e[2J")
      (display "\e[0;0H"))

    (define (draw-board board)
      (match-define (cons m n) (mat-dimension board))
      (clear-screen)
      (for ([line board])
           (for ([c line]) (display (if (= (bitwise-and c 1) 0) "." "o")))
           (displayln "")))

    ;; Make a mutable matrix from an immutable vector (from vector literal)
    (define (make-mutable board)
      (match-define (cons m n) (mat-dimension board))
      (for/vector ([row board]) (for/vector ([v row]) v)))

    (define board
      (make-mutable
        #( 
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 1 0 0 0 0 0 0)
           #(0 1 0 1 0 0 0 0 0 0)
           #(0 0 1 1 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           #(0 0 0 0 0 0 0 0 0 0)
           )))

    (draw-board board)
    (sleep SLEEP)

    ;; Infinit loop to keep updating and drawing the board
    (letrec ([loop (lambda () 
                     (update-board board)
                     (draw-board board)
                     (sleep SLEEP)
                     (loop))])
      (loop))

)
