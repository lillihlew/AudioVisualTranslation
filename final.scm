; Group Members: Salif, Lilli, Maritza, Alex
; Topic: Composing Shapes With Music
; Our project idea is to create images from music using many helper functions which ultimately produces a master function (notes->picture).
; List of helper functions we want to implement: duration->size, midi->type, random-colors, reverse-function, and a function that matchespar/seq with beside/above to represent how the music is being played.
; Then, each group member will create their own beat/song. Trying to turn compositions into pictures/images.
; Milestone #1 outline below:

(import music)
(import image)

; ; duration determines size of shapes (let function)
; ;       wn -> 320
; ;       hn -> 160
; ;       en -> 80
; ;       sn -> 40
; ;       tn -> 20

; ; (duration->size (duration)) -> integer?
; ;   duration: duration?
; ; Returns a size corresponding to duration; the longer duration, the larger size

(define duration->size
  (lambda (duration)
      (cond
          [(equal? wn duration) 320]
          [(equal? hn duration) 160]
          [(equal? qn duration) 80]
          [(equal? en duration) 40]
          [(equal? sn duration) 20]
          [(equal? tn duration) 10]
          )))

; ; midi value determines type of shape (let function)
; ;       0-29 -> circle
; ;       30-39 -> triangle
; ;       40-49 -> square
; ;       50-59 -> pentagon
; ;       60-69 -> hexagon
; ;       70-79 -> heptagon
; ;       80-89 -> octagon
; ;       90-99 -> nonagon
; ;       100-109 -> decagon
; ;       110-119 -> hendecagon
; ;       120-128 -> dodecagon
; (define midi->type
;     (lambda (midi)))
; ; (midi->type (midi)) -> shape
; ;   midi: integer? between 0 and 128
; ; Returns the shape corresponding to the midi value: the tens integer of the
; ; MIDI value will determine the number of sides the shape has.
; ; color is random
; ;       random color generator

; ; (random-colors) -> list?
; ; Returns a list of four integers, the first from 0 to 255, which will determine the color,
; ; and the last from 0-1, which will determine transparency.

(define random-colors
  (color (random 256), (random 256), (random 256), (/ (+ 6 (random 5)) 10)))


; ; maybe try to match par and seq with above and beside
; ;       par -> beside
; ;       seq -> above

(define par-or-seq
    (lambda (x)
        (cond
            [(equal? x par) above]
            [(equal? x seq) beside])))

((par-or-seq par) (circle 50 "solid" random-colors) (circle 50 "solid" random-colors))

; ; each group member makes own beat/song
; ;       maybe try to make songs match drawing
; ;       (salif suggested making spongebob come from spongebob intro);
; ;(salif-function) -> comp?
; ;Returns a composition in Salif's style!

; ; see what images would sound like as music if we put them through the reverse of our function
; (define reverse-function
;     (lambda (image)
;         ()))


; Milestone 2

(define circle-one
  (lambda (duration)
      (circle (/ (duration->size duration) 2) "solid" random-colors)))
 
(define triangle-one
  (lambda (duration)
      (triangle (duration->size duration) "solid" random-colors)))
 
(define square-one
  (lambda (duration)
      (overlay
           (square (* .9 (duration->size duration)) "solid" random-colors)
           (square (duration->size duration) "solid" "white"))))
 
(define pentagon
 (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [downsize (lambda (x) (* length x))])
         (path
             length
             length
             (list
                 (pair (downsize .5) (downsize .05))
                 (pair (downsize .964) (downsize .4))
                 (pair (downsize .8) (downsize .95))
                 (pair (downsize .2) (downsize .95))
                 (pair (downsize .036) (downsize .4))
                 (pair (downsize .5) (downsize .05)))
             "solid"
             random-colors))))
 
(define hexagon
 (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [downsize (lambda (x) (* length x))])
         (path
             length
             length
             (list
                 (pair 0 (downsize .5))
                 (pair (downsize 0.2857) (downsize 0.0625))
                 (pair (downsize 0.7142) (downsize 0.0625))
                 (pair length (downsize 0.5))
                 (pair (downsize 0.7142) (downsize 0.9375))
                 (pair (downsize 0.2857) (downsize 0.9375))
                 (pair 0 (downsize 0.5)))
             "solid"
             random-colors))))
 
(define heptagon
   (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [scale (lambda (x) (/ x 9))]
          [downsize (lambda (y) (* length (scale y)))])
         (path
             length
             length
             (list
                 (pair (downsize 4.5) (downsize 0.25))
                 (pair (downsize 8) (downsize 1.8))
                 (pair (downsize 8.75) (downsize 5.75))
                 (pair (downsize 6.5) (downsize 8.75))
                 (pair (downsize 2.5) (downsize 8.75))
                 (pair (downsize .25) (downsize 5.75))
                 (pair (downsize 1) (downsize 1.8))
                 (pair (downsize 4.5) (downsize 0.25)))
             "solid"
             random-colors))))
 
(define octagon
  (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [scale (lambda (x) (/ x 5))]
          [downsize (lambda (y) (* length (scale y)))])
         (path
             length
             length
             (list
               (pair (downsize 3.5) (downsize 0.1))
               (pair (downsize 4.9) (downsize 1.5))
               (pair (downsize 4.9) (downsize 3.5))
               (pair (downsize 3.5) (downsize 4.9))
               (pair (downsize 1.5) (downsize 4.9))
               (pair (downsize 0.1) (downsize 3.5))
               (pair (downsize 0.1) (downsize 1.5))
               (pair (downsize 1.5) (downsize 0.1))
               (pair (downsize 3.5) (downsize 0.1)))
              "solid"
              random-colors))))
 
(define nonagon
   (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [downsize (lambda (x) (* length x))])
         (path
             length
             length
             (list
                 (pair (downsize .5) 0.02)
                 (pair (downsize .83) (downsize .14))
                 (pair (downsize 1) (downsize .43))
                 (pair (downsize .935) (downsize .77))
                 (pair (downsize .675) (downsize .98))
                 (pair (downsize .325) (downsize .98))
                 (pair (downsize .065) (downsize .77))
                 (pair 0 (downsize .43))
                 (pair (downsize .17) (downsize .14))
                 (pair (downsize .5) 0.02))
             "solid"
             random-colors))))

(define decagon
  (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [scale (lambda (x) (/ x 16))]
          [downsize (lambda (y) (* length (scale y)))])
         (path
             length
             length
             (list
          (pair (downsize 10) (downsize 0))
          (pair (downsize 14) (downsize 2))
          (pair (downsize 16) (downsize 6))
          (pair (downsize 16) (downsize 10))
          (pair (downsize 14) (downsize 14))
          (pair (downsize 10) (downsize 16))
          (pair (downsize 6) (downsize 16))
          (pair (downsize 2) (downsize 14))
          (pair (downsize 0) (downsize 10))
          (pair (downsize 0) (downsize 6))
          (pair (downsize 2) (downsize 2))
          (pair (downsize 6) (downsize 0))
          (pair (downsize 10) (downsize 0)))
          "solid"
          random-colors
          ))))

(define hendecagon
 (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [scale (lambda (x) (/ x 360))]
          [downsize (lambda (y) (* length (scale y)))])
         (path
             length
             length
             (list
                 (pair (downsize 180) (downsize 5))
                 (pair (downsize 275) (downsize 35))
                 (pair (downsize 340) (downsize 110))
                 (pair (downsize 355) (downsize 210))
                 (pair (downsize 313) (downsize 301))
                 (pair (downsize 230) (downsize 355))
                 (pair (downsize 130) (downsize 355))
                 (pair (downsize 47) (downsize 301))
                 (pair (downsize 5) (downsize 210))
                 (pair (downsize 20) (downsize 110))
                 (pair (downsize 85) (downsize 35))
                 (pair (downsize 180) (downsize 5)))
             "solid"
             random-colors))))

(define dodecagon
  (lambda (duration)
     (let*
         ([length (duration->size duration)]
          [scale (lambda (x) (/ x 16))]
          [downsize (lambda (y) (* length (scale y)))])
         (path
             length
             length
             (list
                  (pair (downsize 0) (downsize 10))
                  (pair (downsize 2) (downsize 14))
                  (pair (downsize 6) (downsize 16))
                  (pair (downsize 10) (downsize 16))
                  (pair (downsize 14) (downsize 14))
                  (pair (downsize 16) (downsize 10))
                  (pair (downsize 16) (downsize 6))
                  (pair (downsize 14) (downsize 2))
                  (pair (downsize 10) (downsize 0))
                  (pair (downsize 6) (downsize 0))
                  (pair (downsize 2) (downsize 2))
                  (pair (downsize 0) (downsize 6))
                  (pair (downsize 0) (downsize 10)))
              "solid"
             random-colors))))

(define rest-one
    (lambda (duration)
    (square (duration->size duration) "solid" "white")))

(define midi->type
  (lambda (midi duration)
      (cond
          [(equal? midi rest) (rest-one duration)]
          [(and (<= 0 midi) (>= 29 midi)) (circle-one duration)]
          [(and (<= 30 midi) (>= 39 midi)) (triangle-one duration)]
          [(and (<= 40 midi) (>= 49 midi)) (square-one duration)]
          [(and (<= 50 midi) (>= 59 midi)) (pentagon duration)]
          [(and (<= 60 midi) (>= 69 midi)) (hexagon duration)]
          [(and (<= 70 midi) (>= 79 midi)) (heptagon duration)]
          [(and (<= 80 midi) (>= 89 midi)) (octagon duration)]
          [(and (<= 90 midi) (>= 99 midi)) (nonagon duration)]
          [(and (<= 100 midi) (>= 109 midi)) (decagon duration)]
          [(and (<= 110 midi) (>= 119 midi)) (hendecagon duration)]
          [(and (<= 120 midi) (>= 128 midi)) (dodecagon duration)]
          )))

(midi->type rest hn)
(midi->type 4 hn)
(midi->type 14 hn)
(midi->type 24 hn)
(midi->type 34 hn)
(midi->type 44 hn)
(midi->type 54 hn)
(midi->type 64 hn)
(midi->type 74 hn)
(midi->type 84 hn)
(midi->type 94 hn)
(midi->type 104 hn)
(midi->type 114 hn)
(midi->type 124 hn)



