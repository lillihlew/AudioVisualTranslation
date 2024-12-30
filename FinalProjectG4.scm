; Group Members: Salif, Lilli, Maritza, Alex
; Topic: Composing Shapes With Music
; Our project idea is to create images from music using many helper functions which ultimately produces a master function 
;   (midi->type) and its partner function above-or-beside.
; List of helper functions we want to implement: duration->size, midi->type, random-colors, and above-or-beside.
; Then, each group member will create their own beat/song, trying to turn compositions into pictures/images.


; Final Project Below
; All functions are documented and have been implimented, with clear headings too follow. We hope you enjoy our songs ;)
; Acknowledgements: Mikey helped with the note struct, we used desmos to find points on graphs to create some of the shapes, 
;   and we used images found on google for the shapes we couldn't find on demsos.

; import music and image
(import music)
(import image)


"Final Project"
;;; Duration will determine the size of the shape
;;;       wn -> 320
;;;       hn -> 160
;;;       qn -> 80  
;;;       en -> 40
;;;       sn -> 20
;;;       tn -> 10
;;; (duration->size duration) -> integer?
;;; duration : dur?
;;; Takes duration as an input and returns a # size corresponding to duration, 
;;; the duration determines how big or small the image will be.
;;; The bigger or longer duration the bigger the image will be and vice versa.

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


;;; (random-colors)
;;; Returns a list of four integers, the first from 0 to 255, which will determine the color,
;;; and the last from 0.6-1, which will determine transparency.

(define random-colors
(color (random 256), 
       (random 256), 
       (random 256), 
       (/ (+ 6 (random 5)) 10)))


"Generalized Shape Functions"

;;; (circle-one duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid circle
;;; with a random color and size depending on the duration that is called.

(define circle-one
(lambda (duration)
    (circle (/ (duration->size duration) 2) "solid" random-colors)))
 
;Two examples
(circle-one wn)
(circle-one en)


;;; (triangle-one duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid triangle
;;; with a random color and size depending on the duration that is called.

(define triangle-one
(lambda (duration)
    (triangle (duration->size duration) "solid" random-colors)))
 
;Two examples
(triangle-one qn)
(triangle-one hn)


;;; (square-one duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid square
;;; with a random color and size depending on the duration that is called.

(define square-one
(lambda (duration)
    (overlay
         (square (* .9 (duration->size duration)) "solid" random-colors)
         (square (duration->size duration) "solid" "white"))))
 
;Two examples
(square-one en)
(square-one wn)


;;; (pentagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid pentagon (5 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(pentagon qn)
(pentagon wn)


;;; (hexagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid hexagon (6 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(hexagon hn)
(hexagon en)


;;; (heptagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid heptagon (7 sides)
;;; with a random color and size depend ingon the duration that is called.

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

;Two examples
(heptagon wn)
(heptagon qn)


;;; (octagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid octagon (8 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(octagon en)
(octagon hn)


;;; (nonagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid nonagon (9 sides)
;;; with a random color and size depending on the duration that is called.

(define nonagon
 (lambda (duration)
   (let*
       ([length (duration->size duration)]
        [downsize (lambda (x) (* length x))])
       (path
           length
           length
           (list
               (pair (downsize .5) (downsize 0.02))
               (pair (downsize .83) (downsize .14))
               (pair (downsize 1) (downsize .43))
               (pair (downsize .935) (downsize .77))
               (pair (downsize .675) (downsize .98))
               (pair (downsize .325) (downsize .98))
               (pair (downsize .065) (downsize .77))
               (pair 0 (downsize .43))
               (pair (downsize .17) (downsize .14))
               (pair (downsize .5) (downsize 0.02)))
           "solid"
           random-colors))))

;Two examples 
(nonagon sn)
(nonagon wn)


;;; (decagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid decagon (10 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(decagon wn)
(decagon qn)


;;; (hendecagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid hendecagon (11 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(hendecagon hn)
(hendecagon en)


;;; (dodecagon duration) -> image?
;;; duration : dur? 
;;; takes duration as an input which will be a number, and outputs a solid dodecagon (12 sides)
;;; with a random color and size depending on the duration that is called.

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
 
;Two examples
(dodecagon wn)
(dodecagon qn)


"Helper Functions"

;;; (rest-one duration) -> image? 
;;; duration : dur?
;;; takes duration as an input which will be a number, and outputs a solid white square 
;;; and size depending on the duration that is called.

(define rest-one
  (lambda (duration)
  (square (duration->size duration) "solid" "white")))

;;; (struct notee) -> Structure of a note
;;; We created a struct for notee, to hold the structure of a note in order for us to implement a function that takes a note
;;; (note midi-value duration) as input instead of just duration and outputs a shape. 

(struct notee (midi-value duration))

; midi->type outline plan
; The midi-value determines type of shape (cond function)
;       0-29 -> circle
;       30-39 -> triangle
;       40-49 -> square
;       50-59 -> pentagon
;       60-69 -> hexagon
;       70-79 -> heptagon
;       80-89 -> octagon
;       90-99 -> nonagon
;       100-109 -> decagon
;       110-119 -> hendecagon
;       120-128 -> dodecagon


"Master Function"

;;; (midi->type notee) -> image?
;;;   notee: note struct containing integer? between 0 and 128 and dur? 
;;; Takes a note as input with (struct notee) and outputs an image, which corresponds to the midi-value and duration.
;;; the tens integer of the MIDI value will determine the number of sides the shape has.
;;; Meanwhile the duration determines how big or small the shape will be.

(define midi->type
(lambda (notee)
   (let
    ([midi (notee-midi-value notee)]
     [duration (notee-duration notee)])
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
        ))))
 
 "Tests of Master"
;Example 1
(midi->type (notee 57 wn))

;Example 2
(midi->type (notee 13 en))

;Example 3
(midi->type (notee 112 hn))


"Implementatation of Master Functions"

;;; (above-or-beside par-seq) -> Images Beside or Above each other?
;;; par-seq : above or beside?
;;; Takes either par or seq as input and returns the images called either above or beside each other in 1 image. 
;;; Overall it turns par/seq to above/beside. Seq is beside and par is above.

(define above-or-beside
   (lambda (par-seq)
       (cond
           [(equal? par-seq seq) beside]
           [(equal? par-seq par) above])))
 

; Implimentation of the above-or-beside function (image)
((above-or-beside seq)
    (midi->type (notee 12 wn))
       (midi->type (notee 43 hn))
       (midi->type (notee 56 en))
       (midi->type (notee 17 sn))
       (midi->type (notee 123 hn)))
   ((above-or-beside par)
       (midi->type (notee 12 wn))
       (midi->type (notee 43 hn))
       (midi->type (notee 56 en))
       (midi->type (notee 17 sn))
       (midi->type (notee 123 hn)))

 
;Implimentation of the calls (music)
(seq
   (par
       (note 12 wn)
       (note 43 hn)
       (note 56 en)
       (note 17 sn)
       (note 123 hn)))

   (par
       (note 12 wn)
       (note 43 hn)
       (note 56 en)
       (note 17 sn)
       (note 123 hn))


"Our Songs"

; We all made songs that were an acumulated 4 whole notes to impliment with our master function. This is our project. 
; We were very happy to be able to transform music that we composed into images. 


"Alex's Song"
;Alex's Image Song
(define Alex's-Song-Images
       (let*
           ([seq (above-or-beside seq)]
            [par (above-or-beside par)]
            [note (lambda (midi duration) (midi->type (notee midi duration)))]
            [rest (lambda (duration) (note rest duration))])
(seq
   (seq
        (note 57 qn)
        (note 67 qn)
        (note 23 qn)
        (note 98 qn)
    (par
        (note 32 hn)
        (note 76 hn))
    (seq
        (note 120 qn)
        (note 67 qn)
        (note 45 qn)
        (note 104 qn)
    (par
        (note 17 hn)
        (note 45 hn)))))))
Alex's-Song-Images

;Alex's Song
(seq
   (seq
        (note 57 qn)
        (note 67 qn)
        (note 23 qn)
        (note 98 qn)
    (par
        (note 32 hn)
        (note 76 hn))
    (seq
        (note 120 qn)
        (note 67 qn)
        (note 45 qn)
        (note 104 qn)
    (par
        (note 17 hn)
        (note 45 hn)))))

"Lilli's Song"
; Lilli's Song image  
(define Lilli's-Song-images
   (let*
   ([seq (above-or-beside seq)]
    [par (above-or-beside par)]
    [note (lambda (midi duration) (midi->type (notee midi duration)))]
    [rest (lambda (duration) (note rest duration))])
   (seq
       (seq
           (par
               (note 1 en)
               (note 11 en)
               (note 21 en)
               (note 1 en)
               (note 11 en)
               (note 21 en))
           (rest en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en))
       (seq
           (par
               (note 1 en)
               (note 11 en)
               (note 21 en)
               (note 1 en)
               (note 11 en)
               (note 21 en))
           (note 34 en)
           (note 43 en)
           (note 54 en)
           (note 65 en)
           (note 76 en)
           (note 87 en)
           (note 111 en)
           (note 64 en))
       (seq
           (note 56 qn)
           (note 56 qn)
           (note 56 qn)
           (note 56 qn))
       (seq
           (note 67 en)
           (note 59 en)
           (note 78 en)
           (note 44 en)
           (note 32 en)
           (rest en)
           (rest qn)))))
Lilli's-Song-images

; Lilli's Song 
 (seq
       (seq
           (par
               (note 1 en)
               (note 11 en)
               (note 21 en)
               (note 1 en)
               (note 11 en)
               (note 21 en))
           (rest en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en)
           (note 78 en))
       (seq
           (par
               (note 1 en)
               (note 11 en)
               (note 21 en)
               (note 1 en)
               (note 11 en)
               (note 21 en))
           (note 34 en)
           (note 43 en)
           (note 54 en)
           (note 65 en)
           (note 76 en)
           (note 87 en)
           (note 111 en)
           (note 64 en))
       (seq
           (note 56 qn)
           (note 56 qn)
           (note 56 qn)
           (note 56 qn))
       (seq
           (note 67 en)
           (note 59 en)
           (note 78 en)
           (note 44 en)
           (note 32 en)
           (rest en)
           (rest qn)))

"Salif's Song"
; Salif's Song

(define salif-song

(seq
(seq (note 30 qn)
     (note 45 qn)
     
(par (note 50 qn)
     (note 25 qn)
     (note 55 qn))
     
(seq 
  (note 100 hn)
  (note 33 hn)
  (note 67 qn)
  
(par (note 20 hn)
     (note 50 hn))))))

salif-song

;Salif Song image
(define salif-song->images
   (let*
       ([seq (above-or-beside seq)]
        [par (above-or-beside par)]
        [note (lambda (midi duration) (midi->type (notee midi duration)))]
        [rest (lambda (duration) (note rest duration))])
       (seq
           (seq
               (note 30 qn)
               (note 45 qn)
               (par
                   (note 50 qn)
                   (note 25 qn)
                   (note 55 qn))
               (seq
                   (note 100 hn)
                   (note 33 hn)
                   (note 67 qn)
                   (par
                       (note 20 hn)
                       (note 50 hn)))))))
salif-song->images

"Martiza's Song"
; Maritza's Song Image
(define maritzas-song-image
   (let*
   ([seq (above-or-beside seq)]
    [par (above-or-beside par)]
    [note (lambda (midi duration) (midi->type (notee midi duration)))]
    [rest (lambda (duration) (note rest duration))])
(seq
    (par
        (par (note 125 qn)
             (rest en)
             (note 100 qn)
             (note 75 qn)
             (rest en))
        (seq (note 125 qn)
             (note 100 qn)
             (note 85 hn)))
    (seq
        (note 125 en)
        (note 120 en)
        (note 110 en)
        (note 100 en)
        (note 100 en)
        (note 100 qn)
        (rest en)
        (note 100 en)
        (note 100 en)
        (note 100 en)
        (note 110 en)
        (note 120 en)
        (note 125 en)
        (note 125 qn)))
   ))

maritzas-song-image

; Maritza's song
(seq
    (par
        (par (note 125 qn)
             (rest en)
             (note 100 qn)
             (note 75 qn)
             (rest en))
        (seq (note 125 qn)
             (note 100 qn)
             (note 85 hn)))
    (seq
        (note 125 en)
        (note 120 en)
        (note 110 en)
        (note 100 en)
        (note 100 en)
        (note 100 qn)
        (rest en)
        (note 100 en)
        (note 100 en)
        (note 100 en)
        (note 110 en)
        (note 120 en)
        (note 125 en)
        (note 125 qn)))
   








