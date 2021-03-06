#lang racket/gui
(require racket/draw)
;(require "utilities.scm")


;*******************************************************************************************************************************
 ; Specify in an interface the methods that a containee needs to make it
; self-placeable.;
 (define self-placeable-containee<%>
   (interface ()
     get-preferred-x get-preferred-y get-preferred-x-y
     set-preferred-x set-preferred-y set-preferred-x-y))
 
 ; Define in a mixin the default implementations for a self-placeable
 ;containee.
  (define (self-placeable-containee%% %)
   (class* % (self-placeable-containee<%>)
     (super-new)
 
     (define preferred-x 0)
     (define preferred-y 0)

     (define/public (get-preferred-x)
       preferred-x)
      (define/public (get-preferred-y)
      preferred-y)
     (define/public (get-preferred-x-y)
      (values preferred-x preferred-y))
 
     (define/public (set-preferred-x x)
       (set! preferred-x x))
     (define/public (set-preferred-y y)
       (set! preferred-y y))
     (define/public (set-preferred-x-y x y)
      (set! preferred-x x)
       (set! preferred-y y))
     ))
 
 ; Define a custom container panel that can place the self-placeable
  ;containees.
 (define placer-panel%
   (class panel%
    (inherit get-children min-width min-height)
 
     (super-new)
 
     ; container-size is called with each child's dimensions and
; strechability,
     ; and returns the container's required minimum dimensions.
     ; In this case, the containee size's are ignored,
     ; and the container's (previously set) minimum width and height are
 ;returned.
     [define/override (container-size containee-sizes)
 ;      (printf "container-size:~a\n" containee-sizes)
       (values (min-width) (min-height))]
 
     ; place-children is called with each child's dimensions and
; strechability,
     ; and returns each child's location and dimensions.
     ; In this case, each containee is simply asked where it wants to be
 ;(previously set).
     ; No adustments are made to a containee's size so as to use any empty
 ;space in the container.
     ; i.e. A containee's strechability is ignored, and each containee
 ;remains the (previously set) minimum width and height.
     [define/override (place-children containee-sizes container-width
 container-height)
 ;      (printf "place-children:~a\n" containee-sizes)
      (map (lambda (containee-size containee)
              (let*-values ([(min-width min-height h-stretch? v-stretch?)
                             (apply values containee-size)]
                            [(x y)
                             (send containee get-preferred-x-y)]
                            )
               (list x y min-width min-height)))
            containee-sizes
            (get-children))]
    ))
 
 (define self-placeable-canvas%
   (class (self-placeable-containee%% canvas%)
     (inherit set-preferred-x-y)
     (init x y)
    (super-new)
     (set-preferred-x-y x y)
     ))
 (define self-placeable-button%
   (class (self-placeable-containee%% button%)
     (inherit set-preferred-x-y)
     (init x y)
    (super-new)
     (set-preferred-x-y x y)
     ))
 (define mybutton%
   (class self-placeable-canvas%
     (init-field [call (λ() (void))])
     (define/override (on-event e)
       (if (equal? (send e get-event-type) 'left-down) (call) (void)))
     (super-new)))

;*******************************************************************************************************************************
 
 (provide (all-defined-out))