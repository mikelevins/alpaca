#lang racket/gui

(define (new-editor)
  (let* ((frame (new frame% [label "Simple Edit"]
                      [width 200]
                      [height 200]))
         (canvas (new editor-canvas% [parent frame]))
         (text (new text%)))
    (send canvas set-editor text)
    (send frame show #t)
    frame))

;;; (define $ed (new-editor))
