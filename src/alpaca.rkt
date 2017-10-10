#lang racket/gui

(define (new-editor)
  (let* ((frame (new frame% [label "Simple Edit"]
                      [width 200]
                      [height 200]))
         (canvas (new editor-canvas% [parent frame]))
         (text (new text% [auto-wrap #t]))
         (menubar (new menu-bar% [parent frame]))
         (edit-menu (new menu% [label "Edit"] [parent menubar]))
         (font-menu (new menu% [label "Font"] [parent menubar])))
    (append-editor-operation-menu-items edit-menu #f)
    (append-editor-font-menu-items font-menu)
    (send canvas set-editor text)
    (send frame show #t)
    (send text set-max-undo-history 100)
    frame))

;;; (define $ed (new-editor))
