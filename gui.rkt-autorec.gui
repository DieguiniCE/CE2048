#lang racket/gui
(require "logic.rkt")


; --- constantes y estado global ---

(define TILE-PADDING  10)
(define BOARD-PADDING 15)

; estas variables guardan el estado actual del juego
(define currentRows      4)
(define currentCols      4)
(define currentTileSize  100)
(define currentBoard     (void))
(define currentScore     0)
(define currentGameOver  #f)
(define currentGameWon   #f)


; --- calculo de dimensiones ---

; calcula el tamano de los cuadritos para que quepan en la ventana
(define (computeTileSize rows cols)
  (define fromW
    (quotient (- 900 (* 2 BOARD-PADDING) (* (+ cols 1) TILE-PADDING)) cols))
  (define fromH
    (quotient (- 700 (* 2 BOARD-PADDING) (* (+ rows 1) TILE-PADDING)) rows))
  (max 36 (min 100 fromW fromH)))

; ancho total del canvas
(define (canvasWidth)
  (+ (* 2 BOARD-PADDING)
     (* currentCols (+ currentTileSize TILE-PADDING))
     TILE-PADDING))

; alto total del canvas
(define (canvasHeight)
  (+ (* 2 BOARD-PADDING)
     (* currentRows (+ currentTileSize TILE-PADDING))
     TILE-PADDING))

; coordenada x del borde izquierdo del cuadro en columna col
(define (tileX col)
  (+ BOARD-PADDING (* (+ col 1) TILE-PADDING) (* col currentTileSize)))

; coordenada y del borde superior del cuadro en fila row
(define (tileY row)
  (+ BOARD-PADDING (* (+ row 1) TILE-PADDING) (* row currentTileSize)))


; --- colores ---

; color de cuadro segun el numero
(define (getTileBgColor value)
  (cond
    [(= value 0)    (make-object color% 204 192 179)] ; vacío — gris
    [(= value 2)    (make-object color% 238 228 218)] ; crema claro
    [(= value 4)    (make-object color% 237 224 200)] ; crema
    [(= value 8)    (make-object color% 242 177 121)] ; naranja claro
    [(= value 16)   (make-object color% 245 149  99)] ; naranja
    [(= value 32)   (make-object color% 246 124  95)] ; salmon
    [(= value 64)   (make-object color% 246  94  59)] ; rojo-naranja
    [(= value 128)  (make-object color% 237 207 114)] ; amarillo dorado
    [(= value 256)  (make-object color% 237 204  97)] ; amarillo
    [(= value 512)  (make-object color% 237 200  80)] ; amarillo intenso
    [(= value 1024) (make-object color% 237 197  63)] ; ámbar
    [(= value 2048) (make-object color% 237 194  46)] ; dorado
    [else           (make-object color%  60  58  50)])) ; > 2048 — casi negro

; Color del texto
(define (getTileFgColor value)
  (if (< value 16)
      (make-object color% 119 110 101) ; gris oscuro
      (make-object color% 249 246 242))) ; blanco crema


; --- funciones de dibujo ---

; calcula el tamano de la fuente por el numero y el cuadro
(define (tileFontSize value)
  (cond
    [(>= value 1000) (max 10 (quotient (* currentTileSize 24) 100))]
    [(>= value 100)  (max 12 (quotient (* currentTileSize 30) 100))]
    [else            (max 14 (quotient (* currentTileSize 38) 100))]))

; dibuja un halo dorado alrededor de baldosas con valor >= 128
#|(define (drawTileGlow dc value x y)
  (when (>= value 128)
    (send dc set-alpha 0.22)
    (send dc set-brush
          (make-object brush% (make-object color% 255 235 100) 'solid))
    (send dc set-pen
          (make-object pen% (make-object color% 255 235 100) 1 'transparent))
    (send dc draw-rounded-rectangle
          (- x 5) (- y 5)
          (+ currentTileSize 10) (+ currentTileSize 10) 12)
    (send dc set-alpha 1.0)))
|#

; creacion de cuadros
(define (drawSingleTile dc value row col)
  (define x (tileX col))
  (define y (tileY row))
  ;(drawTileGlow dc value x y)
  (send dc set-brush (make-object brush% (getTileBgColor value) 'solid))
  (send dc set-pen   (make-object pen%   (getTileBgColor value) 1 'transparent))
  (send dc draw-rounded-rectangle x y currentTileSize currentTileSize 6)
  (when (> value 0)
    (define txt (number->string value))
    (define fsize (tileFontSize value))
    (send dc set-text-foreground (getTileFgColor value))
    (send dc set-font (make-object font% fsize 'default 'normal 'bold #f 'default #t))
    (define-values (tw th _a _d) (send dc get-text-extent txt))
    (send dc draw-text txt
          (+ x (/ (- currentTileSize tw) 2))
          (+ y (/ (- currentTileSize th) 2)))))

; crea los cuadros de una fila de izquierda a derecha
(define (drawRowTiles dc rowData rowIdx colCount colIdx)
  (when (< colIdx colCount)
    (drawSingleTile dc (listRefRec rowData colIdx) rowIdx colIdx)
    (drawRowTiles dc rowData rowIdx colCount (+ colIdx 1))))

; crea todas las filas del tablero de arriba hacia abajo
(define (drawAllRows dc board rowCount colCount rowIdx)
  (when (< rowIdx rowCount)
    (drawRowTiles dc (listRefRec board rowIdx) rowIdx colCount 0)
    (drawAllRows dc board rowCount colCount (+ rowIdx 1))))

; coloca el fondo gris del tablero y luego todos los cuadros
(define (drawBoard dc)
  (send dc set-smoothing 'smoothed)
  (send dc set-brush
        (make-object brush% (make-object color% 187 173 160) 'solid))
  (send dc set-pen
        (make-object pen% (make-object color% 187 173 160) 1 'transparent))
  (send dc draw-rounded-rectangle 0 0 (canvasWidth) (canvasHeight) 8)
  (drawAllRows dc currentBoard currentRows currentCols 0))

; overlay de victoria o derrota
(define (drawOverlay dc mainMsg)
  (send dc set-alpha 0.78)
  (send dc set-brush
        (make-object brush% (make-object color% 238 228 218) 'solid))
  (send dc set-pen
        (make-object pen% (make-object color% 238 228 218) 1 'transparent))
  (send dc draw-rectangle 0 0 (canvasWidth) (canvasHeight))
  (send dc set-alpha 1.0)
  (define fsize (max 24 (min 52 (quotient (canvasWidth) 7))))
  (send dc set-font (make-object font% fsize 'default 'normal 'bold #f 'default #t))
  (send dc set-text-foreground (make-object color% 119 110 101))
  (define-values (tw th _a _d) (send dc get-text-extent mainMsg))
  (send dc draw-text mainMsg
        (/ (- (canvasWidth) tw) 2)
        (- (/ (canvasHeight) 2) th))
  (define subMsg "Presiona N para nueva partida")
  (define subSize (max 12 (quotient fsize 2)))
  (send dc set-font (make-object font% subSize 'default 'normal 'normal))
  (define-values (sw sh _sa _sd) (send dc get-text-extent subMsg))
  (send dc draw-text subMsg
        (/ (- (canvasWidth) sw) 2)
        (+ (/ (canvasHeight) 2) 6)))


; --- union de la parte logica con la parte grafica ---

; aplica el resultado de un movimiento al estado del juego
; solo se actualiza si el tablero cambia
(define (executeMove! result)
  (define newBoard (moveBoard result))
  (define gained   (moveScore result))
  (when (not (boardsEqual? currentBoard newBoard))
    (set! currentScore (+ currentScore gained))
    (set! currentBoard (addRandomTwo newBoard))
    (when (checkWin currentBoard)
      (set! currentGameWon #t))
    (when (and (not currentGameWon) (checkLose currentBoard))
      (set! currentGameOver #t))))

; llama a la funcion correspondiente dependiendo de la tecla que se toco
(define (processMove! direction)
  (when (not (or currentGameOver currentGameWon))
    (executeMove!
     (cond
       [(equal? direction 'left)  (moveLeftScored  currentBoard)]
       [(equal? direction 'right) (moveRightScored currentBoard)]
       [(equal? direction 'up)    (moveUpScored    currentBoard)]
       [(equal? direction 'down)  (moveDownScored  currentBoard)]))))

; reinicia todas las variables y redimensiona la ventana
(define (startGame! rows cols)
  (set! currentRows     rows)
  (set! currentCols     cols)
  (set! currentTileSize (computeTileSize rows cols))
  (set! currentBoard    (initBoard rows cols))
  (set! currentScore    0)
  (set! currentGameOver #f)
  (set! currentGameWon  #f)
  (send theCanvas min-client-width  (canvasWidth))
  (send theCanvas min-client-height (canvasHeight))
  (send mainFrame reflow-container)
  (send scoreMsg set-label "Puntuación: 0")
  (send theCanvas refresh-now)
  (send theCanvas focus))


; --- canvas del juego ---

; clase que extiende canvas% para manejar el teclado y el dibujo
(define gameCanvas%
  (class canvas%
    (inherit set-canvas-background)
    (super-new)
    (set-canvas-background (make-object color% 250 248 239))

    ; detecta las flechas y la tecla N, envia a la funcion la tecla que se toco
    (define/override (on-char event)
      (define key (send event get-key-code))
      (cond
        [(equal? key 'left)                  (processMove! 'left)]
        [(equal? key 'right)                 (processMove! 'right)]
        [(equal? key 'up)                    (processMove! 'up)]
        [(equal? key 'down)                  (processMove! 'down)]
        [(or (equal? key #\n) (equal? key #\N))
         (showNewGameDialog!)])
      (send scoreMsg set-label
            (string-append "Puntuación: " (number->string currentScore)))
      (send this refresh-now))

    ; Vuelve a crear el tablero y muestra overlay si el juego termina
    (define/override (on-paint)
      (define dc (send this get-dc))
      (drawBoard dc)
      (when currentGameWon
        (drawOverlay dc "You win!"))
      (when currentGameOver
        (drawOverlay dc "Game Over :c")))))


; --- construccion de la ventana ---

(define mainFrame
  (new frame%
       [label "2048 — Paradigmas de Programación"]))

; panel superior con el titulo y la puntuacion
(define topPanel
  (new horizontal-panel%
       [parent mainFrame]
       [alignment '(center center)]
       [stretchable-height #f]
       [border 5]))

(new message%
     [parent topPanel]
     [label "2048"]
     [font (make-object font% 34 'default 'normal 'bold)])

; spacer para mover la puntuacion hacia la derecha
(define spacer
  (new panel%
       [parent topPanel]
       [stretchable-width #t]))

(define scoreMsg
  (new message%
       [parent topPanel]
       [label "Puntuación: 0"]
       [font (make-object font% 18 'default 'normal 'normal)]
       [min-width 220]
       [auto-resize #t]))

; canvas donde se dibuja todo el juego
(define theCanvas
  (new gameCanvas%
       [parent mainFrame]))

; panel inferior con boton y los controles
(define bottomPanel
  (new horizontal-panel%
       [parent mainFrame]
       [alignment '(center center)]
       [stretchable-height #f]
       [border 5]))

(new button%
     [parent bottomPanel]
     [label "Nueva Partida  (N)"]
     [callback (lambda (b e) (showNewGameDialog!))])

(new message%
     [parent bottomPanel]
     [label "     ← ↑ ↓ → para mover los cuadros     "]
     [font (make-object font% 11 'default 'normal 'normal)])


; --- dialogo de nueva partida ---

; muestra un dialogo modal para configurar el tamano del tablero
(define (showNewGameDialog!)
  (define dlg
    (new dialog%
         [parent mainFrame]
         [label "Nueva Partida — 2048"]
         [width 300]))

  (new message%
       [parent dlg]
       [label "Configuracion del tamaño del tablero:"]
       [font (make-object font% 12 'default 'normal 'bold)])

  (define rowsField
    (new text-field%
         [parent dlg]
         [label "Filas   (M) : "]
         [init-value (number->string currentRows)]))

  (define colsField
    (new text-field%
         [parent dlg]
         [label "Columnas (N) : "]
         [init-value (number->string currentCols)]))

  #|(new message%
       [parent dlg]
       [label "  Ejemplos: 4x4 (clásico),  8x10,  10x12"]
       [font (make-object font% 10 'default 'italic 'normal)])
  |#

  (define btnPanel
    (new horizontal-panel%
         [parent dlg]
         [alignment '(center center)]
         [border 6]))

  ; valida los valores ingresados e inicia el juego si son correctos
  (new button%
       [parent btnPanel]
       [label "Iniciar"]
       [callback
        (lambda (b e)
          (define m (string->number (send rowsField get-value)))
          (define n (string->number (send colsField get-value)))
          (cond
            [(not (and m n))
             (message-box "Error de entrada"
                          "Por favor ingrese un número válido."
                          dlg '(ok caution))]
            [(or (< m 2) (< n 2))
             (message-box "Error de tamaño"
                          "El tablero debe ser de al menos 2x2."
                          dlg '(ok caution))]
            [else
             (send dlg show #f)
             (startGame! (inexact->exact (floor m))
                         (inexact->exact (floor n)))]))])

  (new button%
       [parent btnPanel]
       [label "Cancelar"]
       [callback (lambda (b e) (send dlg show #f))])

  (send dlg show #t))


; --- iniciar con datos predeterminados ---

; tablero 4x4 y muestra la ventana
(startGame! 4 4)
(send mainFrame show #t)
