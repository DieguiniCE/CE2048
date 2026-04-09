#lang racket
(provide (all-defined-out))

; el tablero es una lista de listas, 0 = celda vacia
; los movimientos retornan (cons nuevo-tablero puntos-ganados)


; --- utilidades de lista ---

; devuelve el elemento en la posicion index
(define (listRefRec lst index)
  (if (= index 0)
      (car lst)
      (listRefRec (cdr lst) (- index 1))))

; devuelve una nueva lista con value en la posicion index
; busca value y lo modifica en una lista que se va creando al mismo tiempo
(define (listSet lst index value)
  (if (= index 0)
      (cons value (cdr lst))
      (cons (car lst) (listSet (cdr lst) (- index 1) value))))

; cuenta cuantos elementos tiene la lista
(define (listLength lst)
  (if (null? lst)
      0
      (+ 1 (listLength (cdr lst)))))

; une dos listas en una sola
(define (listAppendTwo lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (listAppendTwo (cdr lst1) lst2))))

; invierte el orden de la lista
(define (listReverse lst)
  (listReverseAcc lst '()))

; auxiliar de listReverse, usa acumulador para no apilar llamadas
(define (listReverseAcc lst acc)
  (if (null? lst)
      acc
      (listReverseAcc (cdr lst) (cons (car lst) acc))))


; --- utilidades del tablero ---

; retorna cuantas filas tiene el tablero
(define (boardRows board)
  (listLength board))

; retorna cuantas columnas tiene el tablero
(define (boardCols board)
  (listLength (car board)))

; retorna el valor en la celda (row, col)
(define (getCell board row col)
  (listRefRec (listRefRec board row) col))

; retorna un tablero nuevo con value colocado en (row, col)
(define (setCell board row col value)
  (listSet board row
           (listSet (listRefRec board row) col value)))


; --- generacion del tablero ---

; crea una fila de n ceros
(define (generateEmptyRow n)
  (if (= n 0)
      '()
      (cons 0 (generateEmptyRow (- n 1)))))

; crea un tablero m x n lleno de ceros
(define (generateEmptyBoard m n)
  (if (= m 0)
      '()
      (cons (generateEmptyRow n)
            (generateEmptyBoard (- m 1) n))))

; recorre todas las filas buscando celdas vacias
(define (findEmptyRows board rowIdx)
  (if (null? board)
      '()
      (listAppendTwo
       (findEmptyInRow (car board) rowIdx 0)
       (findEmptyRows (cdr board) (+ rowIdx 1)))))

; busca celdas vacias dentro de una sola fila
(define (findEmptyInRow rowData rowIdx colIdx)
  (if (null? rowData)
      '()
      (if (= (car rowData) 0)
          (cons (list rowIdx colIdx)
                (findEmptyInRow (cdr rowData) rowIdx (+ colIdx 1)))
          (findEmptyInRow (cdr rowData) rowIdx (+ colIdx 1)))))

; retorna la lista de coordenadas de todas las celdas vacias
(define (findEmptyCells board)
  (findEmptyRows board 0))

; coloca un 2 en la celda indicada por cells[idx]
(define (placeTile board cells idx)
  (setCell board
           (car  (listRefRec cells idx))
           (cadr (listRefRec cells idx))
           2))

; escoge una celda vacia al azar y coloca un 2 ahi
(define (addTwoHelper board emptyCells)
  (if (null? emptyCells)
      board
      (placeTile board emptyCells
                 (random (listLength emptyCells)))))

; agrega un 2 en una posicion aleatoria del tablero
(define (addRandomTwo board)
  (addTwoHelper board (findEmptyCells board)))

; crea el tablero inicial con dos 2 en posiciones aleatorias
(define (initBoard m n)
  (addRandomTwo (addRandomTwo (generateEmptyBoard m n))))


; --- movimientos ---
; deslizar izquierda: comprimir, combinar, rellenar ---
; elimina los ceros de la fila para juntar los cuadros
(define (compressRow row)
  (if (null? row)
      '()
      (if (= (car row) 0)
          (compressRow (cdr row))
          (cons (car row) (compressRow (cdr row))))))

; combina elementos iguales adyacentes, acumula el puntaje
(define (mergeAcc lst acc score)
  (if (null? lst)
      (cons (listReverse acc) score)
      (if (null? (cdr lst))
          (cons (listReverse (cons (car lst) acc)) score)
          (if (= (car lst) (cadr lst))
              (mergeAcc (cddr lst)
                        (cons (* 2 (car lst)) acc)
                        (+ score (* 2 (car lst))))
              (mergeAcc (cdr lst)
                        (cons (car lst) acc)
                        score)))))

; punto de entrada para combinar una fila
(define (mergeWithScore lst)
  (mergeAcc lst '() 0))

; rellena con ceros a la derecha hasta llegar al largo n
(define (padRight lst n)
  (if (>= (listLength lst) n)
      lst
      (padRight (listAppendTwo lst '(0)) n)))

; aplica padRight al tablero dentro del par resultado
(define (attachPadded pair n)
  (cons (padRight (car pair) n) (cdr pair)))

; aplica las tres funciones para deslizar una fila a la izquierda
(define (slideRowLeftScored row)
  (attachPadded
   (mergeWithScore (compressRow row))
   (listLength row)))

; invierte la lista dentro del par sin tocar el puntaje
(define (reversePairList pair)
  (cons (listReverse (car pair)) (cdr pair)))

; desliza una fila a la derecha usando el truco de invertir
(define (slideRowRightScored row)
  (reversePairList
   (slideRowLeftScored (listReverse row))))


; --- movimiento del tablero completo ---

; procesa una fila y sigue con el resto del tablero
(define (processRowLeft rest acc score rowResult)
  (moveRowsLeft rest
                (cons (car rowResult) acc)
                (+ score (cdr rowResult))))

; aplica slideRowLeftScored a cada fila del tablero
(define (moveRowsLeft board acc score)
  (if (null? board)
      (cons (listReverse acc) score)
      (processRowLeft (cdr board) acc score
                      (slideRowLeftScored (car board)))))

; mueve todas los cuadros hacia la izquierda
(define (moveLeftScored board)
  (moveRowsLeft board '() 0))

; invierte las columnas de cada fila
(define (flipBoardCols board)
  (if (null? board)
      '()
      (cons (listReverse (car board))
            (flipBoardCols (cdr board)))))

; aplica flipBoardCols al tablero dentro del par
(define (flipPairCols pair)
  (cons (flipBoardCols (car pair)) (cdr pair)))

; mueve a la derecha: espejo, mover izquierda, espejo ---
(define (moveRightScored board)
  (flipPairCols (moveLeftScored (flipBoardCols board))))

; extrae la primera columna del tablero como lista
(define (getFirstCol board)
  (if (null? board)
      '()
      (cons (caar board) (getFirstCol (cdr board)))))

; retorna el tablero sin su primera columna
(define (removeFirstCol board)
  (if (null? board)
      '()
      (cons (cdar board) (removeFirstCol (cdr board)))))

; transpone el tablero, convierte filas en columnas y viceversa ---
(define (transpose board)
  (if (or (null? board) (null? (car board)))
      '()
      (cons (getFirstCol board)
            (transpose (removeFirstCol board)))))

; aplica transpose al tablero dentro del par
(define (transposePair pair)
  (cons (transpose (car pair)) (cdr pair)))

; mueve hacia arriba: transponer, mover izquierda, transponer
(define (moveUpScored board)
  (transposePair (moveLeftScored (transpose board))))

; mueve hacia abajo: transponer, mover derecha, transponer ---
(define (moveDownScored board)
  (transposePair (moveRightScored (transpose board))))

; extrae el tablero del resultado de un movimiento
(define (moveBoard result) (car result))

; extrae los puntos ganados del resultado de un movimiento
(define (moveScore result) (cdr result))


; --- condiciones de victoria y derrota ---

; revisa si alguna celda de la fila vale 2048
(define (rowHas2048 row)
  (if (null? row)
      #f
      (if (= (car row) 2048)
          #t
          (rowHas2048 (cdr row)))))

; retorna #t si algun valor del tablero es 2048
(define (checkWin board)
  (if (null? board)
      #f
      (if (rowHas2048 (car board))
          #t
          (checkWin (cdr board)))))

; compara dos filas elemento a elemento
(define (rowsEqual? r1 r2)
  (if (null? r1)
      #t
      (if (= (car r1) (car r2))
          (rowsEqual? (cdr r1) (cdr r2))
          #f)))

; compara dos tableros fila a fila
(define (boardsEqual? b1 b2)
  (if (null? b1)
      #t
      (if (rowsEqual? (car b1) (car b2))
          (boardsEqual? (cdr b1) (cdr b2))
          #f)))

; retorna #t si ninguna direccion produce un cambio en el tablero
(define (noValidMoves board)
  (if (not (boardsEqual? board (moveBoard (moveLeftScored  board)))) #f
      (if (not (boardsEqual? board (moveBoard (moveRightScored board)))) #f
          (if (not (boardsEqual? board (moveBoard (moveUpScored    board)))) #f
              (boardsEqual? board (moveBoard (moveDownScored  board)))))))

; retorna #t si no hay celdas vacias y tampoco movimientos validos
(define (checkLose board)
  (if (not (null? (findEmptyCells board)))
      #f
      (noValidMoves board)))

; suma todos los valores de una fila
(define (rowSum row)
  (if (null? row)
      0
      (+ (car row) (rowSum (cdr row)))))

; suma el valor de todas las celdas del tablero
(define (boardSum board)
  (if (null? board)
      0
      (+ (rowSum (car board)) (boardSum (cdr board)))))
