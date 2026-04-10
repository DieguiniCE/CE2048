# CE2048 🎮

![Platform](https://img.shields.io/badge/Platform-DrRacket-blue)
![Language](https://img.shields.io/badge/Language-Racket-red)
![Paradigm](https://img.shields.io/badge/Paradigm-Functional-orange)

CE2048 es un clon interactivo del popular videojuego 2048, desarrollado completamente en el lenguaje Racket utilizando el paradigma de programación funcional. La aplicación gestiona la lógica del juego y el renderizado gráfico mediante el uso estricto de recursividad, operaciones sobre listas puras y un motor gráfico nativo.

---

## 🚀 Funcionalidades

### 1. Tableros Dinámicos (MxN)
El sistema soporta la creación de tableros de dimensiones personalizables (por ejemplo, 4x4, 8x10, etc.). 
* **Generación aleatoria:** Al iniciar una partida, el sistema inserta automáticamente dos baldosas con el valor "2" en posiciones completamente aleatorias.
* **Configuración visual:** Cuenta con un menú interactivo que permite al usuario definir la cantidad de filas y columnas (M y N) antes de iniciar una nueva partida.

### 2. Mecánicas y Operaciones de Juego
* 🕹️ **Movimientos:** Desplazamiento de baldosas hacia la izquierda, derecha, arriba y abajo utilizando las flechas del teclado.
* ➕ **Fusión y Puntaje:** Cuando dos baldosas del mismo valor colisionan, se combinan sumando su valor, lo que a su vez incrementa la puntuación total en tiempo real.
* 🔄 **Reinicio rápido:** Opción para abandonar la partida actual y comenzar una nueva presionando la tecla "N".

### 3. Detección de Estados y Gráficos
El juego evalúa constantemente el estado del tablero para determinar resultados y mostrarlos en la Interfaz Gráfica (GUI):
* **Victoria:** Detecta automáticamente si el jugador alcanza una baldosa con el número 2048, mostrando una pantalla superpuesta (overlay) de "You win!".
* **Derrota:** Verifica si no existen celdas vacías ni movimientos válidos posibles en ninguna dirección, emitiendo un estado de "Game Over :c".
* **Feedback Visual:** Implementa una paleta de colores dinámicos que cambia dependiendo del valor de la baldosa (tonos crema, naranjas, rojos y dorados).

---

## 🛠️ Detalles Técnicos
* **Lenguaje:** Racket (`#lang racket` y `#lang racket/gui`).
* **Arquitectura Modular:** Separación estricta de responsabilidades en dos archivos principales: `logic.rkt` para el motor algorítmico y `gui.rkt` para el manejo de ventanas y eventos.
* **Paradigma Funcional:** Implementación estricta sin el uso de estado mutable, bucles iterativos convencionales ni funciones de biblioteca restringidas como `let`, `map` o `apply`. Todo el control de flujo se maneja a través de recursividad pura e iteración por llamadas de cola.
* **Estructuras de Datos:** El tablero se representa internamente como una estructura de matriz anidada (una lista de listas).
* **Procesamiento de Datos:** Manejo de la física del tablero mediante operaciones matriciales creadas desde cero, como transposición e inversión de listas para reciclar la lógica de compresión y fusión matemática en distintas direcciones.
