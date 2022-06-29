## WORDLE

Este proyecto implementa el juego Wordle en el lenguaje de programación Haskell.
Cuenta con dos modos de juego: Modo mente maestra y modo descifrador.

### Modo mente maestra 
En este modo el jugador debe adivinar una palabra seleccionada por la computadora. El usuario debe introducir una palabra de 5 letras en mayúsculas y la computadora verificará cada una de las letras de dicha palabra. Si están en la posición correcta, son toros, si no están en la posición correcta pero forman parte de la palabra son vacas y si no están en la palabra son -.
El jugador tiene 6 oportunidades para adivinar la palabra correcta, sino se termina el juego.

### Modo descifrador
En este modo el jugador debe pensar una palabra de 5 letras y la computadora adivinará dicha palabra. La computadora dará opciones de palabras y es el jugador quien debe indicar cuales son toros, vacas y -. En caso de no adivinar la palabra, el jugador gana, pero si la computadora adivina la palabra o el jugador hace trampa, el jugador pierde.

### ¿Cómo jugar Wordle?
El juego puede iniciarse con una llamada en consola, introduciendo el modo de juego como un argumento, es decir:

    wordle mentemaestra
    wordle descifrador
