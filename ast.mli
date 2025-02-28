type re =
| Vazio
| Caracter of char
| Concatenacao of re * re
| Escolha of re * re
| Estrela of re
