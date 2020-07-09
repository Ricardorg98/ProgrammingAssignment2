## vamos tornar verdadeira a função makeCacheMatrix e cacheSolve
> makeCacheMatrix <- função (x = matriz ()) {
+ inv <- NULL
+ definir <- função (y) {
+ x << - y
+ inv << - NULL
+}
+ get <- function () {x}
+ setInverse <- função (inversa) {inv << - inversa}
+ getInverse <- function () {inv}
+ lista (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
+}
> cacheSolve <- função (x, ...) {
+ inv <- x $ getInverse ()
+ if (! is.null (inv)) {
+ mensagem ("obtendo dados em cache")
+ retorno (inv)
+}
+ mat <- x $ get ()
+ inv <- resolver (mat, ...)
+ x $ setInverse (inv)
+ inv
+}
> ## formando uma matriz 4x4 que vai de 1 a 16, usando o seguinte código:
> fonte ("makeCacheMatrix.R")  
> matriz <- makeCacheMatrix (matriz (1: 4, nrow = 2, ncol = 2))
> ## Veja a matriz:
> matriz $ get ()
     [, 1] [, 2]
[1] 1 3
[2] 2 4
> ## E agora Matriz Inversa:
> matriz $ getInverse ()
NULO
> cacheSolve (matriz)
     [, 1] [, 2]
[1,] -2 1,5
[2,] 1 -0,5
