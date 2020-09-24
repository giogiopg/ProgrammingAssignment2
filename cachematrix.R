## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  ##Funcion que recibe una matrix, la reasigna a x y reasigna NULL a la inversa
  set <- function(y){   
    x <<- y      
    inversa <<- NULL
  }
   ## Devuelve el valor de la matriz que se tenga
  get <- function() {x}
  ## La siguiente función me coge el valor de la inversa que calcule y me 
  ## lo asigna a "inversa" (que hasta ahora tenía valor nulo).
  set_inv <- function(calculated_inv){inversa <<- calculated_inv}
  ## La siguiente tan solo e devuelve el valor de la inversa
  get_inv <- function(){inversa}
  ##Creo lista cn el output igualando valores a sus funciones
  list(set = set, get = get,
       set_inv = set_inv, get_inv = get_inv)
}


## Esta funcion me calcula la función inversa de la matriz x y, si estaba
## calculada, me retorna el valor que ya estaba calculado.

cacheSolve <- function(x, ...) {
  inversa <- x$get_inv() ## Aquirimos el valor de la inversa
  ## Habiendo cogido el dato ahora compruebo si es NULL o ya está calculado
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get() ##Me coge el valor de la matriz que he metido.
  inversa <- solve(data, ...) ##Funcion de R que me calcula el valor de la inversa.
  x$set_inv(inversa)
  inversa
  ## Return a matrix that is the inverse of 'x'
}
