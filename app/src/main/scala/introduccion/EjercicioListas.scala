package introduccion

import javax.naming.OperationNotSupportedException

class EjercicioListas {
  /*
  * Punto 2 repetir lista
  * @param lista Lista a repetir
  * @param n Número de veces a repetir la lista
  * @return Lista repetida n veces
  * @example repetirLista(List(1, 2, 3), 2) = List(List(1,1),List(2,2),List(3,3))
  * @throws IllegalArgumentException si n es negativo
  */
  def repetirListas(lista: List[Int], n: Int): List[List[Int]] = {
    var listaRepetida : List[List[Int]] = List()
    if (n < 0) {
      throw new IllegalArgumentException("n no puede ser negativo")
    }else{
      for (i <- 1 to lista.size) {
        var listaAux: List[Int] = List()
        for (j <- 1 to n) {
          listaAux = listaAux :+ lista(i - 1)
        }
        listaRepetida = listaRepetida :+ listaAux

      }
    }
    listaRepetida
  }
  /*
  * Punto 3: Filtrar listas
  * @param criterioIn Criterio de filtrado que puede ser "mayor", "menor", "mayoroigual", "igual", "diferente" o "menoroigual"
  * @param n Número a comparar
  * @param lista Lista a filtrar
  * @return Lista filtrada de acuerdo al criterio y n
  * @throws IllegalArgumentException si el criterio no es uno de los valores válidos
  */

  def filtrarListas(criterioIn: String, n: Int, lista: List[Int]) : List[Int] = {
    var criterio : String = criterioIn.toLowerCase()
    var listaFiltrada : List[Int] = List()
    if (criterio == "mayor"){
      for (i <- 1 to lista.size){
        if (lista(i-1) > n){
          listaFiltrada = listaFiltrada :+ lista(i-1)
        }
      }
    }else if (criterio == "menor"){
      for (i <- 1 to lista.size){
        if (lista(i-1) < n){
          listaFiltrada = listaFiltrada :+ lista(i-1)
        }
      }
    }else if (criterio == "mayoroigual"){
      for (i <- 1 to lista.size){
        if (lista(i-1) >= n){
          listaFiltrada = listaFiltrada :+ lista(i-1)
        }
      }
    }else if (criterio == "igual"){
      for (i <- 1 to lista.size){
        if (lista(i-1) == n){
          listaFiltrada = listaFiltrada :+ lista(i-1)
        }
      }
    }else if (criterio == "diferente"){
      for (i <- 1 to lista.size){
        if (lista(i-1) != n){
          listaFiltrada = listaFiltrada :+ lista(i-1)
        }
      }
    }else if (criterio == "menoroigual"){
      for (i <- 1 to lista.size) {
        if (lista(i - 1) <= n) {
          listaFiltrada = listaFiltrada :+ lista(i - 1)
        }
      }
    }else{
      throw new IllegalArgumentException("El criterio no es válido")
    }
    listaFiltrada
  }
}
