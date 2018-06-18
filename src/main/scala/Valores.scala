abstract class Valores (dominio : Dominio) {
  /**
    * Método para obtener el número de variables del objeto Valores
    * @return Número de variables del dominio asociado
    */
  def obtenerVariables : List[Variable] = dominio.variables
}

class ValoresArray (dominio : Dominio, datos : List[Double]) extends Valores (dominio) {

}

object ValoresArray {
  /**
    * Método para crear un objeto ValoresArray sin utilizar la palabra reservada new
    * @param dominio Dominio sobre el que construir el objeto ValoresArray
    * @param datos Conjunto de datos asociados a los distintos valores
    * @return Nuevo objeto ValoresArray
    */
  def apply (dominio : Dominio, datos : List[Double]) : ValoresArray = {
    new ValoresArray(dominio, datos)
  }
}
