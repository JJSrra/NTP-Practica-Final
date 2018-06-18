abstract class Valores (val dominio : Dominio) {
  /**
    * Método para obtener el número de variables del objeto Valores
    * @return Número de variables del dominio asociado
    */
  def obtenerVariables : List[Variable] = dominio.variables

  /**
    * Método abstracto para devolver lista de valores
    * @return Lista de valores
    */
  def obtenerValores : List[Double]

  /**
    * Método abstracto para calcular un valor en base a una asignación
    * @param asignacion Asignación de la que obtener el valor
    * @return Valor de dicha asignación
    */
  def obtenerValor (asignacion : Asignacion)
}

class ValoresArray (override val dominio : Dominio, val valores : List[Double]) extends Valores (dominio) {
  /**
    * Método para devolver los valores del objeto ValoresArray
    * @return Lista de valores
    */
  override def obtenerValores : List[Double] = valores

  /**
    * Método para calcular un valor en base a una asignación
    * @param asignacion Asignación de la que obtener el valor
    * @return Valor de dicha asignación
    */
  override def obtenerValor (asignacion : Asignacion) = {
    if (asignacion.dominio.variables != dominio.variables) null
    else valores(asignacion.calcularIndice)
  }

  override def toString = (0 until dominio.maximoIndice).map(indice => Asignacion(dominio,indice).toString
    + " = " + valores(indice)) mkString "\n"
}

object ValoresArray {
  /**
    * Método para crear un objeto ValoresArray sin utilizar la palabra reservada new
    * @param dominio Dominio sobre el que construir el objeto ValoresArray
    * @param valores Conjunto de datos asociados a los distintos valores
    * @return Nuevo objeto ValoresArray
    */
  def apply (dominio : Dominio, valores : List[Double]) : ValoresArray = {
    new ValoresArray(dominio, valores)
  }
}
