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
  def obtenerValor (asignacion : Asignacion) : Double

  /**
    * Método para combinar dos objetos Valores sin importar qué subclase sean
    * @param nuevoPotencial Nuevo objeto Valores a combinar
    * @return Objeto Valores combinado
    */
  def combinar (nuevoPotencial : Valores) : Valores = {
    this match {
      case p1:ValoresArray => {
        val potencialArray = nuevoPotencial match {
          case p2:ValoresArbol => p2.convertir
          case p2:ValoresArray => p2
        }

        p1.combinarArrayArray(potencialArray)
      }
      case p1:ValoresArbol => {
        val potencialArbol = nuevoPotencial match {
          case p2:ValoresArray => p2.convertir
          case p2:ValoresArbol => p2
        }

        p1.combinarArbolArbol(potencialArbol)
      }

    }
  }

  /**
    * Método abstracto para convertir un objeto Valores a su equivalente de la otra subclase
    * @return ValoresArray si se ejecuta sobre un ValoresArbol, y viceversa
    */
  def convertir : Valores

}

case class ValoresArray (override val dominio : Dominio, valores : List[Double]) extends Valores (dominio) {
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
  override def obtenerValor (asignacion : Asignacion) : Double = {
    valores(asignacion.calcularIndice)
  }

  /**
    * Método toString para mostrar una representación gráfica del objeto ValoresArray
    * @return Cadena representativa del objeto ValoresArray
    */
  override def toString = (0 until dominio.maximoIndice).map(indice => Asignacion(dominio,indice).toString
    + " = " + valores(indice)) mkString "\n"

  /**
    * Método para combinar dos objetos ValoresArray, llamado desde el método combinar
    * @param nuevoArray Nuevo objeto ValoresArray que combinar
    * @return Objeto ValoresArray combinado
    */
  def combinarArrayArray (nuevoArray : ValoresArray) : ValoresArray = {
    val dominioFinal = dominio + nuevoArray.dominio
    val valoresFinal = (0 until dominioFinal.maximoIndice).map(indice => {
      val asignacionFinal = Asignacion(dominioFinal, indice)
      val asignacionThis = asignacionFinal.proyectar(dominio)
      val asignacionNuevo = asignacionFinal.proyectar(nuevoArray.dominio)
      obtenerValor(asignacionThis) * nuevoArray.obtenerValor(asignacionNuevo)
    }).toList
    ValoresArray(dominioFinal, valoresFinal)
  }

  /**
    * Método para convertir objetos de tipo ValoresArray a ValoresArbol
    * @return ValoresArray si se ejecuta sobre un ValoresArbol, y viceversa
    */
  override def convertir : ValoresArbol = ???
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

case class ValoresArbol (override val dominio : Dominio) extends Valores (dominio) {
  override def obtenerValor(asignacion: Asignacion): Double = ???

  override def obtenerValores: List[Double] = ???

  override def toString: String = ???

  override def convertir : ValoresArray = ???

  def combinarArbolArbol (nuevoArbol : ValoresArbol) : ValoresArbol = ???
}