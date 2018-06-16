class Variable (val nombre : String, val numEstados : Int) {
  /**
    * Método toString para imprimir el objeto Variable
    * @return
    */
  override def toString() = "Variable: " + nombre + " | Estados: " + numEstados
}

object Variable {
  /**
    * Método para obtener un objeto Variable sin utilizar la palabra reservada new
    * @param nombre nombre de la variable
    * @param numEstados número de estados de la variable
    * @return Nuevo objeto Variable
    */
  def apply(nombre : String, numEstados : Int) : Variable = {
    new Variable(nombre, numEstados)
  }
}