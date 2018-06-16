package Variable

class Variable (val nombre : String, val numEstados : Int) {
  override def toString() = "Variable: " + nombre + " | Estados: " + numEstados
}

object Variable {
  def apply(nombre : String, numEstados : Int) : Variable = {
    new Variable(nombre, numEstados)
  }
}