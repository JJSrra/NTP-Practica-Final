package Variable

class Variable (val nombre : String, val numEstados : Int) {

  def apply(nombre : String, numEstados : Int) : Variable = {
    return new Variable(nombre, numEstados)
  }
}