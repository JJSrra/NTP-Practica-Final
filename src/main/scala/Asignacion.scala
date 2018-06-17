class Asignacion (val dominio : Dominio, val valores : List[Int]) {

  /**
    * Dato miembro que almacena pares variable - valor en un mapa
    */
  val datos = asignarValoresADominio()

  /**
    * Método privado para asignar los valores a las variables del dominio
    * @return Mapa variable - valor para las variables del dominio
    */
  private def asignarValoresADominio(): Map[Variable, Int] = {
    if (dominio.longitud != valores.length) null
    else {
      (dominio.variables zip valores).map{
        case(variable, valor) => if (valor >= variable.numEstados || valor < 0) return null
        else (variable -> valor)
      }.toMap
    }
  }

  /**
    * Método para comprobar si la asignación está construida sobre un dominio vacío
    * @return True si el dominio está vacío, false en caso contrario
    */
  def vacia : Boolean = dominio.vacio
}

object Asignacion {
  def apply(dominio : Dominio, valores : List[Int]) : Asignacion = {
    new Asignacion(dominio, valores)
  }
}
