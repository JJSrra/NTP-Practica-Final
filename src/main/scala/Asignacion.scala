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

  /**
    * Override del método toString para imprimir la asignación
    * @return String explicativo de la asignación
    */
  override def toString = {
    if (datos == null) "Asignación vacía"
    else datos.map { case (variable, valor) => "[" + variable.nombre + " - " + valor + "]" } mkString " "
  }

  /**
    * Método para devolver el número de variables del dominio
    * @return Longitud del dominio
    */
  def obtenerNumeroVariables : Int = dominio.longitud

  /**
    * Método para devolver el valor asociado a una variable
    * @param variable Variable de la que se quiere comprobar el valor
    * @return Valor en caso de que exista la variable, -1 en caso contrario
    */
  def obtenerValorVariable(variable : Variable) = datos.getOrElse(variable, null)

  def + (nuevaVariable : Variable, nuevoValor : Int) = Asignacion(dominio+nuevaVariable, valores:+nuevoValor)
}

object Asignacion {
  /**
    * Método para obtener un objeto Asignación sin utilizar la palabra reservada new
    * @param dominio El dominio sobre el que construir la asignación
    * @param valores Los valores que asociar a las variables del dominio
    * @return Nuevo objeto Asignación
    */
  def apply(dominio : Dominio, valores : List[Int]) : Asignacion = {
    new Asignacion(dominio, valores)
  }
}
