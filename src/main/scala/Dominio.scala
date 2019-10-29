class Dominio (val variables : List[Variable]) {

  /**
    * Dato miembro que almacena pares variable - índice en un mapa
    */
  val indiceVariables = variables.zipWithIndex.toMap

  /**
    * Función privada para calcular pesos de un dominio
    */
  private def calcularPesos : List[Int] = {

    var listaPesos = List[Int]()

    def go(indice : Int): Int = {
      if (indice == variables.length-1) {
        listaPesos = 1 :: listaPesos
        return 1
      }
      else {
        val peso = variables(indice+1).numEstados * go(indice+1)
        listaPesos = peso :: listaPesos
        return peso
      }
    }

    variables.length != 0 ? go(0) : return listaPesos
  }

  /**
    * Dato miembro que almacena pares variable - peso en un mapa
    */
  val pesosVariables = (variables zip calcularPesos).toMap

  /**
    * Método para comprobar si el dominio está vacío
    * @return True si el dominio está vacío, false en caso contrario
    */
  def vacio : Boolean = variables.isEmpty

  /**
    * Método para obtener el número de variables del dominio
    * @return Longitud de la lista de variables asociada al dominio
    */
  def longitud : Int = variables.length

  /**
    * Método para obtener el vector de pesos de las variables del dominio
    * @return Lista de pesos de las variables del dominio
    */
  def pesos : List[Int] = pesosVariables.values.toList

  /**
    * Método para obtener una unidad más que el máximo índice que se puede utilizar en este dominio
    * @return Máximo índice como producto del peso máximo por el número de estados de la mayor variable
    */
  def maximoIndice : Int = pesosVariables(variables(0)) * variables(0).numEstados

  /**
    * Método de acceso a la variable que ocupa una posición
    * @param indice Posición a acceder
    * @return Variable en la posición índice, null si la posición no existe
    */
  def apply (indice : Int) : Variable = {
    if (indice >= longitud || indice < 0) null
    else variables(indice)
  }

  /**
    * Override del método toString para imprimir el dominio
    * @return String explicativo del dominio
    */
  override def toString = variables.map(variable => variable.nombre + "(s: " + variable.numEstados
      + " w: " + pesosVariables(variable) + ")") mkString " "

  /**
    * Sobrecarga del operador + para añadir una nueva variable al dominio
    * @param nuevaVariable Nueva variable a añadir al dominio
    * @return Nuevo dominio que contiene la variable
    */
  def + (nuevaVariable : Variable) : Dominio = {
    if (variables.contains(nuevaVariable)) this
    else Dominio(variables :+ nuevaVariable)
  }

  /**
    * Sobrecarga del operador + para hacer la suma de dos dominios
    * @param nuevoDominio Nuevo dominio cuyas variables añadir al dominio
    * @return Nuevo dominio que contiene las variables de ambos
    */
  def + (nuevoDominio : Dominio) : Dominio = {
    val nuevasVariables = (variables ++ nuevoDominio.variables).distinct
    Dominio(nuevasVariables)
  }

  /**
    * Sobrecarga del operador - para eliminar una variable del dominio
    * @param variableEliminada Variable que se quiere eliminar del dominio
    * @return Nuevo dominio sin la variable a eliminar
    */
  def - (variableEliminada : Variable) : Dominio = {
    if (!variables.contains(variableEliminada)) this
    else Dominio(variables.filter(variable => variable != variableEliminada))
  }

  /**
    * Sobrecarga del operador - para hacer la resta entre dominios
    * @param nuevoDominio Dominio a restar
    * @return Nuevo dominio sin las variables del dominio restado
    */
  def - (nuevoDominio : Dominio) : Dominio = {
    val nuevasVariables = variables diff nuevoDominio.variables
    Dominio(nuevasVariables)
  }

}

object Dominio {
  /**
    * Método para obtener un objeto Dominio sin utilizar la palabra reservada new
    * @param variables Lista de variables que compondrán el dominio
    * @return Nuevo objeto Dominio
    */
  def apply(variables : List[Variable]) : Dominio = {
    new Dominio(variables)
  }
}
