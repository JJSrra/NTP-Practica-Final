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

    if (variables.length != 0)
      go(0)

    return listaPesos
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