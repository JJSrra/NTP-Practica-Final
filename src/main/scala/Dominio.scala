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

    go(0)

    return listaPesos
  }

  /**
    * Dato miembro que almacena pares variable - peso en un mapa
    */
  val pesosVariables = (variables zip calcularPesos).toMap
}
