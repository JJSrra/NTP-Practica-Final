class Dominio (val variables : List[Variable]) {

  /**
    * Dato miembro que almacena pares variable - índice en un mapa
    */
  val indiceVariables = variables.zipWithIndex.toMap
}
