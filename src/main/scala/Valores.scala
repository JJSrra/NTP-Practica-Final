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

  /**
    * Método para restringir un objeto Valores con aquellas asignaciones donde una variable toma un estado
    * @param variable Variable que se va a restringir
    * @param estado Estado que toma dicha variable
    * @return Objeto Valores restringido
    */
  def restringir (variable : Variable, estado : Int) : Valores
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

  /**
    * Método que restringe un objeto ValoresArray para las asignaciones donde una variable tiene un estado
    * @param variable Variable a restringir
    * @param estado Estado que tiene dicha variable
    * @return Objeto ValoresArray restringido
    */
  override def restringir (variable : Variable, estado : Int) : ValoresArray = {
    val dominioFinal = dominio - variable
    val valoresFinal = (0 until dominioFinal.maximoIndice).map(indice => {
      val asignacionFinal = Asignacion(dominioFinal, indice)
      val asignacionCompleta = asignacionFinal + (variable, estado)
      val asignacionOrdenada = asignacionCompleta.proyectar(dominioFinal)
      obtenerValor(asignacionOrdenada)
    }).toList
    ValoresArray(dominioFinal, valoresFinal)
  }
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

case class ValoresArbol (override val dominio : Dominio, val raiz : Nodo) extends Valores (dominio) {
  override def obtenerValor(asignacion: Asignacion): Double = ???

  override def obtenerValores: List[Double] = ???

  override def toString: String = ???

  override def convertir : ValoresArray = ???

  override def restringir (variable : Variable, estado : Int) : ValoresArbol = ???

  def combinarArbolArbol (nuevoArbol : ValoresArbol) : ValoresArbol = ???
}

object ValoresArbol {
  def apply (dominio : Dominio, valores : List[Double]) : ValoresArbol = {

    def go (indice : Int, asignacion: Asignacion) : Nodo = {
      val variable = dominio.variables(indice)
      val nodoVariable = new NodoVariable(variable)
      if (indice < dominio.longitud-1) {
        (0 until variable.numEstados).map(estado => nodoVariable.listaHijos :+
          go(indice+1, asignacion + (variable, estado)))
      }
      else {
        (0 until variable.numEstados).map(estado => {
          val asignacionHoja = asignacion + (variable, estado)
          val hoja = NodoHoja(valores(asignacionHoja.calcularIndice))
          nodoVariable.listaHijos :+ hoja
        })
      }

      nodoVariable
    }

    val nodoRaiz = go(0, Asignacion(Dominio(List())))
    // Ahora se puede construir el objeto de tipo ValoresArbol

    new ValoresArbol(dominio, nodoRaiz)
  }
}

abstract class Nodo {
  def obtenerValores : List[Double]
}

case class NodoVariable (val nivel : Variable) extends Nodo {
  def obtenerValores : List[Double] = {
    listaHijos.map(indice => indice.obtenerValores).reduce(_ ::: _)
  }

  def obtenerHijo (estado : Int) : Nodo = listaHijos(estado)

  var listaHijos : List[Nodo] = List()
}

case class NodoHoja (val valor : Double) extends Nodo {
  override def obtenerValores : List[Double] = List(valor)
}