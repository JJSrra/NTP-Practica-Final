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
  override def convertir : ValoresArbol = ValoresArbol(dominio, valores)

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
  /**
    * Método para obtener el valor de una asignación
    * @param asignacion Asignación de la que obtener el valor
    * @return Valor de dicha asignación
    */
  override def obtenerValor(asignacion: Asignacion): Double = raiz.obtenerValor(asignacion, 0)

  /**
    * Método para obtener los valores asociados al objeto Valores
    * @return Lista de valores
    */
  override def obtenerValores: List[Double] = raiz.obtenerValores

  /**
    * Método toString para mostrar de forma gráfica el contenido del objeto
    * @return String del objeto
    */
  override def toString: String = raiz.toString(0)

  /**
    * Método para convertir un objeto ValoresArbol a un objeto ValoresArray
    * @return ValoresArray si se ejecuta sobre un ValoresArbol, y viceversa
    */
  override def convertir : ValoresArray = ValoresArray(dominio, obtenerValores)

  /**
    * Método para restringir un objeto ValoresArbol en torno a una variable y un estado
    * @param variable Variable que se va a restringir
    * @param estado Estado que toma dicha variable
    * @return Objeto ValoresArbol restringido
    */
  override def restringir (variable : Variable, estado : Int) : ValoresArbol = ValoresArbol(dominio, raiz.restringir(variable, estado))

  /**
    * Método auxiliar para combinar dos objetos ValoresArbol
    * @param nuevoArbol Objeto ValoresArbol que combinar con this
    * @return Nuevo objeto ValoresArbol
    */
  def combinarArbolArbol (nuevoArbol : ValoresArbol) : ValoresArbol = ValoresArbol(dominio, raiz.combinarArbolArbol(nuevoArbol.raiz))
}

object ValoresArbol {
  /**
    * Método apply para poder crear un objeto ValoresArbol mediante una lista de valores
    * @param dominio Dominio sobre el que crear el objeto ValoresArbol
    * @param valores Lista de valores asociada al objeto
    * @return Nuevo objeto ValoresArbol
    */
  def apply (dominio : Dominio, valores : List[Double]) : ValoresArbol = {

    def go (indice : Int, asignacion: Asignacion) : Nodo = {
      val variable = dominio.variables(indice)
      var listaHijos : List[Nodo] = List()
      if (indice < dominio.longitud-1) {
        listaHijos = (0 until variable.numEstados).map(estado =>
          go(indice+1, asignacion + (variable, estado))).toList
      }
      else {
        listaHijos = (0 until variable.numEstados).map(estado => {
          val asignacionHoja = asignacion + (variable, estado)
          NodoHoja(valores(asignacionHoja.calcularIndice))
        }).toList
      }
      val nodoVariable = new NodoVariable(variable, listaHijos)

      nodoVariable
    }

    val nodoRaiz = if (dominio.longitud == 0) NodoHoja(valores(0))
      else go(0, Asignacion(Dominio(List())))
    // Ahora se puede construir el objeto de tipo ValoresArbol

    new ValoresArbol(dominio, nodoRaiz)
  }
}

abstract class Nodo {
  /**
    * Método para obtener el valor de un nodo
    * @param asignacion Asignación asociada al valor
    * @param numHijo Parámetro que indica el nivel de hijo en el que se encuentra la búsqueda
    * @return Valor asociado a la asignación
    */
  def obtenerValor (asignacion: Asignacion, numHijo : Int) : Double

  /**
    * Método para obtener la lista de valores derivada de un nodo
    * @return Lista de valores derivada del nodo
    */
  def obtenerValores : List[Double]

  /**
    * Método toString
    * @param tabs Nivel de profundidad para incluir tabuladores
    * @return String ilustrativo del objeto
    */
  def toString (tabs : Int) : String

  /**
    * Método para restringir los valores derivados de un nodo en función de una variable y un estado
    * @param variableRest Variable a restringir
    * @param estadoRest Estado a restringir
    * @return Nuevo nodo asociado a los valores restringidos
    */
  def restringir (variableRest : Variable, estadoRest : Int) : Nodo

  /**
    * Método para combinar dos objetos Árbol Árbol a nivel de nodo
    * @param nuevaRaiz Nodo que combinar
    * @return Nuevo nodo combinado de ambos
    */
  def combinarArbolArbol (nuevaRaiz : Nodo) : Nodo
}

case class NodoVariable (nivel : Variable, listaHijos : List[Nodo]) extends Nodo {
  /**
    * Método para obtener el valor de un nodo
    * @param asignacion Asignación asociada al valor
    * @param numHijo Parámetro que indica el nivel de hijo en el que se encuentra la búsqueda
    * @return Valor asociado a la asignación
    */
  def obtenerValor (asignacion: Asignacion, numHijo : Int) : Double =
    obtenerHijo(asignacion.valores(numHijo)).obtenerValor(asignacion, numHijo+1)

  /**
    * Método para obtener la lista de valores derivada de un nodo
    * @return Lista de valores derivada del nodo
    */
  def obtenerValores : List[Double] = {
    listaHijos.map(indice => indice.obtenerValores).reduce(_ ::: _)
  }

  /**
    * Método toString
    * @param tabs Nivel de profundidad para incluir tabuladores
    * @return String ilustrativo del objeto
    */
  override def toString (tabs : Int) : String = listaHijos.indices.map(estado =>
    "\t"*tabs + nivel.nombre + " : " + estado + "\n" + obtenerHijo(estado).toString(tabs+1)) mkString "\n"

  /**
    * Método para obtener el nodo hijo asociado a un estado
    * @param estado Estado de la variable
    * @return Nodo hijo
    */
  def obtenerHijo (estado : Int) : Nodo = listaHijos(estado)

  /**
    * Método para combinar dos objetos Árbol Árbol a nivel de nodo
    * @param nuevaRaiz Nodo que combinar
    * @return Nuevo nodo combinado de ambos
    */
  def combinarArbolArbol(nuevaRaiz: Nodo): Nodo = {
    nuevaRaiz match {
      case nodoHoja:NodoHoja => {
        val hijos = listaHijos.indices.map(indice => obtenerHijo(indice).combinarArbolArbol(nuevaRaiz)).toList
        NodoVariable(nivel, hijos)
      }
      case nodoVariable:NodoVariable => {
        val hijos = listaHijos.indices.map(estado => obtenerHijo(estado).combinarArbolArbol(nodoVariable.restringir(nivel, estado))).toList
        NodoVariable(nivel, hijos)
      }
    }
  }

  /**
    * Método para restringir los valores derivados de un nodo en función de una variable y un estado
    * @param variableRest Variable a restringir
    * @param estadoRest Estado a restringir
    * @return Nuevo nodo asociado a los valores restringidos
    */
  override def restringir(variableRest: Variable, estadoRest: Int): Nodo = {
    if (nivel == variableRest) obtenerHijo(estadoRest)
    else {
      val hijos = listaHijos.indices.map(estado => obtenerHijo(estado).restringir(variableRest, estadoRest)).toList
      NodoVariable(nivel, hijos)
    }
  }
}

case class NodoHoja (valor : Double) extends Nodo {
  /**
    * Método para obtener el valor de un nodo
    * @param asignacion Asignación asociada al valor
    * @param numHijo Parámetro que indica el nivel de hijo en el que se encuentra la búsqueda
    * @return Valor asociado a la asignación
    */
  override def obtenerValor (asignacion: Asignacion, numHijo : Int) : Double = valor

  /**
    * Método para obtener la lista de valores derivada de un nodo
    * @return Lista de valores derivada del nodo
    */
  override def obtenerValores : List[Double] = List(valor)

  /**
    * Método toString
    * @param tabs Nivel de profundidad para incluir tabuladores
    * @return String ilustrativo del objeto
    */
  override def toString (tabs : Int) : String = "\t"*tabs + "= " + valor

  /**
    * Método para combinar dos objetos Árbol Árbol a nivel de nodo
    * @param nuevaRaiz Nodo que combinar
    * @return Nuevo nodo combinado de ambos
    */
  def combinarArbolArbol(nuevaRaiz: Nodo): Nodo = {
    nuevaRaiz match {
      case nodoHoja:NodoHoja => {
        NodoHoja(valor * nodoHoja.valor)
      }
      case nodoVariable:NodoVariable => {
        nodoVariable.combinarArbolArbol(this)
      }
    }
  }

  /**
    * Método para restringir los valores derivados de un nodo en función de una variable y un estado
    * @param variableRest Variable a restringir
    * @param estadoRest Estado a restringir
    * @return Nuevo nodo asociado a los valores restringidos
    */
  override def restringir (variableRest : Variable, estadoRest : Int) : Nodo = this
}