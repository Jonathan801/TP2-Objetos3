package o3.casosDeAnalisis

import o3.{Analizador, DeclararVariable, Operacion, OperacionBinaria, Problema, Programa, Variable}

class VariableSinUtilizar extends CasosDeAnalisis{

  var variablesUtilizadas: List[String] = List()

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case op: DeclararVariable if !variablesUtilizadas.contains(op.nombre) => generarProblema(op)
  }

  def generarProblema(operacion: DeclararVariable): Problema ={
    new Problema(s"La variable ${operacion.nombre} no se utiliza", "Advertencia", operacion.toString)
  }

  def variablesDeUnAST(ast :Operacion): Unit = {
    ast match {
      case variable: Variable                 => variablesUtilizadas = variable.valor +: variablesUtilizadas
      case operacionBinaria: OperacionBinaria => variablesDeUnAST(operacionBinaria.valor1)
                                                 variablesDeUnAST(operacionBinaria.valor2)
      case _                                  => ()
    }
  }

  def obtenerVariablesUtilizadas(programa: Programa): Unit ={
    programa.listaAst.foreach(operacion => variablesDeUnAST(operacion))
  }

  override def analizar(programa: Programa, analizador: Analizador): Unit = {
    obtenerVariablesUtilizadas(programa)
    programa.listaAst.foreach(operacion => informarSiHayProblema(operacion, analizador))
  }
}

