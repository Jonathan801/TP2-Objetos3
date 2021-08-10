package o3.casosDeAnalisis

import o3.{Analizador, DeclararVariable, Operacion, Problema, Programa, Variable}

class VariableUsadaAntesDeDeclararla extends CasosDeAnalisis {

  var variablesDeclaradasTotales: List[String] = List()
  var variablesDeclaradasMomentaneas :List[String] = List()

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case op: Variable if variablesDeclaradasTotales.contains(op.valor) &&
      !variablesDeclaradasMomentaneas.contains(op.valor) => generarProblema(op)

  }

  def generarProblema(operacion: Variable): Problema ={
    new Problema(s"La variable ${operacion.valor} se esta usando antes de su declaracion",
      "Error", operacion.toString)
  }

  def getDeclaracionDeVariablesTotales(programa: Programa): Unit ={
    programa.listaAst.foreach(operacion => operacion match {
      case DeclararVariable(nombre ,_)  => variablesDeclaradasTotales = nombre +: variablesDeclaradasTotales
      case _                            => ()
    })
  }

  override def analizar(programa: Programa, analizador: Analizador): Unit = {
    getDeclaracionDeVariablesTotales(programa)
    programa.listaAst.foreach { operacion =>
      operacion match {
        case DeclararVariable(nombre ,_) => variablesDeclaradasMomentaneas = nombre +: variablesDeclaradasMomentaneas
        case _                           => ()
      }
      analizarAst(operacion, analizador)
    }
  }
}
