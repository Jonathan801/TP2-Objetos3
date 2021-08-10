package o3.casosDeAnalisis

import o3.{Analizador, DeclararVariable, Operacion, Problema, Programa}

class VariableDuplicada extends CasosDeAnalisis {

  var variablesDeclaradas: List[String] = List()

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case op: DeclararVariable if variablesDeclaradas.contains(op.nombre) => generarProblema(op)
  }

  def generarProblema(operacion: DeclararVariable): Problema ={
    new Problema(s"La variable ${operacion.nombre} ya fue declarada", "Error", operacion.toString)
  }

  override def analizar(programa: Programa, analizador: Analizador): Unit = {
    programa.listaAst.foreach{operacion =>
      informarSiHayProblema(operacion, analizador)
      operacion match {
        case op :DeclararVariable => variablesDeclaradas = op.nombre +: variablesDeclaradas
        case _                    => ()
      }
    }
  }
}
