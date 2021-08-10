package o3.casosDeAnalisis

import o3.{Analizador, DeclararVariable, Operacion, Problema, Programa, Variable}

class VariableNoDeclarada extends CasosDeAnalisis {

  var variablesDeclaradas: List[String] = List()

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case op: Variable if !variablesDeclaradas.contains(op.valor) => generarProblema(op)
  }

  def generarProblema(operacion: Variable): Problema ={
    new Problema(s"La variable ${operacion.valor} no fue declarada", "Error", operacion.toString)
  }

  override def analizar(programa: Programa, analizador: Analizador): Unit = {
    programa.listaAst.foreach { operacion =>
        operacion match {
          case declararVariable: DeclararVariable => variablesDeclaradas = declararVariable.nombre +: variablesDeclaradas
          case _                                  => ()
        }
        analizarAst(operacion, analizador)
    }
  }
}