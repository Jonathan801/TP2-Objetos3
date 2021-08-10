package o3.casosDeAnalisis

import o3.{Multiplicacion, Numero, Operacion, Problema}

class MultiplicacionPorCero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case Multiplicacion(operacion: Operacion, Numero(0)) => generarProblema(Multiplicacion(operacion, Numero(0)))
    case Multiplicacion(Numero(0), operacion: Operacion) => generarProblema(Multiplicacion(Numero(0), operacion))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se multiplica por 0", "Advertencia", operacion.toString)
  }
}
