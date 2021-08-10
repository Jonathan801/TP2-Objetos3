package o3.casosDeAnalisis

import o3.{Numero, Operacion, Problema, Resta}

class RestaPorCero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case Resta(operacion: Operacion, Numero(0)) => generarProblema(Resta(operacion, Numero(0)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se resta por 0", "Advertencia", operacion.toString)
  }
}