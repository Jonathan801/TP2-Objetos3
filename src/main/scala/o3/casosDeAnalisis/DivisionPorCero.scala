package o3.casosDeAnalisis

import o3.{Division, Numero, Operacion, Problema}

class DivisionPorCero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case Division(operacion: Operacion, Numero(0)) => generarProblema(Division(operacion, Numero(0)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se divide por 0", "Error", operacion.toString)
  }
}
