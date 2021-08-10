package o3.casosDeAnalisis

import o3.{Division, Numero, Operacion, Problema}

class DivisionACero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case Division(Numero(0), operacion: Operacion) => generarProblema(Division(Numero(0), operacion))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se divide a 0", "Advertencia", operacion.toString)
  }
}
