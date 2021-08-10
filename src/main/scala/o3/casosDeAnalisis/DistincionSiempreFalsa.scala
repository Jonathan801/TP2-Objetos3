package o3.casosDeAnalisis

import o3.{Distinto, Operacion, Problema}

class DistincionSiempreFalsa extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case distinto :Distinto if distinto.valor1 == distinto.valor2 => generarProblema(distinto)
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre falso, se esta comparando cosas iguales", "Advertencia", operacion.toString)
  }
}