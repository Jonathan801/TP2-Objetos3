package o3.casosDeAnalisis

import o3.{Distinto, Operacion, Problema}

class DistincionSiempreVerdadera extends CasosDeAnalisis {


  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case distinto :Distinto if distinto.valor1 != distinto.valor2 => generarProblema(distinto)
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre verdadero, se esta comparando cosas distintas", "Advertencia", operacion.toString)
  }
}
