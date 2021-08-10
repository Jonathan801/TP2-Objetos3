package o3.casosDeAnalisis

import o3.{MenorQue, Numero, Operacion, Problema}

class MenorSiempreVerdadero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MenorQue(Numero(valor1), Numero(valor2)) if valor1 < valor2 => generarProblema(MenorQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre verdadero, el primero es menor que el segundo", "Advertencia", operacion.toString)
  }
}
