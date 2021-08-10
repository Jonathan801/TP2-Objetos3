package o3.casosDeAnalisis

import o3.{MayorQue, Numero, Operacion, Problema}

class MayorSiempreVerdadero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MayorQue(Numero(valor1), Numero(valor2)) if valor1 > valor2 => generarProblema(MayorQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre verdadero, el primero es mayor que el segundo", "Advertencia", operacion.toString)
  }
}
