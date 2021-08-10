package o3.casosDeAnalisis

import o3.{MayorQue, Numero, Operacion, Problema}

class MayorSiempreFalso extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MayorQue(Numero(valor1), Numero(valor2)) if valor1 < valor2 => generarProblema(MayorQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre falso, el primero es menor que el segundo", "Advertencia", operacion.toString)
  }
}
