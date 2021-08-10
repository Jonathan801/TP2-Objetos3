package o3.casosDeAnalisis

import o3.{MayorIgualQue, Numero, Operacion, Problema}

class MayorIgualSiempreFalso extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MayorIgualQue(Numero(valor1), Numero(valor2)) if valor1 < valor2 => generarProblema(MayorIgualQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre falso, el primero es menor que el segundo", "Advertencia", operacion.toString)
  }
}
