package o3.casosDeAnalisis

import o3.{MenorQue, Numero, Operacion, Problema}

class MenorSiempreFalso extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MenorQue(Numero(valor1), Numero(valor2)) if valor1 > valor2 => generarProblema(MenorQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre falso, el primero es mayor que el segundo", "Advertencia", operacion.toString)
  }
}
