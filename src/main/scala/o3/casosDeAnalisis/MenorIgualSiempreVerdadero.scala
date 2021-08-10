package o3.casosDeAnalisis

import o3.{MenorIgualQue, Numero, Operacion, Problema}

class MenorIgualSiempreVerdadero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MenorIgualQue(Numero(valor1), Numero(valor2)) if valor1 <= valor2 => generarProblema(MenorIgualQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre verdadero, el primero es menor o igual que el segundo", "Advertencia", operacion.toString)
  }
}