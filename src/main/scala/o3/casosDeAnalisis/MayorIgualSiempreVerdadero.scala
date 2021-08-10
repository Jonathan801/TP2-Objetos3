package o3.casosDeAnalisis

import o3.{MayorIgualQue, Numero, Operacion, Problema}

class MayorIgualSiempreVerdadero extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case MayorIgualQue(Numero(valor1), Numero(valor2)) if valor1 >= valor2 => generarProblema(MayorIgualQue(Numero(valor1), Numero(valor2)))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre verdadero, el primero es mayor o igual que el segundo", "Advertencia", operacion.toString)
  }
}
