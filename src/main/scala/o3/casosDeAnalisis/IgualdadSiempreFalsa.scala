package o3.casosDeAnalisis

import o3.{Igual, Operacion, Problema}

class IgualdadSiempreFalsa extends CasosDeAnalisis {


  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case igual :Igual if igual.valor1 != igual.valor2 => generarProblema(igual)
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Siempre falso, se esta comparando cosas distintas", "Advertencia", operacion.toString)
  }
}
