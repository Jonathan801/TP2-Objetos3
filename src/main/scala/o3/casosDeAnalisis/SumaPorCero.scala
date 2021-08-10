package o3.casosDeAnalisis

import o3.{Numero, Operacion, Problema, Suma}

class SumaPorCero extends CasosDeAnalisis {

  val casoDeAnalisis = {
    case Suma(valor :Operacion, Numero(0)) => generarProblema(Suma(valor, Numero(0)))
    case Suma(Numero(0), valor :Operacion) => generarProblema(Suma(Numero(0), valor))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se suma 0", "Advertencia", operacion.toString)
  }
}
