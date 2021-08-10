package o3.casosDeAnalisis

import o3.{Multiplicacion, Numero, Operacion, Problema}

class MultiplicacionPorUno extends CasosDeAnalisis {

  override val casoDeAnalisis: PartialFunction[Operacion, Problema] = {
    case Multiplicacion(operacion: Operacion, Numero(1)) => generarProblema(Multiplicacion(operacion, Numero(1)))
    case Multiplicacion(Numero(1), operacion: Operacion) => generarProblema(Multiplicacion(Numero(1), operacion))
  }

  def generarProblema(operacion: Operacion): Problema ={
    new Problema("Se multiplica por 1", "Advertencia", operacion.toString)
  }
}
