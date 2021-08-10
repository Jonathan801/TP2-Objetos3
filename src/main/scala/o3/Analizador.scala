package o3

import o3.casosDeAnalisis.CasosDeAnalisis

class Problema(var descripcion : String, var gravedad : String, var operacion: String)

class Analizador {

  var problemasEncontrados: List[Problema] = List()

  def agregarProblemas(problema: Problema): Unit = problemasEncontrados = problema +: problemasEncontrados

  def analizarOperacion(operacion: Operacion, casosDeAnalisis: List[CasosDeAnalisis]): Unit ={
    casosDeAnalisis.foreach{
      caso => caso.analizarAst(operacion, this)
    }
  }

  def analizar(programa: Programa, casosDeAnalisis: List[CasosDeAnalisis]): Unit ={
    casosDeAnalisis.foreach{
      caso => caso.analizar(programa, this)
    }
  }
}



