package o3.casosDeAnalisis

import o3._

abstract class CasosDeAnalisis {

  val casoDeAnalisis :PartialFunction[Operacion, Problema]

  def informarSiHayProblema(operacion: Operacion, analizador: Analizador): Unit ={
    if (casoDeAnalisis isDefinedAt operacion){
      analizador.agregarProblemas(casoDeAnalisis apply operacion)
    }
  }

  def analizarAst(ast :Operacion, analizador: Analizador): Unit ={
    ast match {
      case constante: Constante                      => informarSiHayProblema(constante, analizador)
      case DeclararVariable(_, operacion :Operacion) => informarSiHayProblema(operacion, analizador)
                                                        analizarAst (operacion, analizador)
      case operacionBinaria: OperacionBinaria        => informarSiHayProblema(operacionBinaria, analizador)
                                                        analizarAst(operacionBinaria.valor1, analizador)
                                                        analizarAst(operacionBinaria.valor2, analizador)
      case _                                         => ()

    }
  }

  def analizar(programa :Programa, analizador: Analizador): Unit ={
    programa.listaAst.foreach(operacion => analizarAst(operacion, analizador))
  }

}





