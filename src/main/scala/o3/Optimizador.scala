package o3

import scala.collection.mutable.ArrayBuffer

class Optimizador {

  var variablesAsignadas :ArrayBuffer[String] = ArrayBuffer[String]()
  var variablesUsadas : ArrayBuffer[String] = ArrayBuffer[String]()
  var astsOptimizadas :ArrayBuffer[Operacion] = ArrayBuffer[Operacion]()

  def optimizaroperacionVariable(asts: ArrayBuffer[Operacion]): Programa = {
    for (element <- asts) {
      element match {
        case DeclararVariable(nombre, valor) =>
          variablesAsignadas = variablesAsignadas :+ nombre
          DeclararVariable(nombre, valor)
        case _ => otrosCasosVariables(element)
      }
    }
    for (element <- asts) {
      element match {
        case DeclararVariable(nombre, valor) =>
          if (!variablesUsadas.contains(nombre))
            ()
          else
            astsOptimizadas = astsOptimizadas :+  DeclararVariable(nombre, valor)
        case ast => astsOptimizadas = astsOptimizadas :+ ast
      }
    }
    Programa(astsOptimizadas)
  }

  def otrosCasosVariables(ast: Operacion): Operacion = {
    ast match {
      case Variable(nombre) =>
        variablesUsadas = variablesUsadas :+ nombre
        Variable(nombre)
      case Suma(as: Variable, algo) => Suma(otrosCasosVariables(as), algo)
      case Suma(algo, as: Variable) => Suma(algo, otrosCasosVariables(as))
      case Resta(as: Variable, algo) => Resta(otrosCasosVariables(as), algo)
      case Resta(algo, as: Variable) => Resta(algo, otrosCasosVariables(as))
      case Division(as: Variable, algo) => Division(otrosCasosVariables(as), algo)
      case Division(algo, as: Variable) => Division(algo, otrosCasosVariables(as))
      case Multiplicacion(as: Variable, algo) => Multiplicacion(otrosCasosVariables(as), algo)
      case Multiplicacion(algo, as: Variable) => Multiplicacion(algo, otrosCasosVariables(as))
    }
  }

  def optimizarOperacionAritmetica(ast: Operacion): Operacion = ast match {
    //Suma(Numero(1), Suma(Numero(2), Numero(0)) -> ???
    case Suma(valor1, Numero(0))                        => optimizarOperacionAritmetica(valor1)
    case Suma(Numero(0), valor2)                        => optimizarOperacionAritmetica(valor2)
    case Suma(valor1, valor2)                           => Suma(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case Multiplicacion(valor, Numero(1))               => optimizarOperacionAritmetica(valor)
    case Multiplicacion(Numero(1), valor)               => optimizarOperacionAritmetica(valor)
    case Multiplicacion(valor1, valor2)                 => Multiplicacion(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case Resta(valor, Numero(0))                        => optimizarOperacionAritmetica(valor)
    case Resta(valor1, valor2)                          => Resta(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case Division(valor, Numero(1))                     => optimizarOperacionAritmetica(valor)
    case Division(valor1, valor2)                       => Division(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case Igual(Numero(valor1), Numero(valor2))          => if (valor1 == valor2) {True} else False
    case Igual(valor1, valor2)                          => Igual(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case MayorQue(Numero(valor1), Numero(valor2))       => if (valor1 > valor2)  {True} else False
    case MayorQue(valor1, valor2)                       => MayorQue(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case MenorQue(Numero(valor1), Numero(valor2))       => if (valor1 < valor2)  {True} else False
    case MenorQue(valor1, valor2)                       => MenorQue(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case MayorIgualQue(Numero(valor1), Numero(valor2))  => if (valor1 >= valor2) {True} else False
    case MayorIgualQue(valor1, valor2)                  => MayorIgualQue(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case MenorIgualQue(Numero(valor1), Numero(valor2))  => if (valor1 <= valor2) {True} else False
    case MenorIgualQue(valor1, valor2)                  => MenorIgualQue(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case Distinto(Numero(valor1), Numero(valor2))       => if (valor1 != valor2) {True} else False
    case Distinto(valor1, valor2)                       => Distinto(optimizarOperacionAritmetica(valor1),
      optimizarOperacionAritmetica(valor2))
    case _                                              => ast
  }
}