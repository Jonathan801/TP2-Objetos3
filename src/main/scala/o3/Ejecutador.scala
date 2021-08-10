package o3

import java.util.NoSuchElementException

class Ejecutador {

  var variables :Map[String, Operacion] = Map()

  def ejecutar(programa: Programa): Any ={
    programa.listaAst.dropRight(1).foreach(operacion => ejecutarAst(operacion))
    val resultado = ejecutarAst(programa.listaAst.last)
    variables = Map()
    resultado
  }

  def ejecutarAst(operacion: Operacion): Any = operacion match {
      case DeclararVariable(nombre, valor)          => variables = variables + (nombre -> valor)
      case AsignarVariable(nombre, valor)           => variables = variables updated(nombre, valor)
      case constante: Constante                     => ejecutarConstante(constante)
      case operacionBooleana: OperacionBooleana     => ejecutarBooleano(operacionBooleana)
      case operacionAritmetica: OperacionAritmetica => ejecutarAritmetica(operacionAritmetica)
    }

  def ejecutarConstante(constante: Constante) :Any = constante match {
    case numero: Numero  => numero.valor
    case True            => true
    case False           => false
    case Variable(valor) => try {
                              ejecutarAst(variables(valor))
                            } catch {
                              case _:NoSuchElementException => throw new NoSuchElementException(s"La variable $valor no fue declarada")
                            }
  }

  def ejecutarAritmetica(ast:Operacion): Int = ast match{
    case constante: Constante           => verificarNumero(ejecutarConstante(constante))
    case Suma(op1,op2)                  => ejecutarAritmetica(op1) + ejecutarAritmetica(op2)
    case Resta(valor1, valor2)          => ejecutarAritmetica(valor1) - ejecutarAritmetica(valor2)
    case Division(valor1, valor2)       => ejecutarAritmetica(valor1) / ejecutarAritmetica(valor2)
    case Multiplicacion(valor1, valor2) => ejecutarAritmetica(valor1) * ejecutarAritmetica(valor2)
    case _                              => throw new IllegalArgumentException("Operacion invalida")
  }

  def ejecutarBooleano(ast:Operacion) : Any = ast match{
    case constante: Constante          => ejecutarConstante(constante)
    case igual: Igual                  => ejecutarIgual(igual)
    case distinto: Distinto            => ejecutarDistinto(distinto)
    case MayorQue(valor1, valor2)      => ejecutarAritmetica(valor1) > ejecutarAritmetica(valor2)
    case MenorQue(valor1, valor2)      => ejecutarAritmetica(valor1) < ejecutarAritmetica(valor2)
    case MayorIgualQue(valor1, valor2) => ejecutarAritmetica(valor1) >= ejecutarAritmetica(valor2)
    case MenorIgualQue(valor1, valor2) => ejecutarAritmetica(valor1) <= ejecutarAritmetica(valor2)
    case operacion: Operacion          => ejecutarAritmetica(operacion)
  }

  def verificarNumero(resultadoVar :Any): Int = resultadoVar match {
      case numero: Int => numero
      case _           => throw new IllegalArgumentException("Operacion invalida")
    }

  def ejecutarIgual(igual: Igual) :Boolean = igual match {
    case Igual(valor1 :OperacionAritmetica,
               valor2:OperacionAritmetica) => ejecutarAritmetica(valor1) == ejecutarAritmetica(valor2)
    case Igual(valor1, valor2)             => ejecutarBooleano(valor1) == ejecutarBooleano(valor2)
  }

  def ejecutarDistinto(distinto: Distinto) :Boolean = distinto match {
    case Distinto(valor1 :OperacionAritmetica,
                  valor2:OperacionAritmetica) => ejecutarAritmetica(valor1) != ejecutarAritmetica(valor2)
    case Distinto(valor1, valor2)             => ejecutarBooleano(valor1) != ejecutarBooleano(valor2)
  }

}

