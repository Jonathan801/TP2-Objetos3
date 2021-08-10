package o3

import scala.collection.mutable.ArrayBuffer

sealed abstract class Operacion

abstract class OperacionBinaria extends Operacion{
   val valor1 :Operacion
   val valor2 :Operacion
}

trait OperacionAritmetica

trait OperacionBooleana

class Constante extends Operacion
//////////////////////////////////////////////////////////////////////////////////////////
//Constantes
case class Numero(valor : Int) extends Constante
//case class True extends Operacion
//case class False extends Operacion
case object True extends Constante
case object False extends Constante
case class Variable(valor : String) extends Constante
//////////////////////////////////////////////////////////////////////////////////////////
//Operaciones Aritmeticas
case class Suma(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionAritmetica
case class Resta(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionAritmetica
case class Multiplicacion(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionAritmetica
case class Division(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionAritmetica
//////////////////////////////////////////////////////////////////////////////////////////
//Operaciones Booleanas
case class Igual(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
case class Distinto(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
case class MenorQue(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
case class MayorQue(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
case class MenorIgualQue(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
case class MayorIgualQue(valor1 : Operacion,valor2 : Operacion) extends OperacionBinaria with OperacionBooleana
//////////////////////////////////////////////////////////////////////////////////////////
//Variables ; ver como se guardan las variables ( a lo funcional?)
//Aca podriamos hacer algo como una clase OperacionAritmetica en un trait para no poner algo de tipo booleano en el valor de DeclararVariable
case class DeclararVariable(nombre : String,valor : Operacion) extends Operacion
case class AsignarVariable(nombre : String,valor : Operacion) extends Operacion
case class Programa(listaAst:ArrayBuffer[Operacion])
