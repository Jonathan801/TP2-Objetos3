package o3

import o3.casosDeAnalisis.{CasosDeAnalisis, DistincionSiempreFalsa, DistincionSiempreVerdadera, DivisionACero, DivisionPorCero, IgualdadSiempreVerdadera, MayorIgualSiempreFalso, MayorIgualSiempreVerdadero, MayorSiempreFalso, MayorSiempreVerdadero, MenorIgualSiempreFalso, MenorIgualSiempreVerdadero, MenorSiempreFalso, MenorSiempreVerdadero, MultiplicacionPorCero, MultiplicacionPorUno, RestaPorCero, SumaPorCero, VariableDuplicada, VariableNoDeclarada, VariableSinUtilizar, VariableUsadaAntesDeDeclararla}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

class AnalizadorSpec extends AnyFunSpec with Matchers {

  describe("Test respecto al analizador aritmetico"){

    it("Se analiza una operacion de suma y se encuentra un problema de suma por 0"){
      val analizador = new Analizador()
      val suma = Suma(Numero(5), Numero(0))
      val sumaPorCero = new SumaPorCero

      analizador.analizarOperacion(suma, List[CasosDeAnalisis](sumaPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se suma 0")
      analizador.problemasEncontrados.head.operacion should equal("Suma(Numero(5),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")

    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de suma con 0 dos veces"){
      val analizador = new Analizador()
      val suma = Resta(Suma(Numero(5), Numero(0)), Suma(Numero(5), Numero(0)))
      val sumaPorCero = new SumaPorCero

      analizador.analizarOperacion(suma, List[CasosDeAnalisis](sumaPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se suma 0")
      analizador.problemasEncontrados.head.operacion should equal("Suma(Numero(5),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
      analizador.problemasEncontrados.size should equal(2)
    }

    it("Se analiza una declaracion de variable y se encuentra un problema de suma con 0"){
      val analizador = new Analizador()
      val declaracion = DeclararVariable("tres", Suma(Numero(3), Numero(0)))
      val sumaPorCero = new SumaPorCero

      analizador.analizarOperacion(declaracion, List[CasosDeAnalisis](sumaPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se suma 0")
      analizador.problemasEncontrados.head.operacion should equal("Suma(Numero(3),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de resta con 0"){
      val analizador = new Analizador()
      val opAritm = Multiplicacion(Resta(Numero(5), Numero(0)), Suma(Numero(5), Numero(0)))
      val restaPorCero = new RestaPorCero

      analizador.analizarOperacion(opAritm, List[CasosDeAnalisis](restaPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se resta por 0")
      analizador.problemasEncontrados.head.operacion should equal("Resta(Numero(5),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se analiza una declaracion de variable con una operacion aritmetica anidada y se encuentra un problema de resta con 0"){
      val analizador = new Analizador()
      val declaracion = DeclararVariable("tres", Suma(Numero(3), Resta(Numero(2),Numero(0))))
      val restaPorCero = new RestaPorCero

      analizador.analizarOperacion(declaracion, List[CasosDeAnalisis](restaPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se resta por 0")
      analizador.problemasEncontrados.head.operacion should equal("Resta(Numero(2),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de multiplicacion con 0"){
      val analizador = new Analizador()
      val opAritm = Resta(Resta(Numero(5), Numero(0)), Multiplicacion(Numero(5), Numero(0)))
      val multPorCero = new MultiplicacionPorCero()

      analizador.analizarOperacion(opAritm, List[CasosDeAnalisis](multPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se multiplica por 0")
      analizador.problemasEncontrados.head.operacion should equal("Multiplicacion(Numero(5),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")


    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de multiplicacion con 1"){
      val analizador = new Analizador()
      val opAritm = Resta(Resta(Numero(5), Numero(0)), Multiplicacion(Numero(5), Numero(1)))
      val multPorUno = new MultiplicacionPorUno()

      analizador.analizarOperacion(opAritm, List[CasosDeAnalisis](multPorUno))

      analizador.problemasEncontrados.head.descripcion should equal("Se multiplica por 1")
      analizador.problemasEncontrados.head.operacion should equal("Multiplicacion(Numero(5),Numero(1))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de division con 0"){
      val analizador = new Analizador()
      val opAritm = Resta(Resta(Numero(5), Numero(0)), Division(Numero(5), Numero(0)))
      val divPorCero = new DivisionPorCero()

      analizador.analizarOperacion(opAritm, List[CasosDeAnalisis](divPorCero))

      analizador.problemasEncontrados.head.descripcion should equal("Se divide por 0")
      analizador.problemasEncontrados.head.operacion should equal("Division(Numero(5),Numero(0))")
      analizador.problemasEncontrados.head.gravedad should equal("Error")
    }

    it("Se analiza una operacion aritmetica anidada y se ecuentra un problema de division a 0"){
      val analizador = new Analizador()
      val opAritm = Resta(Resta(Numero(5), Numero(0)), Division(Numero(0), Numero(5)))
      val divACero = new DivisionACero()

      analizador.analizarOperacion(opAritm, List[CasosDeAnalisis](divACero))

      analizador.problemasEncontrados.head.descripcion should equal("Se divide a 0")
      analizador.problemasEncontrados.head.operacion should equal("Division(Numero(0),Numero(5))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se compara por Igual dos Numeros iguales, genera una advertencia de tipo Igual"){
      val analizador = new Analizador()
      val igual = Igual(Numero(5), Numero(5))
      val igualdadSiempreVerdadera = new IgualdadSiempreVerdadera

      analizador.analizarOperacion(igual, List[CasosDeAnalisis](igualdadSiempreVerdadera))

      analizador.problemasEncontrados.head.descripcion should equal("Siempre verdadero, se esta comparando lo mismo")
      analizador.problemasEncontrados.head.operacion should equal("Igual(Numero(5),Numero(5))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
    }

    it("Se compara por Menor dos Numeros, el primero es menor que el segundo, genera una advertencia de tipo MenorQue"){
      val analizador = new Analizador()
      val menorQue = MenorQue(Numero(3), Numero(4))
      val menorSiempreVerdadero = new MenorSiempreVerdadero

      analizador.analizarOperacion(menorQue, List[CasosDeAnalisis](menorSiempreVerdadero))

      val problema = analizador.problemasEncontrados.head
      problema.descripcion should equal("Siempre verdadero, el primero es menor que el segundo")
      problema.operacion should equal("MenorQue(Numero(3),Numero(4))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se compara por Menor dos Numeros, el primero es mayor que el segundo, se genera una advertencia de tipo MenorQue"){
      val analizador = new Analizador()
      val menorQue = MenorQue(Numero(4), Numero(3))
      val menorSiempreFalso = new MenorSiempreFalso

      analizador.analizarOperacion(menorQue, List[CasosDeAnalisis](menorSiempreFalso))

      val problema = analizador.problemasEncontrados.head
      problema.descripcion should equal("Siempre falso, el primero es mayor que el segundo")
      problema.operacion should equal("MenorQue(Numero(4),Numero(3))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se compara por Mayor dos Numeros, el primero es mayor que el segundo, se genera una advertencia de tipo MayorQue"){
      val analizador = new Analizador()
      val mayorQue = MayorQue(Numero(4),Numero(3))
      val mayorSiempreVerdadero = new MayorSiempreVerdadero

      analizador.analizarOperacion(mayorQue, List[CasosDeAnalisis](mayorSiempreVerdadero))

      val problema = analizador.problemasEncontrados.head
      problema.descripcion should equal("Siempre verdadero, el primero es mayor que el segundo")
      problema.operacion should equal("MayorQue(Numero(4),Numero(3))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se compara por mayor dos Numeros, el primero es menor que el segundo, se genera una advertencia de tipo MayorQue"){
      val analizador = new Analizador()
      val mayorQue = MayorQue(Numero(3),Numero(4))
      val mayorSiempreFalso = new MayorSiempreFalso

      analizador.analizarOperacion(mayorQue, List[CasosDeAnalisis](mayorSiempreFalso))

      val problema = analizador.problemasEncontrados.head
      problema.descripcion should equal("Siempre falso, el primero es menor que el segundo")
      problema.operacion should equal("MayorQue(Numero(3),Numero(4))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se compara por MayorIgual dos Numeros, " +
      "Si el primero es menor que el segundo => se genera una advertencia de tipo MayorIgualQue" +
      "Si el primero es mayor que el segundo => se genera una advertencia de tipo MayorIgualQue"+
      "Si el primero es igual al segundo => se genera una advertencia de tipo MayorIgualQue"){
      val analizador = new Analizador()
      val elPrimeroMenorQueElSegundo = MayorIgualQue(Numero(3),Numero(4))
      val elPrimeroMayorQueElSegundo = MayorIgualQue(Numero(4),Numero(3))
      val elPrimeroIgualAlSegundo = MayorIgualQue(Numero(4),Numero(4))

      val mayorIgualSiempreVerdadero = new MayorIgualSiempreVerdadero
      val mayorIgualSiempreFalso = new MayorIgualSiempreFalso

      analizador.analizarOperacion(elPrimeroIgualAlSegundo, List[CasosDeAnalisis](mayorIgualSiempreVerdadero))
      analizador.analizarOperacion(elPrimeroMayorQueElSegundo, List[CasosDeAnalisis](mayorIgualSiempreVerdadero))
      analizador.analizarOperacion(elPrimeroMenorQueElSegundo, List[CasosDeAnalisis](mayorIgualSiempreFalso))

      val problemaIgual = analizador.problemasEncontrados(2)
      val problemaMayor = analizador.problemasEncontrados(1)
      val problemaMenor = analizador.problemasEncontrados.head

      problemaIgual.descripcion should equal("Siempre verdadero, el primero es mayor o igual que el segundo")
      problemaMayor.descripcion should equal("Siempre verdadero, el primero es mayor o igual que el segundo")
      problemaMenor.descripcion should equal("Siempre falso, el primero es menor que el segundo")
      problemaIgual.operacion should equal("MayorIgualQue(Numero(4),Numero(4))")
      problemaMayor.operacion should equal("MayorIgualQue(Numero(4),Numero(3))")
      problemaMenor.operacion should equal("MayorIgualQue(Numero(3),Numero(4))")
    }

    it("Se compara por MenorIgual dos Numeros, " +
      "Si el primero es menor que el segundo => se genera una advertencia de tipo MenorIgualQue" +
      "Si el primero es mayor que el segundo => se genera una advertencia de tipo MenorIgualQue"+
      "Si el primero es igual al segundo => se genera una advertencia de tipo MenorIgualQue"){
      val analizador = new Analizador()
      val elPrimeroMenorQueElSegundo = MenorIgualQue(Numero(3),Numero(4))
      val elPrimeroMayorQueElSegundo = MenorIgualQue(Numero(4),Numero(3))
      val elPrimeroIgualAlSegundo = MenorIgualQue(Numero(4),Numero(4))

      val menorIgualSiempreVerdadero = new MenorIgualSiempreVerdadero
      val menorIgualSiempreFalso = new MenorIgualSiempreFalso

      analizador.analizarOperacion(elPrimeroIgualAlSegundo, List[CasosDeAnalisis](menorIgualSiempreVerdadero))
      analizador.analizarOperacion(elPrimeroMayorQueElSegundo, List[CasosDeAnalisis](menorIgualSiempreFalso))
      analizador.analizarOperacion(elPrimeroMenorQueElSegundo, List[CasosDeAnalisis](menorIgualSiempreVerdadero))

      val problemaIgual = analizador.problemasEncontrados(2)
      val problemaMayor = analizador.problemasEncontrados(1)
      val problemaMenor = analizador.problemasEncontrados.head

      problemaIgual.descripcion should equal("Siempre verdadero, el primero es menor o igual que el segundo")
      problemaMayor.descripcion should equal("Siempre falso, el primero es mayor que el segundo")
      problemaMenor.descripcion should equal("Siempre verdadero, el primero es menor o igual que el segundo")
      problemaIgual.operacion should equal("MenorIgualQue(Numero(4),Numero(4))")
      problemaMayor.operacion should equal("MenorIgualQue(Numero(4),Numero(3))")
      problemaMenor.operacion should equal("MenorIgualQue(Numero(3),Numero(4))")
    }

    it("Se compara por Distinto dos numeros distintos, se genera una advertencia de tipo Distinto"){
      val analizador = new Analizador()
      val distintoQue = Distinto(Numero(3),Numero(4))
      val distincionSiempreVerdadera = new DistincionSiempreVerdadera

      analizador.analizarOperacion(distintoQue, List[CasosDeAnalisis](distincionSiempreVerdadera))

      val problema = analizador.problemasEncontrados.head

      problema.descripcion should equal("Siempre verdadero, se esta comparando cosas distintas")
      problema.operacion should equal("Distinto(Numero(3),Numero(4))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se compara por Distinto dos numeros iguales, se genera una advertencia de tipo Distinto"){
      val analizador = new Analizador()
      val distintoQue = Distinto(Numero(4),Numero(4))
      val distincionSiempreFalsa = new DistincionSiempreFalsa

      analizador.analizarOperacion(distintoQue, List[CasosDeAnalisis](distincionSiempreFalsa))

      val problema = analizador.problemasEncontrados.head

      problema.descripcion should equal("Siempre falso, se esta comparando cosas iguales")
      problema.operacion should equal("Distinto(Numero(4),Numero(4))")
      problema.gravedad should equal("Advertencia")
    }

    it("Se analiza un programa y se encuentra un problema de variable duplicada"){
      val analizador = new Analizador()
      val variableDuplicada = new VariableDuplicada
      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(15)),
        DeclararVariable("suma", Suma(Variable("edad"), Numero(1))),
        DeclararVariable("edad", Numero(32))
      ))

      analizador.analizar(programa, List[CasosDeAnalisis](variableDuplicada))

      analizador.problemasEncontrados.head.descripcion should equal("La variable edad ya fue declarada")
      analizador.problemasEncontrados.head.operacion should equal("DeclararVariable(edad,Numero(32))")
      analizador.problemasEncontrados.head.gravedad should equal("Error")
      analizador.problemasEncontrados.size should equal(1)
    }

    it("Se analiza un programa y se encuentra un problema de variable no declarada"){
      val analizador = new Analizador()
      val variableNoDeclarada = new VariableNoDeclarada
      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("añoActual", Numero(2020)),
        Resta(Variable("añoActual"), Variable("edad"))
      ))

      analizador.analizar(programa, List[CasosDeAnalisis](variableNoDeclarada))

      analizador.problemasEncontrados.head.descripcion should equal("La variable edad no fue declarada")
      analizador.problemasEncontrados.head.operacion should equal("Variable(edad)")
      analizador.problemasEncontrados.head.gravedad should equal("Error")
      analizador.problemasEncontrados.size should equal(1)
    }

    it("Se analiza un programa y se encuentra un problema de variable sin usar"){
      val analizador = new Analizador()
      val variableSinUsar = new VariableSinUtilizar
      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("pepe", Numero(1)),
        DeclararVariable("añoActual", Numero(2020)),
        Resta(Variable("añoActual"), Numero(25))
      ))

      analizador.analizar(programa, List[CasosDeAnalisis](variableSinUsar))

      analizador.problemasEncontrados.head.descripcion should equal("La variable pepe no se utiliza")
      analizador.problemasEncontrados.head.operacion should equal("DeclararVariable(pepe,Numero(1))")
      analizador.problemasEncontrados.head.gravedad should equal("Advertencia")
      analizador.problemasEncontrados.size should equal(1)
    }

    it("Se analiza un programa y se encuentra un problema de variable utilizada antes de declarla"){
      val analizador = new Analizador()
      val variableUsadaAntesDeDeclararla = new VariableUsadaAntesDeDeclararla
      val programa = Programa(ArrayBuffer[Operacion](
        Resta(Variable("añoActual"), Numero(25)),
        DeclararVariable("añoActual", Numero(2020))
      ))

      analizador.analizar(programa, List[CasosDeAnalisis](variableUsadaAntesDeDeclararla))

      analizador.problemasEncontrados.head.descripcion should equal("La variable añoActual se esta usando antes de su declaracion")
      analizador.problemasEncontrados.head.operacion should equal("Variable(añoActual)")
      analizador.problemasEncontrados.head.gravedad should equal("Error")
      analizador.problemasEncontrados.size should equal(1)
    }
  }
}
