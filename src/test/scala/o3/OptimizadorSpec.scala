package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

class OptimizadorSpec extends AnyFunSpec with Matchers {
  val optimizador = new Optimizador()

  describe("Test respecto al optimizador aritmetico"){

    it("optimizo una suma 0"){

      val suma = Suma(Numero(5), Numero(0))

      optimizador.optimizarOperacionAritmetica(suma) should equal(Numero(5))

    }

    it("optimizo una suma 0 de lado izquierda"){

      val suma = Suma(Numero(0),Numero(5))

      optimizador.optimizarOperacionAritmetica(suma) should equal(Numero(5))

    }

    it("optimizo una suma 0 de anidada en una suma a la derecha"){

      val suma = Suma(Numero(0), Suma(Numero(5), Numero(0)))

      optimizador.optimizarOperacionAritmetica(suma) should equal(Numero(5))

    }

    it("optimizo una suma 0 de anidada en una suma a la izquierda"){

      val suma = Suma(Suma(Numero(5), Numero(0)), Numero(0))

      optimizador.optimizarOperacionAritmetica(suma) should equal(Numero(5))

    }

    it("optimizo una division por 1"){

      val division = Division(Numero(3), Numero(1))

      optimizador.optimizarOperacionAritmetica(division) should equal(Numero(3))

    }

    it("optimizo una division por 1 anidada a izquierda"){

      val division = Division(Division(Numero(4), Numero(1)), Numero(2))

      optimizador.optimizarOperacionAritmetica(division) should equal(Division(Numero(4) ,Numero(2)))

    }

    it("optimizo una division por 1 anidada a derecha"){

      val division = Division(Numero(2), Division(Numero(4), Numero(1)))

      optimizador.optimizarOperacionAritmetica(division) should equal(Division(Numero(2) , Numero(4)))

    }

    it("optimizo una resta por 0"){

      val resta = Resta(Numero(100) , Numero(0))

      optimizador.optimizarOperacionAritmetica(resta) should equal(Numero(100))

    }

    it("optimizo una resta por 0 a un numero negativo"){

      val resta = Resta(Numero(-1) , Numero(0))

      optimizador.optimizarOperacionAritmetica(resta) should equal(Numero(-1))

    }

    it("optimizo una resta por 0 anidada a izquierda"){

      val resta = Resta(Resta(Numero(100) , Numero(0)), Numero(50))

      optimizador.optimizarOperacionAritmetica(resta) should equal(Resta(Numero(100) , Numero(50)))

    }

    it("optimizo una resta por 0 anidada a derecha"){

      val resta = Resta(Numero(50), Resta(Numero(20) , Numero(0)))

      optimizador.optimizarOperacionAritmetica(resta) should equal(Resta(Numero(50) , Numero(20)))

    }

    it("optimizo una multiplicacion por 1"){

      val multiplicacion = Multiplicacion(Numero(5), Numero(1))

      optimizador.optimizarOperacionAritmetica(multiplicacion) should equal(Numero(5))

    }

    it("optimizo una multiplicacion 1 de lado izquierda"){

      val multiplicacion = Multiplicacion(Numero(1),Numero(5))

      optimizador.optimizarOperacionAritmetica(multiplicacion) should equal(Numero(5))

    }

    it("optimizo una suma compleja de un 0"){
      val suma = Suma(Numero(1),Suma(Numero(2),Suma(Numero(3),Numero(0))))

      optimizador.optimizarOperacionAritmetica(suma) should equal(Suma(Numero(1),Suma(Numero(2),Numero(3))))
    }

    it("optimizo una resta compleja de un 0"){
      val resta = Resta(Numero(1),Suma(Numero(2),Resta(Numero(3),Numero(0))))

      optimizador.optimizarOperacionAritmetica(resta) should equal(Resta(Numero(1),Suma(Numero(2),Numero(3))))
    }

    it("optimizo una multiplicacion compleja de un 1"){
      val multiplicacion = Multiplicacion(Numero(5),Suma(Numero(3),Multiplicacion(Numero(3),Numero(1))))

      optimizador.optimizarOperacionAritmetica(multiplicacion) should equal(Multiplicacion(Numero(5),Suma(Numero(3),Numero(3))))
    }

    it("optimizo una division compleja de un 1"){
      val division = Division(Numero(1),Suma(Numero(2),Division(Numero(3),Numero(1))))


      optimizador.optimizarOperacionAritmetica(division) should equal(Division(Numero(1),Suma(Numero(2),Numero(3))))
    }

    it("optimizo una operacion compleja para probar el optimizador"){
      val operacion = Division(Numero(1),Resta(Suma(Numero(0),Multiplicacion(Numero(3),Numero(1))),Numero(3)))


      optimizador.optimizarOperacionAritmetica(operacion) should equal(Division(Numero(1),Resta(Numero(3),Numero(3))))
    }

    it("optimizo una multiplicacion 1 anidada izquierda"){

      val multiplicacion = Multiplicacion(Multiplicacion(Numero(1),Numero(5)), Numero(5))

      optimizador.optimizarOperacionAritmetica(multiplicacion) should equal(Multiplicacion(Numero(5),Numero(5)))
    }

    it("optimizo una multiplicacion 1 anidada derecha"){

      val multiplicacion = Multiplicacion(Numero(5), Multiplicacion(Numero(1),Numero(5)))

      optimizador.optimizarOperacionAritmetica(multiplicacion) should equal(Multiplicacion(Numero(5),Numero(5)))

    }

    it("optimizo un Igual de dos Numeros iguales el resultado es True"){
      val igualQue = Igual(Numero(3),Numero(3))

      optimizador.optimizarOperacionAritmetica(igualQue) should equal(True)
    }

    it("optimizo un Igual de dos Numeros distintos el resutaldo es false"){
      val igualQue = Igual(Numero(3),Numero(4))

      optimizador.optimizarOperacionAritmetica(igualQue) should equal(False)
    }

    it("optimizo un Mayor de dos Numeros, el primero mayor que el segundo, resultado true"){
      val mayorQue = MayorQue(Numero(4),Numero(3))

      optimizador.optimizarOperacionAritmetica(mayorQue) should equal(True)
    }

    it("optimizo un Mayor de dos Numeros, el primero menor que el segundo, resultado false"){
      val mayorQue = MayorQue(Numero(2),Numero(3))

      optimizador.optimizarOperacionAritmetica(mayorQue) should equal(False)
    }

    it("optimizo un Menor de dos Numeros, el primero menor que el segundo, resultado true"){
      val menorQue = MenorQue(Numero(2),Numero(3))

      optimizador.optimizarOperacionAritmetica(menorQue) should equal(True)
    }

    it("optimizo un Menor de dos Numeros, el primero mayor que el segundo, resultado False"){
      val menorQue = MenorQue(Numero(4),Numero(3))

      optimizador.optimizarOperacionAritmetica(menorQue) should equal(False)
    }

    it("optimizo un MenorIgual de dos Numeros, el primero mayor que el segundo, resultado False"){
      val menorIgualQue = MenorIgualQue(Numero(4),Numero(3))

      optimizador.optimizarOperacionAritmetica(menorIgualQue) should equal(False)
    }

    it("optimizo un MenorIgual de dos Numeros, el primero menor que el segundo, resultado true"){
      val menorIgualQue = MenorIgualQue(Numero(2),Numero(3))

      optimizador.optimizarOperacionAritmetica(menorIgualQue) should equal(True)
    }

    it("optimizo un MenorIgual de dos Numeros, el primero igual que el segundo, resultado true"){
      val menorIgualQue = MenorIgualQue(Numero(4),Numero(4))

      optimizador.optimizarOperacionAritmetica(menorIgualQue) should equal(True)
    }

    it("optimizo un MayorIgual de dos Numeros, el primero igual que el segundo, resultado true"){
      val mayorIgualQue = MayorIgualQue(Numero(4),Numero(4))

      optimizador.optimizarOperacionAritmetica(mayorIgualQue) should equal(True)
    }

    it("optimizo un MayorIgual de dos Numeros, el primero mayor que el segundo, resultado true"){
      val mayorIgualQue = MayorIgualQue(Numero(4),Numero(3))

      optimizador.optimizarOperacionAritmetica(mayorIgualQue) should equal(True)
    }

    it("optimizo un MayorIgual de dos Numeros, el primero menor que el segundo, resultado False"){
      val mayorIgualQue = MayorIgualQue(Numero(2),Numero(4))

      optimizador.optimizarOperacionAritmetica(mayorIgualQue) should equal(False)
    }

    it("optimizo un Distinto de dos Numeros, el primero distinto al segundo , resultado true"){
      val distintoQue = Distinto(Numero(2),Numero(5))

      optimizador.optimizarOperacionAritmetica(distintoQue) should equal(True)
    }

    it("optimizo un Distinto de dos Numeros, el primero igual al segundo , resultado false"){
      val distintoQue = Distinto(Numero(5),Numero(5))

      optimizador.optimizarOperacionAritmetica(distintoQue) should equal(False)
    }

    it("Test variable"){

      val una = DeclararVariable("pepe", Numero(1))
      val dos = DeclararVariable("añoActual", Numero(2020))
      val tres = Resta(Variable("añoActual"), Numero(25))

      val astsOptimizadas = ArrayBuffer[Operacion](una,dos,tres)
      //val program: Programa = Programa(astsOptimizadas)

      val result =  optimizador.optimizaroperacionVariable(astsOptimizadas)
      result.listaAst.size should equal(2)
    }

    it("test de caso recursivo extra"){
      val algo = Suma(Resta(Numero(0),Numero(0)),MenorQue(Numero(1),Numero(2)))

      optimizador.optimizarOperacionAritmetica(algo) should equal(Suma(Numero(0),True))
    }

  }

}
