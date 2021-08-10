package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

class EjecutadorSpec extends AnyFunSpec with Matchers {
  val ejecutador = new Ejecutador()

  describe("Test respecto al ejecutador aritmetico"){

    it("se ejecuta una suma entre 2 numeros y se obtiene su resultado"){

      val suma = Suma(Numero(10), Numero(5))

      ejecutador.ejecutarAst(suma) should equal(15)
    }

    it("se ejecuta una resta entre 2 numeros y se obtiene su resultado"){

      val resta = Resta(Numero(10), Numero(5))

      ejecutador.ejecutarAst(resta) should equal(5)
    }

    it("se ejecuta una multiplicacion entre 2 numeros y se obtiene su resultado"){

      val multiplicacion = Multiplicacion(Numero(10), Numero(5))

      ejecutador.ejecutarAst(multiplicacion) should equal(50)
    }

    it("se ejecuta una division entre 2 numeros y se obtiene su resultado"){

      val division = Division(Numero(10), Numero(5))

      ejecutador.ejecutarAst(division) should equal(2)
    }

    it("Se ejecuta una operacion aritmetica anidada y se obtiene su resultado"){

      val div = Division(Suma(Numero(10), Numero(5)), Resta(Numero(5), Numero(2)))

      ejecutador.ejecutarAst(div) should equal(5)
    }

    it("Se ejecuta una operacion aritmetica anidada con numeros negativos y se obtiene su resultado"){

      val div = Division(Suma(Numero(10), Numero(5)), Resta(Numero(5), Numero(-4)))

      ejecutador.ejecutarAst(div) should equal(1)
    }

    it("Se ejecuta una division con cero y se obtiene un error"){

      val div = Division(Numero(10), Numero(0))

      //Una manera
      //an [ArithmeticException] should be thrownBy ejecutador.ejecutarAritmetica(div)

      the [ArithmeticException] thrownBy ejecutador.ejecutarAst(div) should have message "/ by zero"

    }
  }

  describe("Test respecto al ejecutador booleano"){

    it("se ejecuta un operacion de Igual entre numeros y se obtiene true"){

      val igual = Igual(Suma(Numero(10), Numero(40)), Division(Multiplicacion(Numero(20),Numero(5)),Numero(2)))

      ejecutador.ejecutarAst(igual) should equal(true)
    }

    it("se ejecuta un operacion de Igual entre booleanos y se obtiene false"){

      val igual = Igual(True, False)

      ejecutador.ejecutarAst(igual) should equal(false)
    }

    it("se ejecuta un operacion de Igual entre operaciones booleanas y se obtiene true"){

      val igual = Igual(MenorQue(Numero(10), Numero(40)), Distinto(Multiplicacion(Numero(20),Numero(5)),Numero(2)))

      ejecutador.ejecutarAst(igual) should equal(true)
    }

    it("se ejecuta un operacion de Distinto entre numeros y se obtiene true"){

      val distinto = Distinto(Suma(Numero(10), Numero(50)), Division(Multiplicacion(Numero(20),Numero(5)),Numero(2)))

      ejecutador.ejecutarAst(distinto) should equal(true)
    }

    it("se ejecuta un operacion de Distinto entre booleanos y se obtiene false"){

      val distinto = Distinto(True, True)

      ejecutador.ejecutarAst(distinto) should equal(false)
    }

    it("se ejecuta un operacion de Distinto entre operaciones booleanas y se obtiene true"){

      val distinto = Distinto(MenorQue(Numero(10), Numero(40)), Igual(Multiplicacion(Numero(20),Numero(5)),Numero(2)))

      ejecutador.ejecutarAst(distinto) should equal(true)
    }

    it("se ejecuta una operacion de mayorQue entre numeros y se obtiene true"){

      val mayorQue = MayorQue(Numero(3), Numero(1))

      ejecutador.ejecutarAst(mayorQue) should equal(true)

    }

    it("se ejecuta una operacion de mayorQue entre operaciones aritmeticas y se obtiene true"){

      val mayorQue = MayorQue(Suma(Numero(5), Numero(1)), Resta(Numero(10), Numero(5)))

      ejecutador.ejecutarAst(mayorQue) should equal(true)

    }

    it("se ejecuta una operacion de mayorQue entre booleanos y se obtiene un error"){

      val mayorQue = MayorQue(True, False)

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(mayorQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(mayorQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de mayorQue entre operaciones booleanas y se obtiene un error"){

      val mayorQue = MayorQue(Distinto(True, False), MenorQue(Numero(1), Numero(0)))

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(mayorQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(mayorQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de mayorIgualQue entre operaciones aritmeticas y se obtiene true"){

      val mayorIgualQue = MayorIgualQue(Suma(Numero(4), Numero(1)), Resta(Numero(10), Numero(5)))

      ejecutador.ejecutarAst(mayorIgualQue) should equal(true)

    }

    it("se ejecuta una operacion de mayorIgualQue entre booleanos y se obtiene un error"){

      val mayorIgualQue = MayorIgualQue(True, True)

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(mayorIgualQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(mayorIgualQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de mayorIgualQue entre operaciones booleanas y se obtiene un error"){

      val mayorIgualQue = MayorIgualQue(Distinto(True, False), MenorQue(Numero(1), Numero(0)))

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(mayorIgualQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(mayorIgualQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de menorQue entre numeros y se obtiene true"){

      val menorQue = MenorQue(Numero(3), Numero(4))

      ejecutador.ejecutarAst(menorQue) should equal(true)

    }

    it("se ejecuta una operacion de menorQue entre operaciones aritmeticas y se obtiene true"){

      val menorQue = MenorQue(Suma(Numero(3), Numero(1)), Resta(Numero(10), Numero(5)))

      ejecutador.ejecutarAst(menorQue) should equal(true)

    }

    it("se ejecuta una operacion de menorQue entre booleanos y se obtiene un error"){

      val menorQue = MenorQue(True, False)

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(menorQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(menorQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de menorQue entre operaciones booleanas y se obtiene un error"){

      val menorQue = MayorQue(Distinto(True, False), MenorQue(Numero(1), Numero(0)))

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(menorQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(menorQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de menorIgualQue entre operaciones aritmeticas y se obtiene true"){

      val menorIgualQue = MenorIgualQue(Suma(Numero(4), Numero(1)), Resta(Numero(10), Numero(5)))

      ejecutador.ejecutarAst(menorIgualQue) should equal(true)

    }

    it("se ejecuta una operacion de menorIgualQue entre booleanos y se obtiene un error"){

      val menorIgualQue = MenorIgualQue(True, True)

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(menorIgualQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(menorIgualQue)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta una operacion de menorIgualQue entre operaciones booleanas y se obtiene un error"){

      val menorIgualQue = MenorIgualQue(Distinto(True, False), MenorQue(Numero(1), Numero(0)))

      assertThrows[IllegalArgumentException](ejecutador.ejecutarAst(menorIgualQue))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutarAst(menorIgualQue)
      }
      error.getMessage should equal("Operacion invalida")
    }
  }

  describe("ejecutador de un programa entero"){

    it("se ejecuta un programa con operaciones aritmeticas y se obtiene su ultimo argumento"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(20)),
        Resta(Numero(2020), Variable("edad"))
      ))

      ejecutador.ejecutar(programa) should equal (2000)
    }

    it("se ejecuta un programa con operaciones booleanas y se obtiene su ultimo argumento"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(20)),
        DeclararVariable("añoActual", Numero(2020)),
        MayorQue(Variable("añoActual"), Variable("edad"))
      ))

      ejecutador.ejecutar(programa) should equal (true)
    }

    it("se ejecuta un programa con operaciones mixtas y se obtiene su ultimo argumento"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(20)),
        DeclararVariable("añoActual", Numero(2020)),
        Igual(Suma(Variable("añoActual"), Variable("edad")), Suma(Variable("edad"), Variable("añoActual")))
      ))

      ejecutador.ejecutar(programa) should equal (true)
    }

    it("se ejecuta un programa con operaciones mixtas y se obtiene un error al querer sumar un booleano"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("si", True),
        DeclararVariable("añoActual", Numero(2020)),
        Suma(Variable("añoActual"), Variable("si"))
      ))

      //Se podria crear una excepcion propia
      assertThrows[IllegalArgumentException](ejecutador.ejecutar(programa))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutar(programa)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta un programa de comparacion de variables y se obtiene false"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("declaracionAnidada", DeclararVariable("comparacion", MayorIgualQue(Numero(10), Numero(10)))),
        DeclararVariable("añoActual", Numero(2020)),
        Igual(Variable("añoActual"), Variable("declaracionAnidada"))
      ))

      ejecutador.ejecutar(programa) should equal (false)
    }

    it("se ejecuta un programa de suma de variables y se obtiene un error"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("declaracionAnidada", DeclararVariable("edad", Numero(20))),
        DeclararVariable("añoActual", Numero(2020)),
        Suma(Variable("añoActual"), Variable("declaracionAnidada"))
      ))

      assertThrows[IllegalArgumentException](ejecutador.ejecutar(programa))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutar(programa)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta un programa de comparacion de variables y se obtiene un error"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("declaracionAnidada", DeclararVariable("edad", Numero(20))),
        DeclararVariable("añoActual", Numero(2020)),
        MenorIgualQue(Variable("añoActual"), Variable("declaracionAnidada"))
      ))

      assertThrows[IllegalArgumentException](ejecutador.ejecutar(programa))
      val error = intercept[IllegalArgumentException] {
        ejecutador.ejecutar(programa)
      }
      error.getMessage should equal("Operacion invalida")
    }

    it("se ejecuta un programa con una declaracion de variable anidada y se obtiene su resultado"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("declaracionAnidada", DeclararVariable("edad", Numero(20))),
        DeclararVariable("añoActual", Numero(2020)),
        Variable("declaracionAnidada"),
        Resta(Variable("añoActual"), Variable("edad"))
      ))

      ejecutador.ejecutar(programa) should equal(2000)
    }

    it("se ejecuta un programa con una variable anidada y se obtiene su resultado"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(20)),
        DeclararVariable("edadCopia", Variable("edad")),
        Resta(Variable("edadCopia"), Variable("edad"))
      ))

      ejecutador.ejecutar(programa) should equal(0)
    }

    it("se ejecuta un programa con una variable no declarada y se obtiene un error"){

      val programa = Programa(ArrayBuffer[Operacion](
        DeclararVariable("edad", Numero(20)),
        Resta(Variable("edadCopia"), Variable("edad"))
      ))

      assertThrows[NoSuchElementException](ejecutador.ejecutar(programa))
      val error = intercept[NoSuchElementException] {
        ejecutador.ejecutar(programa)
      }
      error.getMessage should equal("La variable edadCopia no fue declarada")
    }
  }
}
