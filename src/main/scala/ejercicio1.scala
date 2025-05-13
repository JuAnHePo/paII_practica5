val N = 20
val TOTAL_ENTEROS = 200

class BufferCircular {
  private val elem = Array[Int](N)
  private var p = 0
  private var c = 0
  private var nelem = 0

  @volatile var turno = 0
  @volatile var t0 = false
  @volatile var t1 = false

  private def preProt0(): Unit = {
    t0 = true
    turno = 1
    while(t1 && turno == 1) Thread.`yield`()
  }

  private def postProt0(): Unit = t0 = false

  private def preProt1(): Unit = {
    t1 = true
    turno = 0
    while (t0 && turno == 0) Thread.`yield`()
  }

  private def postProt1(): Unit = t1 = false

  private def bufferLleno(): Boolean = nelem == elem.length

  private def hayDato(): Boolean = nelem > 0

  def almacena(dato: Int): Unit = {
    while(bufferLleno()) Thread.`yield`()

    preProt0()

    elem(p) = dato
    p = (p + 1) % elem.length
    nelem += 1

    postProt0()
  }

  def extrae(): Int = {
    while(!hayDato()) Thread.`yield`()

    preProt1()

    val n = elem(c)
    c = (c + 1) % elem.length
    nelem -= 1

    postProt1()

    n
  }
}

class Productor(bufferCircular: BufferCircular, nIter: Int) extends Thread {
  private val list: List[Int] = List.range(0, TOTAL_ENTEROS)

  override def run(): Unit = {
    for(i <- 0 until nIter) {
      println(s"Productor: ${list(i)}")
      bufferCircular.almacena(list(i))
    }
  }
}

class Consumidor(bufferCircular: BufferCircular, nIter: Int) extends Thread {
  private var list: List[Int] = Nil

  override def run(): Unit = {
    for(i <- 0 until nIter) {
      val dato: Int = bufferCircular.extrae()
      println(s"Consumidor: $dato")
      list = list :+ dato
    }
  }
}

@main def mainEjercicio1(): Unit =
  val nIter = 30
  val bufferCircular = BufferCircular()
  val productor = Productor(bufferCircular, nIter)
  val consumidor = Consumidor(bufferCircular, nIter)

  println("Inicia {")

  productor.start(); consumidor.start()
  productor.join(); consumidor.join()

  println("} Finaliza")