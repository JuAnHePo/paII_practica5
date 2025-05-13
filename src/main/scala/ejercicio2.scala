class Lago {
  @volatile private var nivelAgua = 0
  @volatile var turno: Array[Int] = new Array[Int](4)
  @volatile var pidiendoTurno: Array[Boolean] = new Array[Boolean](4)

  private def meToca(id: Int, i: Int): Boolean = {
    if turno(id) > 0 && turno(id) < turno(i) then false
    else if turno(id) == turno(i) && i < id then false
    else true
  }

  private def cogeTurno(id: Int): Unit = {
    pidiendoTurno(id) = true
    turno(id) = turno.max + 1
    pidiendoTurno(id) = false
  }

  private def esperaTurno(id: Int): Unit = {
    for(i <- turno.indices)
      while(pidiendoTurno(i)) Thread.`yield`()
      while(!meToca(id, i)) Thread.`yield`()
  }

  private def saleTurno(id: Int): Unit = {
    turno(id) = 0
  }

  private def lock(id: Int): Unit = { cogeTurno(id); esperaTurno(id) }

  private def unLock(id: Int): Unit = saleTurno(id)

  def incrementa(id: Int): Unit = {
    lock(id)

    nivelAgua += 1

    unLock(id)
  }

  def decrementa(id: Int): Unit = {
    lock(id)
    while nivelAgua == 0 do
      unLock(id)
      lock(id)


    nivelAgua -= 1


    unLock(id)
  }

  def getNivelAgua: Int = nivelAgua
}

class Rio(lago: Lago, id: Int) extends Thread {
  override def run(): Unit =
    this.setName(s"rio-$id")
    for (i <- 0 until 1000)
      lago.incrementa(id)
      println(s"${this.getName}: el nivel del agua es ${lago.getNivelAgua}")
}

class Presa(lago: Lago, id: Int) extends Thread {
  override def run(): Unit =
    this.setName(s"presa-$id")
    for (i <- 0 until 1000)
      lago.decrementa(id)
      println(s"${this.getName}: el nivel del agua es ${lago.getNivelAgua}")
}

@main def mainEjercicio2(): Unit =
  val lago = Lago()
  val r0 = Rio(lago, 0)
  val r1 = Rio(lago, 1)
  val p0 = Presa(lago, 0)
  val p1 = Presa(lago, 1)

  r0.start(); r1.start(); p0.start(); p1.start()