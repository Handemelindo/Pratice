object SessionType {
  trait Message
  trait !:[A, R]
  trait ?:[A, S]
  trait +:[R, S]
  trait *:[R, S]
  trait ~=[R, S]
  implicit val eps: Message ~= Message = new (Message ~= Message) {}

  // basic duality rules
  implicit def flip[R, S](implicit evd: R ~= S): S ~= R = new (S ~= R) {}
  implicit def send[R, S, A](implicit evd: R ~= S): (A !: R) ~= (A ?: S) = new ((A !: R) ~= (A ?: S)) {}
  implicit def recv[R, S, A](implicit evd: R ~= S): (A ?: R) ~= (A !: S) = new ((A ?: R) ~= (A !: S)) {}
  implicit def choose[R, S, R2, S2](implicit evd1: R ~= S, evd2: R2 ~= S2): (R +: R2) ~= (S *: S2) =
    new ((R +: R2) ~= (S *: S2)) {}
  implicit def offer[R, S, R2, S2](implicit evd: R ~= S, evd2: R2 ~= S2): (R *: R2) ~= (S +: S2) =
    new ((R *: R2) ~= (S +: S2)) {}
}

class Session[S, T, A] private (a: => A) {
  private lazy val action = a

  def flatMap[U, B](next: A => Session[T, U, B]): Session[S, U, B] = Session(next(a).action)

  def map[B](f: A => B): Session[S, T, B] = new Session(f(action))
}

object Session {
  type Server[A] = Session[A, Unit, Unit]
  type Client[A, B] = Session[A, Unit, B]
  import SessionType._
  def apply[S, T, A](a: => A): Session[S, T, A] = Session(a)
  def unit[A, S](a: A): Session[S, S, A] = Session(a)
  def send[A, S](a: A): Session[A !: S, S, Unit] = Session(())
  def ![A, S](a: A): Session[A !: S, S, Unit] = send(a)
  def ?[A, S]: Session[A ?: S, S, A] = Session(???)
  def +[R, S, U, A](left: Session[R, U, A], right: Session[S, U, A]): Session[R +: S, U, A] = Session(left.action)
  val close: Session[Message, Unit, Unit] = Session(println("closed"))
  def run[R, S, A](r: Session[R, Unit, Unit], s: Session[S, Unit, A])(implicit evd: R ~= S): A = {
    r.action; s.action
  }

  def mockRecv[A, S](a: A): Session[A ?: S, S, A] = new Session(a)
  def sel1[R, S]: Session[R +: S, R, Unit] = new Session(())
  def sel2[R, S]: Session[R +: S, S, Unit] = new Session(())
}

object Test {
  def main(args: Array[String]): Unit = {
    import SessionType._

    type Proto1 = Int ?: String !: Message
    type Proto2 = Int !: String ?: Message

//    val server: Session.Server[Proto1] = for {
//      i <- Session.?
//      _ <- Session ! i.toString
//      z <- Session.close
//    } yield z
//
//    def client(i: Int): Session[Proto2, Unit, String] = for {
//      _ <- Session ! i
//      s <- Session.?
//      _ <- Session.close
//    } yield s

    val server: Session[Proto1, Unit, Unit] = for {
      i <- Session.mockRecv(8)
      _ <- Session ! i.toString
      z <- Session.close
    } yield z

    def client(i: Int): Session[Proto2, Unit, String] = for {
      _ <- Session ! i
      s <- Session.mockRecv("8")
      _ <- Session.close
    } yield s

    println(Session.run(server, client(8)))
  }
}
