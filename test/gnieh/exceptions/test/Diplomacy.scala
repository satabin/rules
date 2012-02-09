package gnieh.exceptions
package test

class Diplomacy {

  val solver = new Solver("diplomacy")

  import solver._

  lazy val move = paramRule[Move]("move") { (move, ctx) =>
    println(move)
  } over {
    _[Iterable[Move]]("moves", Nil)
  } unless (standoff1(_), standoff2(_))

  lazy val standoff1 = paramRule[Move]("standoff1") { (move, ctx) =>
    val f = force(move, ctx[Set[Support]]("supports", Set.empty[Support]))
  }

  lazy val standoff2 = paramRule[Move]("standoff2") { (move, ctx) =>

  }

  def force(move: Move, supports: Set[Support]) = supports.foldLeft(1) {
    (force, support) =>
      if (support.move == move)
        force + 1
      else
        force
  }

}

trait Order
final case class Move(unit: String, from: String, to: String) extends Order {
  override def toString = "move " + unit + " from " + from + " to " + to
}
final case class Support(unit: String, in: String, move: Move) extends Order {
  override def toString = unit + " in " + in + " supports " + move
}