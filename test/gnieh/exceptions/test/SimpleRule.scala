package gnieh.exceptions
package test

object SimpleRule extends App {

  val solver = new Solver

  import solver._

  val rule1 = rule("rule1") {
    ctx =>
      ctx("toto") = 10
      println("rule1")
  }

  val rule2 =
    rule("rule2") {
      ctx => println("rule2")
    } when {
      _[Int]("toto").isDefined
    }

  val ctx = new Context

  solver(ctx)

  println("==========")

  solver(ctx)

  println("==========")

  rule2 amends rule1

  solver(ctx)

  println("==========")

  ctx.remove("toto")

  solver(ctx)

  println("==========")

}