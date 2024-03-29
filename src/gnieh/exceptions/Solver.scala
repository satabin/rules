package gnieh.exceptions

import scala.collection.mutable.HashSet

object Solver {
  private var id = 0

  def nextId = {
    val res = id
    id += 1
    res
  }
}

/** A Solver contains a set of Rules and Exceptions */
class Solver(val name: String) {
  self =>

  def this() = this("solver$" + Solver.nextId)

  abstract class AbstractRule[ThisType <: AbstractRule[ThisType]](val name: String) {

    this: ThisType =>
    
    private[Solver] var visited: Boolean

    private[this] var _desc: Option[String] = None

    def description = _desc

    /** Sets the optional description of this rule. */
    def description(desc: String): ThisType = {
      _desc = Option(desc)
      this
    }

    override def equals(other: Any) = other match {
      case that: AbstractRule[_] => that.name == this.name
      case _ => false
    }

    override def hashCode = name.hashCode
  }

  /** A Rule without any parameter */
  class Rule private[Solver] (name: String,
    private[Solver] val code: Context => Unit) extends AbstractRule[Rule](name) {

    // by default a rule always applies
    private[Solver] var cond = (_: Context) => true

    // this rule applies except if at least one of these applies
    private[Solver] val except = new HashSet[Rule]

    private[Solver] var visited = false
    private var lastApplies = true

    def appliesUnder(ctx: Context): Boolean = {
      if (!visited) {
        visited = true
        lastApplies = !except.exists(_.appliesUnder(ctx)) && cond(ctx)
      }
      lastApplies
    }

    /**
     * Adds a condition to specify when this rule can be evaluated.
     * Once the rules are linearized, the condition is evaluated when
     * resolving if depending rules applies. So it is evaluated before any
     * rule code is evaluated.
     */
    def when(cond: Context => Boolean) = modify {
      this.cond = cond
      this
    }

    /** This rule applies, unless the given rule applies. */
    def unless(rule: Rule, rest: Rule*) = modify {
      except += rule
      except ++= rest
      this
    }

    /** This rule amends some other rules, i.e. if this rule applies, the other ones do not apply. */
    def amends(rule: Rule, rest: Rule*) = modify {
      rule.except += this
      rest.foreach(_.except += this)
      this
    }

  }

  /** A ParameterRule allows to define a rule over a collection of values */
  class ParamRule[T] private[Solver] (name: String,
    private[Solver] val code: (T, Context) => Unit) extends AbstractRule[ParamRule[T]](name) {

    type MyType = T

    // by default a rule always applies
    private[Solver] var cond = (_: MyType, _: Context) => true

    private val except = new HashSet[MyType => Rule]

    private[Solver] var visited = false
    private var lastApplies = true

    def appliesUnder(value: MyType)(ctx: Context): Boolean = {
      if (!visited) {
        visited = true
        lastApplies = !except.exists(_(value).appliesUnder(ctx)) && cond(value, ctx)
      }
      lastApplies
    }

    private[Solver] var collection: Context => Iterable[MyType] = (_: Context) => Nil

    def apply(value: MyType) = 
      new Rule(name + " with parameter: " + value, (ctx: Context) => code(value, ctx))

    def over(coll: Context => Iterable[T]) = modify {
      collection = coll
      this
    }

    def unless(rule: MyType => Rule, rest: MyType => Rule*) = modify {
      except += rule
      except ++= rest
      this
    }

    /**
     * Adds a condition to specify when this rule can be evaluated.
     * Once the rules are linearized, the condition is evaluated when
     * resolving if depending rules applies. So it is evaluated before any
     * rule code is evaluated.
     */
    def when(cond: (MyType, Context) => Boolean) = modify {
      this.cond = cond
      this
    }

  }

  // contains the rules for this solver indexed by their name
  private val rules = new HashSet[AbstractRule[_]]

  // indicates if the current set of rules has been initialized
  private var initialized = false

  /** Applies the given rule to the context */
  def apply(ctx: Context = new Context): Context = {

    // TODO check cycles in constraints!

    // first get the applying rules (in pure mode, the context may not be modified)
    ctx.pure = true

    val applyingRules = rules.foldLeft(List[Rule]()) {
      (list, rule) =>
        rule match {
          case r: Rule if r.appliesUnder(ctx) =>
            r :: list
          case p: ParamRule[_] =>
            p.collection(ctx).foldLeft(list) { (res, value) =>
              if (p.appliesUnder(value)(ctx))
                p(value) :: res
              else
                res
            }
          case _ => list
        }
    }

    // then execute the applying rules (context may now be modified)
    ctx.pure = false

    applyingRules.foreach(_.code(ctx))

    rules.foreach(_.visited = false)

    ctx
  }

  /** Creates a new parameterized rule */
  def paramRule[T](name: String)(code: (T, Context) => Unit) = {

    if (rules.exists(_.name == name))
      throw new RuntimeException("Rule " + name + " already exists.")

    val rule = new ParamRule(name, code)

    rules += rule

    rule
  }

  /** Creates a new rule in this solver */
  def rule(name: String)(code: Context => Unit) = modify {
    if (rules.exists(_.name == name))
      throw new RuntimeException("Rule " + name + " already exists.")

    val rule = new Rule(name, code)

    rules += rule

    rule
  }

  // private methods

  /* wraps a block returning some value, and mark this solver as not initialized
     * right after the block has been executed successfully.
     */
  private def modify[T](block: => T) = {
    val ret = block
    initialized = false
    ret
  }

}