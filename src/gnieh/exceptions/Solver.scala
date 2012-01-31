package gnieh.exceptions

import scala.collection.mutable.{ ListBuffer, Map }

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

  /** A Rule */
  class Rule private[Solver] (val name: String,
    private[Solver] val code: Context => Unit,
    private var cond: Context => Boolean = _ => true) {

    // by default a rule always applies

    // this rule applies except if at least one of these applies
    private val except = new ListBuffer[Rule]
    // this rule (if it applies) must be evaluated before these ones
    private val before = new ListBuffer[Rule]
    // this rule (if it applies) must be evaluated after these ones
    private val after = new ListBuffer[Rule]

    private[this] var _desc: Option[String] = None

    def description = _desc

    def appliesUnder(ctx: Context): Boolean =
      cond(ctx) && !except.exists(_.appliesUnder(ctx))

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

    /** This rule must be evaluated before the other ones. */
    def before(rule: Rule, rest: Rule*): Rule = modify {
      this.before += rule
      this.before ++= rest
      this
    }

    /** This rule must be evaluated after the other ones. */
    def after(rule: Rule, rest: Rule*): Rule = modify {
      this.after += rule
      this.after ++= rest
      this
    }

    /**
     * Adds a condition to specify when this rule can be evaluated.
     * Once the rules are linearized, the condition is evaluated when
     * resolving if depending rules applies. So it is evaluated before any
     * rule code is evaluated.
     */
    def when(cond: Context => Boolean): Rule = modify {
      this.cond = cond
      this
    }

    /** Sets the optional description of this rule. */
    def description(desc: String): Rule = {
      _desc = Option(desc)
      this
    }

    // private methods

  }

  // contains the rules for this solver indexed by their name
  private val rules = Map.empty[String, Rule]

  // indicates if the current set of rules has been initialized
  private var initialized = false

  // the linearized rule set
  private var orderedRules: List[Rule] = Nil

  /** Applies the given rule to the context */
  def apply(rule: Rule)(ctx: Context = new Context) {

    orderedRules.foreach { rule =>
      if (rule.appliesUnder(ctx))
        rule.code(ctx)
    }
  }

  /** Creates a new rule in this solver */
  def rule(name: String)(code: Context => Unit) = modify {
    if (rules.contains(name))
      throw new RuntimeException("Rule " + name + " already exists.")

    val rule = new Rule(name, code)
    rules(name) = rule

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

  /*
   * initializes the solver. It means:
   * - linearizes the order in which the rules will be evaluated
   */
  private def initialize {
    // a rule depends on another rule if it is linked with a before, after or except relationship
  }

}