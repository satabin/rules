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

  /** A Rule */
  class Rule private[Solver] (val name: String,
    private[Solver] var code: Context => Unit,
    private var cond: Context => Boolean = _ => true) {

    // by default a rule always applies

    // this rule applies except if at least one of these applies
    private[Solver] val except = new HashSet[Rule]
    // this rule (if it applies) must be evaluated before these ones
    private[Solver] val before = new HashSet[Rule]
    // this rule (if it applies) must be evaluated after these ones
    private[Solver] val after = new HashSet[Rule]

    private[this] var _desc: Option[String] = None

    def description = _desc

    private[Solver] var visited = false
    private var lastApplies = true

    def appliesUnder(ctx: Context): Boolean = {
      if (!visited) {
        visited = true
        lastApplies = !except.exists(_.appliesUnder(ctx)) && cond(ctx)
      }
      lastApplies
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

    /** This rule must be evaluated before the other ones. */
    //    def before(rule: Rule, rest: Rule*): Rule = modify {
    //      this.before += rule
    //      this.before ++= rest
    //      this
    //    }

    /** This rule must be evaluated after the other ones. */
    //    def after(rule: Rule, rest: Rule*): Rule = modify {
    //      this.after += rule
    //      this.after ++= rest
    //      this
    //    }

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

    override def equals(other: Any) = other match {
      case that: Rule => that.name == this.name
      case _ => false
    }

    override def hashCode = name.hashCode

  }

  class LazyRule[T](name: String, create: T => Rule) {
    rules.find(_.name == name) match {
      case Some(rule) => rule
      case None =>
    }

  }

  class Rule1[T] private[Solver] (name: String,
    code: (T, Context) => Unit,
    cond: Context => Boolean = _ => true) {

  }

  // contains the rules for this solver indexed by their name
  private val rules = new HashSet[Rule]

  // indicates if the current set of rules has been initialized
  private var initialized = false

  // rules are automatically added to the rule set by default
  private var autoAdd = true

  /** Applies the given rule to the context */
  def apply(ctx: Context = new Context): Context = {

    // TODO check cycles

    // first get the applying rules (in pure mode, the context may not be modified)
    ctx.pure = true

    val applyingRules = rules.foldLeft(List[Rule]()) {
      (list, rule) =>
        if (rule.appliesUnder((ctx)))
          rule :: list
        else list
    }

    // then execute the applying rules (context may now be modified)
    ctx.pure = false

    applyingRules.foreach(_.code(ctx))

    rules.foreach(_.visited = false)

    ctx
  }

  def over[T](it: Context => Option[Iterable[T]])(create: T => Rule) = try {

    autoAdd = false

    def code(ctx: Context) {
      it(ctx) match {
        case Some(set) => set.foreach { value =>
          create(value)
        }
        case None => // do nothing
      }
    }

    val rule = new Rule(name, code)
    rules += rule

    rule
  } finally {
    autoAdd = true
  }

  def rule1[T](name: String)(code: (T, Context) => Unit) =
    new Rule1(name, code)

  /** Creates a new rule in this solver */
  def rule(name: String)(code: Context => Unit) = modify {
    if (autoAdd && rules.exists(_.name == name))
      throw new RuntimeException("Rule " + name + " already exists.")

    val rule = new Rule(name, code)

    if (autoAdd)
      rules += rule

    rule
  }

  def rule1(name: String) = new {
    def foreach[T](v: T)(code: Context => Unit) = {
      if (rules.exists(_.name == name))
        throw new RuntimeException("Rule " + name + " already exists.")
    }
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