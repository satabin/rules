package gnieh.exceptions

import scala.reflect.Manifest
import scala.collection.mutable.Map

class Context {
  private object variantMap {
    private var _map = Map.empty[String, (Manifest[_], Any)]

    def update[T: Manifest](name: String, item: T) {
      _map(name) = (manifest[T], item)
    }

    def apply[T: Manifest](key: String) = _map.get(key) match {
      case Some((om: Manifest[_], s: Any)) if om <:< manifest[T] =>
        Some(s.asInstanceOf[T])
      case _ => None
    }

    def remove(key: String): Unit = _map.remove(key)
    
    def contains(key: String) = _map.contains(key)
  }

  private[exceptions] var pure = true

  def apply[T: Manifest](key: String) =
    variantMap(key)

  def apply[T: Manifest](key: String, default: => T): T = apply(key) match {
    case Some(v) => v
    case None => default
  }

  def update[T: Manifest](key: String, value: T) =
    if (pure)
      throw new RuntimeException("You are not allowed to modify the context in pure mode")
    else
      variantMap(key) = value

  def remove(key: String) = if (pure)
    throw new RuntimeException("You are not allowed to modify the context in pure mode")
  else
    variantMap.remove(key)
    
  def contains(key: String) = variantMap.contains(key)
}