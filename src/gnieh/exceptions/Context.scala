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
  }

  def apply[T: Manifest](key: String) = {
    
  }
}