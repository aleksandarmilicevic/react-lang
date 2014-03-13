package react.ast

//package.class.fun encoded by List(fun, class, package)
class Id(path: List[String]) {
  def addPrefix(prefix: String) = Id(prefix :: path)
}

object Id {
  def apply(path: List[String]) = new Id(path)
  def apply(path: String) = new Id(List(path))
}

