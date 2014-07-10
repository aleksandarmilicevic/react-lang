package react.ast

//Relational spec
trait SpecRel {

  //TODO (un)prime Id

  var relation: Expr = BoolLit(true)
  var preCondition: Expr = BoolLit(true)
  var postCondition: Expr = BoolLit(true)

  def setRelation(e: Expr): this.type = {
    relation = e
    this
  }

  def setPre(e: Expr): this.type = {
    preCondition = e
    this
  }
  
  def setPost(e: Expr): this.type = {
    postCondition = e
    this
  }

}

//State based spec
trait SpecSt {

  var invariant: Expr = BoolLit(true)

  def setInv(e: Expr): this.type = {
    invariant = e
    this
  }

}
