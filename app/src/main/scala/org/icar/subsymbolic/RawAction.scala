package org.icar.subsymbolic

case class RawAction(id:String,pre:RawLogicFormula,effects:Array[RawEvolution],invariants:List[RawLogicFormula])

case class RawEvolution(name : String, probability : Float, evo : Array[RawEvoOperator])

abstract class RawEvoOperator
case class RawAdd(add : RawProposition) extends RawEvoOperator
case class RawRem(rmv : RawProposition) extends RawEvoOperator
