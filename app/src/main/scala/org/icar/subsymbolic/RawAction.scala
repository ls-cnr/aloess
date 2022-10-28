package org.icar.subsymbolic

case class RawAction(id:Int,pre:RawLogicFormula,effects:List[RawEvolution],invariant:RawLogicFormula)

case class RawEvolution(name : String, evo : List[RawEvoOperator], probability : Double)

abstract class RawEvoOperator
case class RawAdd(add : RawProposition) extends RawEvoOperator
case class RawRem(rmv : RawProposition) extends RawEvoOperator
