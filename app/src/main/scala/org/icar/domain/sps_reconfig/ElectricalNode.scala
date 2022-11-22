package org.icar.domain.sps_reconfig

import org.icar.symbolic.{AtomTerm, NumberTerm, Proposition}

abstract class ElectricNode {
  def up_condition : Proposition
  def node_string : String
}
case class SimpleNode(id:Int) extends ElectricNode {
  override def up_condition: Proposition = Proposition("up_node",List(NumberTerm(id)))
  override def node_string: String = s"N$id"
}
case class Load(id : Int,name:String,pow:Float) extends ElectricNode {
  override def up_condition: Proposition = Proposition("up_load",List(AtomTerm(name)))
  override def node_string: String = s"L$name"
}
case class Generator(id : Int,name:String,pow:Float) extends ElectricNode {
  override def up_condition: Proposition = Proposition("on_gen",List(AtomTerm(name)))
  override def node_string: String = s"G$name"
}

case class Switcher(id: Int, name:String, source : ElectricNode, dest : ElectricNode) {
  def closed_predicate : Proposition = Proposition("closed_sw",List(AtomTerm(name)))
}

case class TwoWaySelector(id: Int, name:String, pos1 : ElectricNode, middle : ElectricNode, pos2 : ElectricNode) {
  def pos1_predicate : Proposition = Proposition("pos1_sel",List(AtomTerm(name)))
  def pos2_predicate : Proposition = Proposition("pos2_sel",List(AtomTerm(name)))
}
case class ConnectionWithFailure(id:Int, source : ElectricNode, dest : ElectricNode) {
  def fired_predicate : Proposition = Proposition("failure",List(NumberTerm(id)))
}