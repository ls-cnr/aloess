package org.icar.domain.sps_reconfig

import org.icar.solver.DomainMetric
import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.subsymbolic.{RawProposition, RawState}

case class ForceFieldLayer(map:Map[RawProposition,Float])

class ForceField(circuit : SPSCircuit, mission : SPSMission) extends DomainMetric {

  private val map = new SubLogicBuilder(circuit.onto)

  var layers : List[ForceFieldLayer] = List.empty

  for (vital_id<-mission.vitals) {
    val vital = circuit.getLoadByName(vital_id)
    if (vital.isDefined){
      layers = ForceFieldLayer.build_by_load(circuit,vital.get,map) :: layers
    }
  }

  private def qos(w:RawState ) : Float = {
    var sum : Float = 0

    for (layer <- layers)
      for (key <- layer.map.keys) {
        if (w.satisfies(key.index))
          sum += layer.map(key)
      }


    sum
  }

  override def evaluate_state(state: RawState): Double = qos(state)

  override def max: Double = layers.size * layers.head.map.size    // ???

  override def min: Double = 0
}


object ForceFieldLayer {

  def build_by_load(circuit : SPSCircuit , focus : Load, map:SubLogicBuilder) : ForceFieldLayer = {
    var force_field : Map[RawProposition,Float] = Map.empty

    explore(focus,1)

    def explore(node : ElectricNode, value : Float) : Unit = {
      val pred = node.up_condition
      val v = RawProposition( map.direct(pred) )
      if (!force_field.contains(v) || force_field(v)<value) {

        force_field += (v -> value)

        val successors = get_successors(node)
        for (s <- successors)
          explore(s,value/2)

      }
    }


    def get_successors(node : ElectricNode) : List[ElectricNode] = {
      var next : List[ElectricNode] = List.empty

      for (s <- circuit.switchers)
        if (s.source==node)
          next = s.dest :: next
        else if (s.dest==node)
          next = s.source :: next

      for (c<-circuit.possible_failures)
        if (c.source==node)
          next = c.dest :: next
        else if (c.dest==node)
          next = c.source :: next

      next
    }

    ForceFieldLayer(force_field)
  }

  def merge_layers_by_sum(array:Array[ForceFieldLayer]) : ForceFieldLayer = {
    var merged_map:Map[RawProposition,Float]=Map.empty

    if (array.length>0) {
      val nodes = array(1).map.keys

      for (n<-nodes) {
        var sum:Float=0
        for (f<-array)
          sum+=f.map(n)

        merged_map += (n->sum)
      }
    }
    ForceFieldLayer(merged_map)
  }

  def merge_layers_by_max(array:Array[ForceFieldLayer]) : ForceFieldLayer = {
    var merged_map:Map[RawProposition,Float]=Map.empty

    if (array.length>0) {
      val nodes = array(1).map.keys

      for (n<-nodes) {
        var max:Float=0
        for (f<-array)
          if (f.map(n) > max)
            max = f.map(n)

        merged_map += (n->max)
      }
    }
    ForceFieldLayer(merged_map)
  }
}
