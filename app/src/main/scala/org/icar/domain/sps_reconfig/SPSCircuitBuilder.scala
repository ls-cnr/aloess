package org.icar.domain.sps_reconfig

import org.icar.symbolic.{AtomTerm, NumberTerm, Proposition, StateOfWorld}

import scala.util.Random

class SPSCircuitBuilder {

  var nodes: List[SimpleNode] = List.empty
  var loads: List[Load] = List.empty
  var generators: List[Generator] = List.empty
  var possible_failures: List[ConnectionWithFailure] = List.empty
  var switchers: List[Switcher] = List.empty
  var twowayselectors: List[TwoWaySelector] = List.empty
  var mutex_switchers : List[(Switcher,Switcher)] = List.empty

  var node_counter = 1
  var load_counter = 1
  var conn_counter = 1
  var gen_counter = 1
  var switcher_counter = 1
  var failure_counter = 1
  var node_to_electricnode : Map[Int,ElectricNode] = Map.empty
  var name_to_switcher : Map[String,Switcher] = Map.empty

  def build_from_file(file:String) : SPSCircuit = {
    reset
    val spec : CircuitSpec = SimulinkParser.load_circuit_spec(file)
    spec.loads.foreach( c=>add_load(c._1,c._2,c._3.toFloat) )
    spec.gens.foreach( c=>add_generator(c._1,c._2,c._3.toFloat) )
    spec.connections.foreach(c=>add_connection(c._1,c._2) )
    spec.switchers.foreach(c=>add_switcher(c._1,c._2,c._3) )
    spec.mutex.foreach( m=>add_mutex(m._1,m._2))

    new SPSCircuit(nodes, loads,generators, possible_failures, switchers, twowayselectors, mutex_switchers)
  }

  def sample_circuit : SPSCircuit = {
    reset
    generators = List( Generator(1,"mg1",100) )
    loads = List( Load(1,"load1",50),Load(2,"load2",50))
    nodes = List( SimpleNode(1),SimpleNode(2),SimpleNode(3),SimpleNode(4),SimpleNode(5) )
    possible_failures = List( ConnectionWithFailure(1,SimpleNode(1),SimpleNode(5)) )
    switchers = List(
      Switcher(1,"sw1",SimpleNode(3),Load(1,"load1",50)),
      Switcher(2,"sw2",SimpleNode(4),Load(2,"load2",50)),
      Switcher(3,"sw3",Generator(1,"mg1",100),SimpleNode(1)),
      Switcher(4,"sw4",Generator(1,"mg1",100),SimpleNode(2))
    )
    twowayselectors = List(
      //TwoWaySelector(1,SimpleNode(1),Generator(1,100),SimpleNode(2)),
      TwoWaySelector(1,"sel1",SimpleNode(1),SimpleNode(3),SimpleNode(2)),
      TwoWaySelector(2,"sel2",SimpleNode(5),SimpleNode(4),SimpleNode(2))
    )
    mutex_switchers = List(
      (Switcher(3,"sw3",Generator(1,"mg1",100),SimpleNode(1)), Switcher(4,"sw4",Generator(1,"mg1",100),SimpleNode(2)))
    )

    new SPSCircuit(nodes, loads,generators, possible_failures, switchers, twowayselectors, mutex_switchers)
  }

  def sample_circuit_mission = SPSMission(List("load1","load2"),List.empty,List.empty)
  def sample_circuit_initial = StateOfWorld(List(
    Proposition("on_gen",List(AtomTerm("mg1"))),
    Proposition("closed_sw",List(AtomTerm("sw2"))),
    Proposition("closed_sw",List(AtomTerm("sw3"))),
    Proposition("pos1_sel",List(AtomTerm("sel1"))),
    Proposition("pos1_sel",List(AtomTerm("sel2"))),

    Proposition("failure",List(NumberTerm(1))),

  ))

  def circuit_3_mission = {
    val vitals = List("load2","load6","load12","load16","load22","load3","load7","load9")
    val semivitals =List("load13","load17","load19","load23","load1","load4","load5")
    val nonvitals =List("load8","load11","load14","load15","load18","load21","load24")
    SPSMission(vitals,semivitals,nonvitals)
  }
  def random_mission : SPSMission = {
    val random = new Random()
    val list: List[String] = List("load2","load6","load12","load16","load22","load3","load7","load9","load13","load17","load19","load23","load1","load4","load5","load8","load11","load14","load15","load18","load21","load24")
    var vitals: List[String] = List.empty
    var semivitals: List[String] = List.empty
    var nonvitals: List[String] = List.empty
    for(l <- list) {
      val i : Int = random.nextInt(3)
      i match {
        case 0 => vitals = l :: vitals
        case 1 => semivitals = l :: semivitals
        case 2 => nonvitals = l :: nonvitals
      }
    }
    SPSMission(vitals,semivitals,nonvitals)
  }

  def circuit_3_initial_simple_failure = {
    val on_generators = List("mg1","mg2")
    val closed_switchers = List(
      "switchswmg1","switchswmg2",
      "switchsw2","switchsw6","switchsw12","switchsw16","switchsw22","switchsw23","switchsw3","switchsw7","switchsw9",
      "switchsws1","switchsws2","switchswp3","switchsws4","switchsws5",
      "switchswp6","switchsws7","switchswaux1s","switchswaux2s"
    )
    val failures = List(1)//List(1,2,3)

    val on_gen_preds = for(i<-on_generators) yield Proposition("on_gen",List(AtomTerm(i)))
    val closed_switcher_preds = for(i<-closed_switchers) yield Proposition("closed_sw",List(AtomTerm(i)))
    val failure_preds = for (i<-failures) yield Proposition("failure",List(NumberTerm(i)))

    StateOfWorld(on_gen_preds:::closed_switcher_preds:::failure_preds)
  }

  def circuit_3_initial_totally_switched_off = {
    val on_generators = List("mg1")
    val closed_switchers = List(
      "switchsw2","switchsw12","switchsw22","switchsw3","switchsw9",
      "switchsws1","switchswp2","switchsws3","switchswp4","switchsws5",
      "switchswp6","switchswp7","switchswaux1s","switchswaux2s"
    )
    val failures = List()//List(1,2,3)

    val on_gen_preds = for(i<-on_generators) yield Proposition("on_gen",List(AtomTerm(i)))
    val closed_switcher_preds = for(i<-closed_switchers) yield Proposition("closed_sw",List(AtomTerm(i)))
    val failure_preds = for (i<-failures) yield Proposition("failure",List(NumberTerm(i)))

    StateOfWorld(on_gen_preds:::closed_switcher_preds:::failure_preds)
  }

  private def reset : Unit = {
    nodes = List.empty
    loads = List.empty
    generators = List.empty
    possible_failures = List.empty
    switchers = List.empty
    twowayselectors = List.empty
    mutex_switchers = List.empty

    node_counter = 1
    load_counter = 1
    conn_counter = 1
    gen_counter = 1
    switcher_counter = 1
    failure_counter = 1
    node_to_electricnode = Map.empty
    name_to_switcher = Map.empty
  }
  private def add_load(name:String, node:Int, pow:Float): Unit = {
    val load = Load(load_counter,name,pow)
    load_counter += 1
    node_to_electricnode += (node->load)
    loads = load :: loads
  }
  private def add_generator(name:String, node:Int, pow:Float): Unit = {
    val gen = Generator(gen_counter,name,pow)
    gen_counter += 1
    node_to_electricnode += (node -> gen)
    generators = gen :: generators
  }
  private def add_connection(node1: Int, node2: Int): Unit = {
    val src_is_new = !node_to_electricnode.contains(node1)
    val dst_is_new = !node_to_electricnode.contains(node2)

    if (!src_is_new && !dst_is_new) {
      val src_node = node_to_electricnode(node1)
      val dst_node = node_to_electricnode(node2)
      possible_failures = ConnectionWithFailure(conn_counter,src_node,dst_node) :: possible_failures
      conn_counter += 1
    } else if (!src_is_new && dst_is_new ) {
      val src_node = node_to_electricnode(node1)
      node_to_electricnode += (node2 -> src_node)
    } else if (src_is_new && !dst_is_new ) {
      val dst_node = node_to_electricnode(node2)
      node_to_electricnode += (node1 -> dst_node)
    } else {
      nodes = SimpleNode(node_counter) :: nodes
      node_to_electricnode += (node1 -> SimpleNode(node_counter))
      node_to_electricnode += (node2 -> SimpleNode(node_counter))
      node_counter += 1
    }
  }
  private def add_switcher(name:String, node1: Int, node2: Int): Unit = {
    val src = node_to_electricnode(node1)
    val dst = node_to_electricnode(node2)
    if (!is_failure(name)) {
      val switcher = Switcher(switcher_counter,name,src,dst)
      switcher_counter += 1
      name_to_switcher += (name->switcher)
      switchers = switcher :: switchers
    } else {
      val failure = ConnectionWithFailure(failure_counter,src,dst)
      failure_counter += 1
      possible_failures = failure :: possible_failures
    }
  }
  private def is_failure(str: String):Boolean = {
    var r=false
    if (str.startsWith("switch")) {
      val real_name = str.substring(6)
      if (real_name.startsWith("f"))
        r=true
    }
    r
  }
  private def add_mutex(sw1: String, sw2: String): Unit = {
    val switcher1 = name_to_switcher(sw1)
    val switcher2 = name_to_switcher(sw2)
    mutex_switchers = (switcher1,switcher2) :: mutex_switchers
  }

}
