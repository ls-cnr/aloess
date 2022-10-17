package org.icar.symbolic.builder

import org.icar.symbolic.{ArgumentType, AtomCategory, AtomTerm, Axiom, BoundedIntervalArgument, ConstantArgument, ConstantTerm, DomainOntology, EnumerableArgument, MixedCategory, NumberCategory, NumberTerm, ObjectCategory, Predicate, PredicateSignature, RuleAntecedent, StringCategory, StringTerm}

class OntologyBuilder(name : String) {
  private var signatures : List[PredicateSignature] = List.empty
  private var categories: List[ObjectCategory] = List.empty
  private var axioms : List[Axiom] = List.empty

  def build() : DomainOntology = DomainOntology(name,signatures.reverse,categories.reverse,axioms.reverse)

  def atom_category(cat_name : String, atom_names : List[String]): AtomCategory = {
    val atoms : List[AtomTerm] = for (s <- atom_names) yield AtomTerm(s)
    val cat = AtomCategory(cat_name,atoms)
    categories = cat :: categories
    cat
  }

  def string_category(cat_name : String, strings : List[String]): StringCategory = {
    val atoms : List[StringTerm] = for (s <- strings) yield StringTerm(s)
    val cat = StringCategory(cat_name,atoms)
    categories = cat :: categories
    cat
  }

  def number_category(cat_name : String, double_set : List[Double]): NumberCategory = {
    val atoms : List[NumberTerm] = for (d <- double_set) yield NumberTerm(d)
    val cat = NumberCategory(cat_name,atoms)
    categories = cat :: categories
    cat
  }

  def mixed_category(cat_name : String, entries : List[ConstantTerm]): MixedCategory = {
    val cat = MixedCategory(cat_name,entries)
    categories = cat :: categories
    cat
  }

  def signature(functor : String) = new PredicateSignatureBuilder(functor,categories)

  def axiom(consequent:Predicate, rhr:RuleAntecedent): Unit = {
    axioms = Axiom(consequent, rhr) :: axioms
  }

  class PredicateSignatureBuilder(name:String,categories: List[ObjectCategory]) {
    var arg_types: List[ArgumentType] = List.empty

    def create: Unit = {
      signatures = PredicateSignature(name, arg_types.reverse) :: signatures
    }

    def with_enum_arg(cat: ObjectCategory): PredicateSignatureBuilder = {
      arg_types = EnumerableArgument(cat.name) :: arg_types
      this
    }

    //    def with_enum_arg(id : String):PredicateSignatureBuilder = {
    //      //val cat = get_category_by_name(id)
    //      arg_types = EnumerableArgument(name) :: arg_types
    //      this
    //    }
    def with_interval_arg(min: Int, max: Int): PredicateSignatureBuilder = {
      arg_types = BoundedIntervalArgument(min, max) :: arg_types
      this
    }

    def with_constant_arg(term: ConstantTerm) = {
      arg_types = ConstantArgument(term) :: arg_types
      this
    }

  }
  
}





object RunBuilder extends App {
  val b = new OntologyBuilder("prova")

  val rooms = b.atom_category("rooms",List("livingroom","kitchen","bedroom"))
  b.number_category("people_number",List(1,6,8))
  b.string_category("etichette",List("stanza di luca","stanza di emilio","stanza di ale"))

  b.signature("stanza_di").with_constant_arg(StringTerm("name")).with_enum_arg(rooms).create
  b.signature("of").with_enum_arg(rooms).with_interval_arg(1,30).create

  val product = b.build()

  println(product.categories)
  println(product.signatures)
}

