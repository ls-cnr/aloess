package org.icar.business_process.converter

import org.icar.business_process.parser.bpmn_parser

import java.io.FileInputStream
import javax.swing.JFileChooser
import javax.swing.filechooser.FileSystemView

object BPMN2GoalFileChooserApp {
  val jfc = new JFileChooser(FileSystemView.getFileSystemView.getHomeDirectory)

  var file = ""

  val returnValue = jfc.showOpenDialog(null)

  if (returnValue == JFileChooser.APPROVE_OPTION) {
    val selectedFile = jfc.getSelectedFile
    file = selectedFile.getAbsolutePath
  }

  if (file != "") {
    val is = new FileInputStream(file)
    val Parser = new bpmn_parser(is)
    val report = Parser.fullFromInputStream
    val initial = Parser.initial_state

    println(report)
    println(initial)
  }

}

object BPMN2GoalApp extends App {
  val file = "app/src/main/resources/process/simplified_email_voting.bpmn"
  val is = new FileInputStream(file)
  val parser = new bpmn_parser(is)
  val goals = parser.goals_from_InputStream
  val initial = parser.initial_state

  goals.foreach( g => println(g) )
  println(initial)
}
