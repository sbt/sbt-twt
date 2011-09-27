package sbttwt

import java.io.File
import scala.collection.mutable
import scala.xml.NodeSeq

class Config(fileName: File) {
  val sections = mutable.Map[String, mutable.Map[String, String]]()
  
  def section(k: String): mutable.Map[String, String] =
    sections getOrElseUpdate (k, mutable.Map[String, String]())
  
  def write() {
    val writer = new java.io.FileWriter(fileName)
    writer write (toNode.toString)
    writer.close
  }
  
  def toNode: NodeSeq = {
    def kvToNode(k: String, v: String): NodeSeq = <value name={k}>{v}</value>
    def sectionToNode(k: String): NodeSeq =
      <section name={k}>{ section(k).toSeq map { case (k, v) => kvToNode(k, v) } }</section>
    
    val xml: NodeSeq = <config>{ sections.toSeq map { case (k, v) => sectionToNode(k) } }</config>
    xml
  }
  
  def fromNode(node: NodeSeq) {
    for {
      s <- node \ "section"
      sn <- s \ "@name"
      v <- s \ "value"
      k <- v \ "@name"
    } section(sn.text)(k.text) = v.text
  }
  
  if (!fileName.exists()) write()
  
  fromNode(scala.xml.XML.loadFile(fileName))
}
