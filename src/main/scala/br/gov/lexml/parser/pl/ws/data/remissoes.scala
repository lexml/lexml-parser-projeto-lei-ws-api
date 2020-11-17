package br.gov.lexml.parser.pl.ws.data

import scala.xml._
import java.net.URI

final case class Remissoes(documentos : Vector[RemissaoDocumento]) {
  lazy val asXML : Elem =
    <rs:Remissoes xmlns:rs={XMLConsts.rsNamespace}>
      {NodeSeq.fromSeq(documentos.map(_.asXML))}
    </rs:Remissoes>
}

object RemissaoConsts {
  val URN = s"@{${XMLConsts.rsNamespace}urn"
  val DISPLAY = s"@{${XMLConsts.rsNamespace}display"
  val HREF = s"${XMLConsts.xlinkNamespace}href"
  val TYPE = s"${XMLConsts.xlinkNamespace}type"
}

final case class RemissaoDocumento(
                               urn : URI,
                               display : String,
                               href : URI,
                               linkType : XlinkType,
                               fragmentos: Vector[RemissaoFragmento]
                             ) {
  def asXML : Elem  =
    <rs:documento xmlns:rs={XMLConsts.rsNamespace} xmlns:xlink={XMLConsts.xlinkNamespace}
                  rs:urn={urn.toString} rs:display={display} xlink:href={href.toString} xlink:type={linkType.value}>
      {NodeSeq.fromSeq(fragmentos.map(_.asXML))}
      </rs:documento>
}

object RemissaoDocumento {
  import RemissaoConsts._
  def fromElem(elem : Elem) : RemissaoDocumento = {
    val urn = URI.create((elem \ URN).text)
    val display = (elem \ DISPLAY).text
    val href = URI.create((elem \ HREF).text)
    val linkType = XlinkType((elem \ TYPE).text)
    val fragmentos = (elem \ "fragmento").collect {
      case x : Elem => RemissaoFragmento.fromElem(x)
    }.toVector
    RemissaoDocumento(urn = urn, display = display, href = href, linkType = linkType, fragmentos = fragmentos)
  }
}

abstract sealed class XlinkType(val value : String) extends Product {
  override def toString() : String = value
}

object XlinkType {
  def apply(value : String) : XlinkType = value match {
    case "simple" => XT_Simple
    case "extended" => XT_Extended
    case "title" => XT_Title
    case "resource" => XT_Resource
    case "locator" => XT_Locator
    case "arc" => XT_Arc
    case x => throw new RuntimeException(s"Unexpected xlink type: $x")
  }
}

case object XT_Simple extends XlinkType("simple")
case object XT_Extended extends XlinkType("extended")
case object XT_Title extends XlinkType("title")
case object XT_Resource extends XlinkType("resource")
case object XT_Locator extends XlinkType("locator")
case object XT_Arc extends XlinkType("arc")


final case class RemissaoFragmento(
                               urn : URI,
                               display : String,
                               href : URI,
                               linkType : XlinkType
                             ) {
  def asXML: Elem =
    <rs:fragmento xmlns:rs={XMLConsts.rsNamespace} xmlns:xlink={XMLConsts.xlinkNamespace}
                  rs:urn={urn.toString} rs:display={display} xlink:href={href.toString} xlink:type={linkType.value}/>
}

object RemissaoFragmento {
  import RemissaoConsts._
  def fromElem(elem : Elem) : RemissaoFragmento = {
    val urn = URI.create((elem \ URN).text)
    val display = (elem \ DISPLAY).text
    val href = URI.create((elem \ HREF).text)
    val linkType = XlinkType((elem \ TYPE).text)
    RemissaoFragmento(urn = urn, display = display, href = href, linkType = linkType)
  }
}

