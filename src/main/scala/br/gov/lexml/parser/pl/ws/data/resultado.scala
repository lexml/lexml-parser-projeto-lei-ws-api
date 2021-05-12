package br.gov.lexml.parser.pl.ws.data

import scala.xml._
import java.net.URI
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import org.apache.commons.codec.binary.Base64

final case class ParserResultado(
                                  metadado : Metadado,
                                  //TipoResultado
                                  falhas : Vector[Falha],
                                  caracteristicas : Vector[Caracteristica],
                                  saidas : Vector[ElementoSaida],
                                  digestFonte : String,
                                  dataHoraProcessamento : ZonedDateTime,
                                  numeroDiferencas : Option[Int],
                                  //TipoConfiguracao
                                  componentes : Vector[Componente]
                                ) {
  import ParserResultado.tf
  def asXML : Elem =
    <pr:ParserResultado xmlns:pr={XMLConsts.prNamespace}>
      {metadado.asXML}
      <pr:resultado>
        <pr:falhas>{NodeSeq.fromSeq(falhas.map(_.asXML))}</pr:falhas>
        <pr:caracteristicas>{NodeSeq.fromSeq(caracteristicas.map(_.asXML))}</pr:caracteristicas>
        <pr:saidas>{NodeSeq.fromSeq(saidas.map(_.asXML))}</pr:saidas>
        <pr:digestFonte>{digestFonte}</pr:digestFonte>
        <pr:dataHoraProcessamento>{tf.format(dataHoraProcessamento)}</pr:dataHoraProcessamento>
        {NodeSeq.fromSeq(numeroDiferencas.toSeq.map(x => <pr:numeroDiferencas>{x.toString}</pr:numeroDiferencas>))}
      </pr:resultado>
      <pr:configuracao>
        {NodeSeq.fromSeq(componentes.map(_.asXML))}
      </pr:configuracao>
    </pr:ParserResultado>
}

object ParserResultado {
  val tf : DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  def fromXML(elem : Elem) : ParserResultado = {
    val metadado = (elem \ "metadado").collectFirst { case e : Elem => Metadado.fromXML(e) }.head
    val resultado = (elem \ "resultado").collectFirst { case e : Elem => e}.head
    val falhas = (resultado \ "falhas" \ "falha").collect { case e : Elem => Falha.fromXML(e) }.toVector
    val caracteristicas = (resultado \ "caracteristicas" \ "caracteristica")
        .collect { case e : Elem => Caracteristica.fromXML(e) }.toVector
    val saidas = (resultado \ "saidas" \ "elementoSaida")
      .collect { case e : Elem => ElementoSaida.fromXML(e) }.toVector
    val digestFonte = (resultado \ "digestFonte").text
    val dataHoraProcessamento = (elem \ "dataHoraProcessamento")
        .collectFirst { case e : Elem => ZonedDateTime.from(tf.parse(e.text)) }.head
    val numeroDiferencas = (resultado \ "numeroDiferencas").collectFirst { case e : Elem => e.text.toInt }
    val componentes = (elem \ "configuracao" \ "componente")
        .collect { case e : Elem => Componente.fromXML(e) }
        .toVector
    ParserResultado(
      metadado = metadado,
      falhas = falhas,
      caracteristicas = caracteristicas,
      saidas = saidas,
      digestFonte = digestFonte,
      dataHoraProcessamento = dataHoraProcessamento,
      numeroDiferencas = numeroDiferencas,
      componentes = componentes
    )
  }
}

final case class Componente(nome : String, versao : String) {
  def asXML : Elem = <pr:componente xmlns:pr={XMLConsts.prNamespace} nome={nome} versao={versao}/>
}

object Componente {
  def fromXML(elem : Elem) : Componente = {
    Componente(nome = (elem \ "@nome").text, versao = (elem \ "@versao").text)
  }
}

final case class Falha(
                      codigoTipoFalha : Int,
                      nomeTipoFalha : String,
                      posicoes : Vector[String],
                      descricao : String,
                      mensagemUsuario : Option[String]
                      ) {
  def asXML : Elem =
    <pr:falha xmlns:pr={XMLConsts.prNamespace}>
      <pr:codigoTipoFalha>{codigoTipoFalha}</pr:codigoTipoFalha>
      <pr:nomeTipoFalha>{nomeTipoFalha}</pr:nomeTipoFalha>
      {NodeSeq.fromSeq(posicoes.map(x => <pr:posicao>{x}</pr:posicao>))}
      <pr:descricao>{descricao}</pr:descricao>
      {NodeSeq.fromSeq(mensagemUsuario.map(x => <pr:mensagemUsuario>{x}</pr:mensagemUsuario>).toSeq)}
    </pr:falha>
}

object Falha {
  def fromXML(elem : Elem) : Falha = {
    val codigoTipoFalha = (elem \ "codigoTipoFalha").text.toInt
    val nomeTipoFalha = (elem \ "nomeTipoFalha").text
    val posicoes = (elem \ "posicao").collect { case e : Elem => e.text }.toVector
    val descricao = (elem \ "descricao").text
    val mensagemUsuario = (elem \ "mensagemUsuario").collectFirst { case e : Elem => e.text }
    Falha(codigoTipoFalha = codigoTipoFalha,
      nomeTipoFalha = nomeTipoFalha,
      posicoes = posicoes,
      descricao = descricao,
      mensagemUsuario = mensagemUsuario)
  }
}

final case class Caracteristica(
                               descricao : String,
                               presente : Boolean,
                               suportadoLexEdit : Boolean
                               ) {
  def asXML : Elem =
    <pr:caracteristica xmlns:pr={XMLConsts.prNamespace}>
      <pr:descricao>{descricao}</pr:descricao>
      <pr:presente>{presente}</pr:presente>
      <pr:suportadoLexEdit>{suportadoLexEdit}</pr:suportadoLexEdit>
    </pr:caracteristica>
}

object Caracteristica {
  def fromXML(elem : Elem) : Caracteristica = {
    Caracteristica(
      descricao = (elem \ "descricao").text,
      presente = (elem \ "presente").text.toBoolean,
      suportadoLexEdit = (elem \ "suportadoLexEdit").text.toBoolean,
    )
  }
}

final case class ElementoSaida(
                              tipoSaida : TipoSaida,
                              tipoMime : TipoMimeSaida,
                              href : Option[URI],
                              digest : Option[String],
                              conteudoSaida : Option[ConteudoSaida],
                              ) {
  def asXML : Elem =
    <pr:elementoSaida xmlns:pr={XMLConsts.prNamespace}
      tipoSaida={tipoSaida.value} tipoMime={tipoMime.value}
      href={href.map(_.toString).orNull}>
      {NodeSeq.fromSeq(digest.toSeq.map(x => <pr:digest>{x}</pr:digest>))}
      {NodeSeq.fromSeq(conteudoSaida.toSeq.map(x => x.asXML))}
    </pr:elementoSaida>
}

object ElementoSaida {
  def fromXML(elem : Elem) : ElementoSaida = {
    val tipoSaida = TipoSaida((elem \ "@tipoSaida").text)
    val tipoMime = TipoMimeSaida((elem \ "@tipoMimeSaida").text)
    val href = (elem \ "@href").headOption.map(x => URI.create(x.text))
    val digest = (elem \ "digest").headOption.map(x => x.text)
    val conteudoSaida = elem.child.collectFirst {
      case e : Elem if e.label == "conteudoBinario" => ConteudoBinario.fromXML(e)
      case e : Elem if e.label == "conteudoXML" => ConteudoXML.fromXML(e)
    }
    ElementoSaida(tipoSaida = tipoSaida,
      tipoMime = tipoMime, href = href, digest = digest,
      conteudoSaida = conteudoSaida )
  }
}

abstract sealed class ConteudoSaida extends Product {
  def asXML : Elem
}

object ConteudoSaida {
  def fromXML(elem : Elem) : ConteudoSaida =
    elem.label match {
      case "conteudoBinario" => ConteudoBinario.fromXML(elem)
      case "conteudoXML" => ConteudoXML.fromXML(elem)
      case _ => throw new RuntimeException(s"contedo de saída não esperado: ${elem.label}")
    }
}

final case class ConteudoBinario(contents : Array[Byte]) extends ConteudoSaida {
  def asXML : Elem =
    <pr:conteudoBinario xmlns:pr={XMLConsts.prNamespace}>{ConteudoBinario.b64.encodeAsString(contents)}</pr:conteudoBinario>
}

object ConteudoBinario {
  val b64 : Base64 = new Base64()
  def fromXML(elem : Elem) : ConteudoBinario =
    ConteudoBinario(b64.decode(elem.text))
}

final case class ConteudoXML(contents : Elem) extends ConteudoSaida {
  def asXML : Elem =
    <pr:conteudoXML xmlns:pr={XMLConsts.prNamespace}>{contents}</pr:conteudoXML>
}

object ConteudoXML {
  def fromXML(elem : Elem) : ConteudoXML =
    ConteudoXML(
      elem.child.collectFirst { case e : Elem => e }
        .getOrElse(throw new RuntimeException("Elemento ausente em ConteudoXML")))
}

abstract sealed class TipoMimeSaida(val value : String) extends Product {
  override def toString() : String = value
}

object TipoMimeSaida {
  val tipoMimeSaidaValues : Map[String,TipoMimeSaida] = Map(
    ("application/msword", TMS_MSWord),
    ("text/rtf", TMS_RTF),
    ("text/html", TMS_HTML),
    ("application/xhtml+xml", TMS_XHTML),
    ("application/vnd.oasis.opendocument.text", TMS_ODF),
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document", TMS_DOCX),
    ("text/xml", TMS_XML),
    ("application/zip", TMS_ZIP),
    ("application/pdf", TMS_PDF),
    ("text/plain", TMS_PLAIN),
    ("application/epub+zip", TMS_EPUB)
  )
  def apply(value : String) : TipoMimeSaida =
    tipoMimeSaidaValues.getOrElse(value, throw new RuntimeException(s"tipo de MIME de saída não esperado: $value"))
}

case object TMS_MSWord extends TipoMimeSaida("application/msword")
case object TMS_RTF extends TipoMimeSaida("text/rtf")
case object TMS_HTML extends TipoMimeSaida("text/html")
case object TMS_XHTML extends TipoMimeSaida("application/xhtml+xml")
case object TMS_ODF extends TipoMimeSaida("application/vnd.oasis.opendocument.text")
case object TMS_DOCX extends TipoMimeSaida("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
case object TMS_XML extends TipoMimeSaida("text/xml")
case object TMS_ZIP extends TipoMimeSaida("application/zip")
case object TMS_PDF extends TipoMimeSaida("application/pdf")
case object TMS_PLAIN extends TipoMimeSaida("text/plain")
case object TMS_EPUB extends TipoMimeSaida("application/epub+zip")

