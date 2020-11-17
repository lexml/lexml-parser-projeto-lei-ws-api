package br.gov.lexml.parser.pl.ws.data

import java.net.URI
import scala.xml._
import org.apache.commons.codec.binary.Base64

final case class ParserRequisicao(
                                 metadado : Metadado,
                                 // TipoTexto:
                                 tipoMime : MimeEntrada,
                                 textoEntrada : TextoEntrada,
                                 // TipoTiposDeSaidas
                                 saidas : Vector[Saida],

                                 opcoes : Option[OpcoesRequisicao]
                                 ) {
  def asXML : Elem = {
    <pr:ParserRequisicao xmlns:pr={XMLConsts.prNamespace}>
      {metadado.asXML}
      <pr:texto>
        <pr:tipoMime>{tipoMime.value}</pr:tipoMime>
        {textoEntrada.asXML}
      </pr:texto>
      <pr:saidas>
        {NodeSeq.fromSeq(saidas.map(_.asXML))}
      </pr:saidas>
      {NodeSeq.fromSeq(opcoes.map(_.asXML).toSeq)}
    </pr:ParserRequisicao>
  }
}

object ParserRequisicao {
  def fromXML(elem : Elem) : ParserRequisicao = {
    val metadado = Metadado.fromXML((elem \ "metadado").head.asInstanceOf[Elem])
    val tipoMime = MimeEntrada((elem \ "texto" \ "tipoMime").text)
    val textoEntrada =
        elem.child.collect { case e : Elem if e.label != "metadado" => TextoEntrada.fromXML(e) }.head
    val saidas = (elem \ "saidas" \ "saida").collect { case e : Elem => Saida.fromXML(e)}.toVector
    val opcoes = (elem \ "opcoes").collectFirst { case e : Elem => OpcoesRequisicao.fromXML(e)}
    ParserRequisicao(metadado = metadado, tipoMime = tipoMime, textoEntrada = textoEntrada,
      saidas = saidas, opcoes = opcoes)
  }
}

final case class Saida(tipo : TipoSaida, formato : FormatoSaida = FS_EXTERNO) { //TipoTipoDeSaida
  def asXML: Elem =
    <pr:tipoSaida xmlns:pr={XMLConsts.prNamespace} tipo={tipo.value} formato={formato.value}/>
}

object Saida {
  def fromXML(elem : Elem) : Saida = {
    val tipo = TipoSaida((elem \ "@tipo").text)
    val formato = (elem \ "@formato").headOption.map(x => FormatoSaida(x.text)).getOrElse(FS_EXTERNO)
    Saida(tipo = tipo, formato = formato)
  }
}

abstract sealed class TipoSaida(val value : String) extends Product {
  override def toString() : String = value
}

object TipoSaida {
  val tipoSaidaValues : Map[String,TipoSaida] = Map(
    (TS_DOCUMENTO_ORIGINAL.value, TS_DOCUMENTO_ORIGINAL),
    (TS_PDF_ORIGINAL.value, TS_PDF_ORIGINAL),
    (TS_XML_DERIVADO.value, TS_XML_DERIVADO),
    (TS_ZIP_DERIVADO.value, TS_ZIP_DERIVADO),
    (TS_PDF_DERIVADO.value, TS_PDF_DERIVADO),
    (TS_RTF_DERIVADO.value, TS_RTF_DERIVADO),
    (TS_DOCX_DERIVADO.value, TS_DOCX_DERIVADO),
    (TS_DOCXDIFF_DERIVADO.value, TS_DOCXDIFF_DERIVADO),
    (TS_XML_REMISSOES.value, TS_XML_REMISSOES),
    (TS_EPUB_DERIVADO.value, TS_EPUB_DERIVADO),
    (TS_PDF_DIFF.value, TS_PDF_DIFF),
    (TS_XHTML_INTERMEDIARIO.value, TS_XHTML_INTERMEDIARIO)
  )
  def apply(value : String) : TipoSaida =
    tipoSaidaValues.getOrElse(value,throw new RuntimeException(s"tipo de saída não esperado: $value"))
}

case object TS_DOCUMENTO_ORIGINAL extends TipoSaida("DOCUMENTO_ORIGINAL")
case object TS_PDF_ORIGINAL extends TipoSaida("PDF_ORIGINAL")
case object TS_XML_DERIVADO extends TipoSaida("XML_DERIVADO")
case object TS_ZIP_DERIVADO extends TipoSaida("ZIP_DERIVADO")
case object TS_PDF_DERIVADO extends TipoSaida("PDF_DERIVADO")
case object TS_RTF_DERIVADO extends TipoSaida("RTF_DERIVADO")
case object TS_DOCX_DERIVADO extends TipoSaida("DOCX_DERIVADO")
case object TS_DOCXDIFF_DERIVADO extends TipoSaida("DOCXDIFF_DERIVADO")
case object TS_XML_REMISSOES extends TipoSaida("XML_REMISSOES")
case object TS_EPUB_DERIVADO extends TipoSaida("EPUB_DERIVADO")
case object TS_PDF_DIFF extends TipoSaida("PDF_DIFF")
case object TS_XHTML_INTERMEDIARIO extends TipoSaida("XHTML_INTERMEDIARIO")

abstract sealed class FormatoSaida(val value : String) extends Product {
  override def toString() : String = value
}

object FormatoSaida {
  val formatoSaidaValues : Map[String,FormatoSaida] = Map(
    (FS_EXTERNO.value, FS_EXTERNO),
    (FS_EMBUTIDO.value, FS_EMBUTIDO)
  )
  def apply(value : String) : FormatoSaida =
    formatoSaidaValues.getOrElse(value,throw new RuntimeException(s"tipo de formato de saída não esperado: $value"))
}


case object FS_EXTERNO extends FormatoSaida("EXTERNO")
case object FS_EMBUTIDO extends FormatoSaida("EMBUTIDO")



final case class Metadado(
                         localidade : String,
                         autoridade : String,
                         tipoNorma : String,
                         descritorEvento : Option[String]
                         ) {
  def asXML : Elem =
    <pr:metadado xmlns:pr={XMLConsts.prNamespace}>
      <pr:localidade>{localidade}</pr:localidade>
      <pr:autoridade>{autoridade}</pr:autoridade>
      <pr:tipoNorma>{tipoNorma}</pr:tipoNorma>
      {descritorEvento.map(x => <pr:descritorEvento>{x}</pr:descritorEvento>).getOrElse(NodeSeq.Empty)}
    </pr:metadado>
}

object Metadado {
  def fromXML(elem : Elem) : Metadado = {
    val localidade = (elem \ "localidade").text
    val autoridade = (elem \ "autoridade").text
    val tipoNorma = (elem \ "tipoNorma").text
    val descritorEvento = (elem \ "descritorEvento").headOption.map(x => x.text)
    Metadado(localidade = localidade, autoridade = autoridade, tipoNorma = tipoNorma,
      descritorEvento = descritorEvento)
  }
}

abstract sealed class MimeEntrada(val value : String) extends Product {
  override def toString() : String = value
}

object MimeEntrada {
  val mimeEntradaValues : Map[String,MimeEntrada] = Map(
    ("application/msword",ME_MSWord),
    ("text/rtf",ME_RTF),
    ("text/html",ME_HTML),
    ("application/xhtml+xml",ME_XHTML),
    ("text/plain",ME_PLAIN),
    ("application/vnd.oasis.opendocument.text",ME_ODF),
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document",ME_DOCX)
  )
  def apply(value : String) : MimeEntrada =
    mimeEntradaValues.getOrElse(value, throw new RuntimeException(s"MIME de entrada não esperado: $value"))
}

case object ME_MSWord extends MimeEntrada("application/msword")
case object ME_RTF extends MimeEntrada("text/rtf")
case object ME_HTML extends MimeEntrada("text/html")
case object ME_XHTML extends MimeEntrada("application/xhtml+xml")
case object ME_PLAIN extends MimeEntrada("text/plain")
case object ME_ODF extends MimeEntrada("application/vnd.oasis.opendocument.text")
case object ME_DOCX extends MimeEntrada("application/vnd.openxmlformats-officedocument.wordprocessingml.document")

abstract sealed class TextoEntrada extends Product {
  def asXML : Elem
}

object TextoEntrada {
  def fromXML(elem : Elem) : TextoEntrada =
    elem.label match {
      case "TextoEmbutido" => TE_TextoEmbutido.fromXML(elem)
      case "TextoAnexo" => TE_TextoAnexo.fromXML(elem)
      case "TextoExterno" => TE_TextoExterno.fromXML(elem)
      case _ => throw new RuntimeException(s"Elemento de texto não esperado em requisição: ${elem.label}")
    }
}

final case class TE_TextoEmbutido(contents : Array[Byte]) extends TextoEntrada {
  def asXML : Elem =
    <pr:TextoEmbutido xmlns:pr={XMLConsts.prNamespace}>
      {new String(TE_TextoEmbutido.b64.encode(contents),"UTF-8")}
    </pr:TextoEmbutido>
}

object TE_TextoEmbutido {
  val b64 = new Base64()
  def fromXML(elem : Elem) : TE_TextoEmbutido = {
    val contents = b64.decode(elem.text)
    TE_TextoEmbutido(contents)
  }
}

final case class TE_TextoAnexo(nomeCampo : String) extends TextoEntrada {
  def asXML : Elem =
    <pr:TextoAnexo xmlns:pr={XMLConsts.prNamespace} nomeCampo={nomeCampo}/>
}

object TE_TextoAnexo {
  def fromXML(elem : Elem) : TE_TextoAnexo = {
    TE_TextoAnexo(nomeCampo = (elem \ "@nomeCampo").text)
  }
}

final case class TE_TextoExterno(href : URI) extends TextoEntrada {
  def asXML : Elem =
    <pr:TextoExterno xmlns:pr={XMLConsts.prNamespace} href={href.toString}/>
}

object TE_TextoExterno {
  def fromXML(elem : Elem) : TE_TextoExterno = {
    TE_TextoExterno(href = URI.create((elem \ "@href").text))
  }
}

final case class OpcoesRequisicao(
                                   profile : Option[ParserProfile] = None,
                                   alwaysParse : Boolean = true
                                 ) {
  def asXML : Elem =
      <pr:opcoes xmlns:pr={XMLConsts.prNamespace}>
        {NodeSeq.fromSeq(profile.map(_.asXML).toSeq)}
        <pr:alwaysParse>{alwaysParse.toString}</pr:alwaysParse>
      </pr:opcoes>
}

object OpcoesRequisicao {
  def fromXML(elem : Elem) : OpcoesRequisicao = {
    val profile = (elem \ "profile").collectFirst { case e : Elem => ParserProfile.fromXML(e) }
    val alwaysParse = (elem \ "alwaysParse").text.toBooleanOption.getOrElse(true)
    OpcoesRequisicao(profile = profile, alwaysParse = alwaysParse)
  }
}

final case class ParserProfile(
  regexLocalData : Option[Vector[String]],
  regexJustificativa : Option[Vector[String]],
  regexAnexos : Option[Vector[String]],
  regexLegislacaoCitada : Option[Vector[String]],
  regexAssinatura : Option[Vector[String]],
  regexEpigrafe : Option[Vector[String]],
  regexPosEpigrafe : Option[Vector[String]],
  regexPreambulo : Option[Vector[String]],
  epigrafeObrigatoria : Option[Boolean], //true
  preEpigrafePermitida : Option[Boolean], //false
  urnFragTipoNorma : Option[String],
  epigrafeHead : Option[String],
  epigrafeTail : Option[String],
  urnFragAutoridade : Option[String],
  autoridadeEpigrafe : Option[Option[String]],
  ementaAusente : Option[Boolean] /* false */) {
  def asXML : Elem = {
    <pr:profile xmlns:pr={XMLConsts.prNamespace}>
      {if (regexLocalData.isEmpty) { NodeSeq.Empty} else {<pr:regexLocalData xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexLocalData.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexLocalData>}}
      {if (regexJustificativa.isEmpty) { NodeSeq.Empty} else {<pr:regexJustificativa xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexJustificativa.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexJustificativa>}}
      {if (regexAnexos.isEmpty) { NodeSeq.Empty} else {<pr:regexAnexos xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexAnexos.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexAnexos>}}
      {if (regexLegislacaoCitada.isEmpty) { NodeSeq.Empty} else {<pr:regexLegislacaoCitada xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexLegislacaoCitada.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexLegislacaoCitada>}}
      {if (regexAssinatura.isEmpty) { NodeSeq.Empty} else {<pr:regexAssinatura xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexAssinatura.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexAssinatura>}}
      {if (regexEpigrafe.isEmpty) { NodeSeq.Empty} else {<pr:regexEpigrafe xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexEpigrafe.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexEpigrafe>}}
      {if (regexPosEpigrafe.isEmpty) { NodeSeq.Empty} else {<pr:regexPosEpigrafe xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexPosEpigrafe.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexPosEpigrafe>}}
      {if (regexPreambulo.isEmpty) { NodeSeq.Empty} else {<pr:regexPreambulo xmlns:pr={XMLConsts.prNamespace}>{NodeSeq.fromSeq(regexPreambulo.toSeq.map(y => <pr:regex>{y}</pr:regex>))}</pr:regexPreambulo>}}
      {NodeSeq.fromSeq(epigrafeObrigatoria.toSeq.map(x => <pr:epigrafeObrigatoria>{x.toString}</pr:epigrafeObrigatoria>))}
      {NodeSeq.fromSeq(preEpigrafePermitida.toSeq.map(x => <pr:preEpigrafePermitida>{x.toString}</pr:preEpigrafePermitida>))}
      {NodeSeq.fromSeq(urnFragTipoNorma.map(x => <pr:urnFragTipoNorma>{x}</pr:urnFragTipoNorma>).toSeq)}
      {NodeSeq.fromSeq(epigrafeHead.map(x => <pr:epigrafeHead>{x}</pr:epigrafeHead>).toSeq)}
      {NodeSeq.fromSeq(epigrafeTail.map(x => <pr:epigrafeTail>{x}</pr:epigrafeTail>).toSeq)}
      {NodeSeq.fromSeq(autoridadeEpigrafe.map(x =>
        <pr:autoridadeEpigrafe>{NodeSeq.fromSeq(x.map(y => <pr:autoridadeEpigrafeValue>{y}</pr:autoridadeEpigrafeValue>).toSeq)}</pr:autoridadeEpigrafe>).toSeq)}
      {NodeSeq.fromSeq(ementaAusente.toSeq.map(x => <pr:ementaAusente>{x.toString}</pr:ementaAusente>))}
    </pr:profile>
  }
}

object ParserProfile {
  def fromXML(elem : Elem) : ParserProfile = {
    def regexList(label : String) : Option[Vector[String]] =
      (elem \ label).collectFirst { case e : Elem => (e \ "regex").map(_.text).toVector }
    val regexLocalData = regexList("regexLocalData")
    val regexJustificativa = regexList("regexJustificativa")
    val regexAnexos = regexList("regexAnexos")
    val regexLegislacaoCitada = regexList("regexLegislacaoCitada")
    val regexAssinatura = regexList("regexAssinatura")
    val regexEpigrafe = regexList("regexEpigrafe")
    val regexPosEpigrafe = regexList("regexPosEpigrafe")
    val regexPreambulo = regexList("regexPreambulo")
    val epigrafeObrigatoria =
      (elem \ "epigrafeObrigatoria").collectFirst { case e : Elem => e.text.toBooleanOption }.flatten
    val preEpigrafePermitida =
      (elem \ "preEpigrafePermitida").collectFirst { case e : Elem => e.text.toBooleanOption }.flatten
    val urnFragTipoNorma = (elem \ "urnFragTipoNorma").collectFirst { case e : Elem => e.text }
    val epigrafeHead = (elem \ "epigrafeHead").collectFirst { case e : Elem => e.text }
    val epigrafeTail = (elem \ "epigrafeTail").collectFirst { case e : Elem => e.text }
    val urnFragAutoridade = (elem \ "urnFragAutoridade").collectFirst { case e : Elem => e.text }
    val autoridadeEpigrafe = (elem \ "autoridadeEpigrafe").collectFirst {
      case e : Elem => (e \ "autoidadeEpigrafeValue").collectFirst { case ee : Elem => ee.text }
    }
    val ementaAusente =
      (elem \ "ementaAusente").collectFirst { case e : Elem => e.text.toBooleanOption }.flatten

    ParserProfile(
      regexLocalData = regexLocalData,
      regexJustificativa = regexJustificativa,
      regexAnexos = regexAnexos,
      regexLegislacaoCitada = regexLegislacaoCitada,
      regexAssinatura = regexAssinatura,
      regexEpigrafe = regexEpigrafe,
      regexPosEpigrafe = regexPosEpigrafe,
      regexPreambulo = regexPreambulo,
      epigrafeObrigatoria = epigrafeObrigatoria,
      preEpigrafePermitida = preEpigrafePermitida,
      urnFragTipoNorma = urnFragTipoNorma,
      epigrafeHead = epigrafeHead,
      epigrafeTail = epigrafeTail,
      urnFragAutoridade = urnFragAutoridade,
      autoridadeEpigrafe = autoridadeEpigrafe,
      ementaAusente = ementaAusente
    )
  }
}
