package br.gov.lexml.parser.pl.ws

import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoSaida
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTipoDeSaida
import br.gov.lexml.parser.pl.ws.data.scalaxb.PDF_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.RTF_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.EPUB_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.ZIP_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.XML_DERIVADO
import br.gov.lexml.parser.pl.ws.data.scalaxb.PDF_DIFF
import br.gov.lexml.parser.pl.ws.data.scalaxb.ParserRequisicao
import br.gov.lexml.parser.pl.ws.data.scalaxb.EXTERNO
import br.gov.lexml.parser.pl.ws.data.scalaxb.TipoTiposDeSaidas

object Dependencies {
  val depMap = Map[TipoSaida, Set[TipoSaida]](
    EPUB_DERIVADO -> Set(XML_DERIVADO),
    ZIP_DERIVADO -> Set(XML_DERIVADO),
    PDF_DERIVADO -> Set(XML_DERIVADO),
    PDF_DIFF -> Set(RTF_DERIVADO)).withDefaultValue(Set[TipoSaida]())
  def deps(t: TipoTipoDeSaida): Seq[TipoTipoDeSaida] = t +: depMap(t.tipo).to[Seq].map(tt => TipoTipoDeSaida(tt, EXTERNO))

  def completeDependencies(req: ParserRequisicao): ParserRequisicao = {
    val alwaysParse = req.opcoes.flatMap(_.alwaysParse).getOrElse(true)
    val saidas = req.saidas.tipoSaida
    val saidas1 = if (alwaysParse && !saidas.contains(XML_DERIVADO)) { saidas :+ TipoTipoDeSaida(XML_DERIVADO, EXTERNO) } else { saidas }
    val saidas2 = saidas1.flatMap(deps) ++ saidas1
    val saidas3 = saidas2.map(t => (t.tipo, t.formato)).toMap.map { case (t, f) => TipoTipoDeSaida(t, f) }.toSeq
    req.copy(saidas = TipoTiposDeSaidas(saidas3: _*))
  }
}
