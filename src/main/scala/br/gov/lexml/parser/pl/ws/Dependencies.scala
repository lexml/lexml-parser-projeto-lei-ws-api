package br.gov.lexml.parser.pl.ws

import br.gov.lexml.parser.pl.ws.data.scalaxb._
import scalaxb._

object Dependencies {
  val tipoTipoDeSaida = { (tt : TipoSaida, tf : TipoFormatoSaida) => 
    TipoTipoDeSaida(Map("@tipo" -> DataRecord(tt), "@formato" -> DataRecord(tf))) } 
  val depMap = Map[TipoSaida, Set[TipoSaida]](
    EPUB_DERIVADO -> Set(XML_DERIVADO),
    ZIP_DERIVADO -> Set(XML_DERIVADO),
    PDF_DERIVADO -> Set(XML_DERIVADO),
    PDF_DIFF -> Set(RTF_DERIVADO)).withDefaultValue(Set[TipoSaida]())
  def deps(t: TipoTipoDeSaida): Seq[TipoTipoDeSaida] = t +: depMap(t.tipo).to[Seq].map(tipoTipoDeSaida(_,EXTERNO))

  def completeDependencies(req: ParserRequisicao): ParserRequisicao = {
    val alwaysParse = req.opcoes.flatMap(_.alwaysParse).getOrElse(true)
    val saidas = req.saidas.tipoSaida
    val saidas1 = if (alwaysParse && !saidas.contains(XML_DERIVADO)) { 
        saidas :+ TipoTipoDeSaida(Map(
            "@tipo" -> DataRecord(XML_DERIVADO : TipoSaida), 
            "@formato" -> DataRecord(EXTERNO : TipoFormatoSaida))) 
       } else { saidas }
    val saidas2 = saidas1.flatMap(deps) ++ saidas1
    val saidas3 = saidas2.map(t => (t.tipo, t.formato)).toMap.map(tipoTipoDeSaida.tupled).to[Seq]
    req.copy(saidas = TipoTiposDeSaidas(saidas3))
  }
}
