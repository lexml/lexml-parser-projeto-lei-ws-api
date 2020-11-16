package br.gov.lexml.parser.pl.ws


object Dependencies {
  import scalaxb._
  import br.gov.lexml.parser.pl.ws.data.scalaxb._

  val depMap = Map[TipoSaida, Set[TipoSaida]](
    EPUB_DERIVADO -> Set(XML_DERIVADO),
    ZIP_DERIVADO -> Set(XML_DERIVADO),
    PDF_DERIVADO -> Set(XML_DERIVADO),
    DOCX_DERIVADO -> Set(XML_DERIVADO),
    DOCXDIFF_DERIVADO -> Set(XML_DERIVADO),
    PDF_DIFF -> Set(DOCXDIFF_DERIVADO)).withDefaultValue(Set[TipoSaida]())
  def deps(t: TipoTipoDeSaida): Seq[TipoTipoDeSaida] = t +: depMap(t.tipo).to(Seq).map(tt => TipoTipoDeSaida(Map("@tipo" -> DataRecord(tt), "@formato" -> DataRecord[TipoFormatoSaida](EXTERNO))))

  def completeDependencies(req: ParserRequisicao): ParserRequisicao = {
    val alwaysParse = req.opcoes.flatMap(_.alwaysParse).getOrElse(true)
    val saidas: Seq[TipoTipoDeSaida] = req.saidas.tipoSaida
    val saidas1: Seq[TipoTipoDeSaida] = if (alwaysParse && !saidas.contains(XML_DERIVADO)) { saidas :+ TipoTipoDeSaida(Map("@tipo" -> DataRecord[TipoSaida](XML_DERIVADO), "@formato" -> DataRecord[TipoFormatoSaida](EXTERNO))) } else { saidas }
    val saidas2: Seq[TipoTipoDeSaida] = saidas1.flatMap(deps) ++ saidas1
    val saidas3: Seq[TipoTipoDeSaida] = saidas2.map(t => (t.tipo, t.formato)).toMap.map { case (t, f) => TipoTipoDeSaida(Map("@tipo" -> DataRecord(t), "@formato" -> DataRecord[TipoFormatoSaida](f))) }.toSeq
    req.copy(saidas = TipoTiposDeSaidas(saidas3))
  }
}
