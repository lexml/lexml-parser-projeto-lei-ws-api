package br.gov.lexml.parser.pl.ws


object Dependencies {
  import br.gov.lexml.parser.pl.ws.data._

  val depMap = Map[TipoSaida, Set[TipoSaida]](
    TS_EPUB_DERIVADO -> Set(TS_XML_DERIVADO),
    TS_ZIP_DERIVADO -> Set(TS_XML_DERIVADO),
    TS_PDF_DERIVADO -> Set(TS_XML_DERIVADO),
    TS_DOCX_DERIVADO -> Set(TS_XML_DERIVADO),
    TS_DOCXDIFF_DERIVADO -> Set(TS_XML_DERIVADO),
    TS_PDF_DIFF -> Set(TS_DOCXDIFF_DERIVADO)).withDefaultValue(Set[TipoSaida]())
  def deps(t: Saida): Seq[Saida] = t +: depMap(t.tipo).to(Seq).map(tt => Saida(tipo = tt, formato = FS_EXTERNO))

  def completeDependencies(req: ParserRequisicao): ParserRequisicao = {
    val alwaysParse = req.opcoes.forall(_.alwaysParse)
    val saidas = req.saidas
    val saidas1: Vector[Saida] =
      if (alwaysParse && !saidas.exists(s => s.tipo == TS_XML_DERIVADO)) {
        saidas :+ Saida(tipo = TS_XML_DERIVADO, formato = FS_EXTERNO) } else { saidas }
    val saidas2: Vector[Saida] = saidas1.flatMap(deps) ++ saidas1
    val saidas3: Seq[Saida] = saidas2.map(t => (t.tipo, t.formato)).toMap
        .map { case (t, f) => Saida(tipo = t, formato = f) }.toSeq
    req.copy(saidas = saidas3.toVector)
  }
}
