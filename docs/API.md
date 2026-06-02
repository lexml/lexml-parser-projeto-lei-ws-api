# Documentação de uso — LexML Parser Web Service API

Esta biblioteca define o **contrato de dados** (requisição e resultado) do serviço web do parser LexML, que converte documentos normativos brasileiros (projetos de lei, leis etc.) em XML LexML e em formatos derivados (PDF, DOCX, EPUB, RTF, ZIP).

A biblioteca **não é o servidor**: ela publica os esquemas XSD, as classes JAXB geradas e os *case classes* Scala usados para montar/ler as mensagens trocadas com um servidor parser já implantado.

- **Namespaces XML**
  - Requisição/resultado: `http://www.lexml.gov.br/parser-ws` (prefixo usual `pr` ou `Q1`)
  - Remissões: `http://www.lexml.gov.br/schema/remissoes` (prefixo `rs`)
  - XLink: `http://www.w3.org/1999/xlink`
- **Esquemas**: `src/main/xsd/ws/parser-ws.xsd`, `src/main/xsd/remissoes/remissoes.xsd`

---

## 1. Protocolo HTTP

O fluxo é **assíncrono em duas etapas**: você submete um documento, recebe um `Location` (HTTP 3xx/201) apontando para o recurso do resultado, aguarda o processamento e então faz `GET` nesse URL para obter o `ParserResultado`.

### Endpoint

```
POST http://<host>/lexml-parser/parse/parseSenado
```

(`<host>` por padrão `localhost:8080` nos scripts de exemplo — veja `src/test/xml/defaults.sh`.)

### Forma A — `multipart/form-data` (arquivo anexo)

A requisição XML usa `<TextoAnexo nomeCampo="...">` e o documento-fonte vai em campo separado do formulário. Nos exemplos o campo de fonte chama-se `fonte` e o da requisição `requisicao`:

```sh
curl -v \
  -F "fonte=@documento.docx" \
  -F "requisicao=@requisicao.xml" \
  "http://localhost:8080/lexml-parser/parse/parseSenado"
```

(ver `src/test/xml/postfile.sh`)

### Forma B — corpo XML único (texto embutido)

A requisição XML carrega o documento em Base64 dentro de `<TextoEmbutido>` e é enviada como `text/xml`:

```sh
curl -v -H "Content-type: text/xml" \
  --data-binary "@requisicao_embutida.xml" \
  "http://localhost:8080/lexml-parser/parse/parseSenado"
```

(ver `src/test/xml/postembutido.sh`)

### Recuperar o resultado

O `POST` retorna um cabeçalho `Location` com a URL do resultado. Após aguardar o processamento, faça o `GET`:

```sh
wget -O - "<URL-do-Location>" | xmllint --format -
```

(ver `src/test/xml/showresult.sh`; `testpost.sh`/`testpostembutido.sh` encadeiam POST → espera → GET, com atraso configurável via `DELAY`.)

---

## 2. Mensagem de requisição — `ParserRequisicao`

Estrutura (elemento raiz `pr:ParserRequisicao`):

| Elemento | Obrig. | Descrição |
|----------|:------:|-----------|
| `metadado` | sim | Identificação da norma (ver abaixo) |
| `texto` | sim | MIME de entrada + uma das três fontes de texto |
| `saidas` | sim | Um ou mais `tipoSaida` desejados |
| `opcoes` | não | Profile de parsing e flag `alwaysParse` |

### `metadado`

| Campo | Obrig. | Exemplo |
|-------|:------:|---------|
| `localidade` | sim | `br` |
| `autoridade` | sim | `senado.federal` |
| `tipoNorma` | sim | `projeto.lei;pls` |
| `descritorEvento` | não | `2011;178@data.evento;iniciativa;2011-10-26t10.03` |

### `texto`

`tipoMime` deve ser um dos MIME de **entrada** suportados:

```
application/msword
text/rtf
text/html
application/xhtml+xml
text/plain
application/vnd.oasis.opendocument.text
application/vnd.openxmlformats-officedocument.wordprocessingml.document   (DOCX)
```

Em seguida, **exatamente uma** das três fontes de texto:

- `<TextoEmbutido>` — conteúdo do documento em **Base64**, no próprio XML.
- `<TextoAnexo nomeCampo="..."/>` — o documento vem como parte `multipart` cujo nome é `nomeCampo`.
- `<TextoExterno href="..."/>` — o servidor busca o documento na URI informada.

### `saidas`

Lista de `<tipoSaida tipo="..." formato="EXTERNO|EMBUTIDO"/>`. `formato` é opcional (padrão `EXTERNO`):

- `EXTERNO` — o resultado referencia o conteúdo por `href`.
- `EMBUTIDO` — o conteúdo vem embutido no resultado (Base64 ou XML).

Tipos de saída disponíveis:

| `tipo` | Conteúdo |
|--------|----------|
| `DOCUMENTO_ORIGINAL` | documento enviado, inalterado |
| `PDF_ORIGINAL` | PDF do documento original |
| `XML_DERIVADO` | **XML LexML** resultante do parsing |
| `ZIP_DERIVADO` | pacote ZIP derivado |
| `PDF_DERIVADO` | PDF gerado a partir do XML |
| `RTF_DERIVADO` | RTF derivado |
| `DOCX_DERIVADO` | DOCX derivado |
| `DOCXDIFF_DERIVADO` | DOCX com marcas de diferença |
| `XML_REMISSOES` | XML de remissões (referências cruzadas) |
| `EPUB_DERIVADO` | EPUB derivado |
| `PDF_DIFF` | PDF do *diff* |
| `XHTML_INTERMEDIARIO` | XHTML intermediário do pipeline |

> **Dependências automáticas:** alguns tipos exigem outros. `EPUB_DERIVADO`, `ZIP_DERIVADO`, `PDF_DERIVADO`, `DOCX_DERIVADO` e `DOCXDIFF_DERIVADO` dependem de `XML_DERIVADO`; `PDF_DIFF` depende de `DOCXDIFF_DERIVADO`. A função `Dependencies.completeDependencies` expande a requisição incluindo todas as dependências transitivas (e, com `alwaysParse`, injeta `XML_DERIVADO`). Chame-a antes de enviar a requisição. Ver `src/main/scala/br/gov/lexml/parser/pl/ws/Dependencies.scala`.

### `opcoes` (opcional)

- `alwaysParse` (boolean, padrão `true`) — quando verdadeiro, força a geração de `XML_DERIVADO` mesmo que não esteja na lista de saídas.
- `profile` — um `ParserProfile` que sobrepõe heurísticas do parser (ver §4).

### Exemplo completo (texto anexo)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Q1:ParserRequisicao xmlns:Q1="http://www.lexml.gov.br/parser-ws">
  <Q1:metadado>
    <Q1:localidade>br</Q1:localidade>
    <Q1:autoridade>senado.federal</Q1:autoridade>
    <Q1:tipoNorma>projeto.lei;pls</Q1:tipoNorma>
    <Q1:descritorEvento>2011;178@data.evento;iniciativa;2011-10-26t10.03</Q1:descritorEvento>
  </Q1:metadado>
  <Q1:texto>
    <Q1:tipoMime>text/rtf</Q1:tipoMime>
    <Q1:TextoAnexo nomeCampo="fonte"/>
  </Q1:texto>
  <Q1:saidas>
    <Q1:tipoSaida tipo="DOCUMENTO_ORIGINAL"/>
    <Q1:tipoSaida tipo="XML_DERIVADO"/>
    <Q1:tipoSaida tipo="PDF_DERIVADO"/>
    <Q1:tipoSaida tipo="RTF_DERIVADO"/>
  </Q1:saidas>
</Q1:ParserRequisicao>
```

---

## 3. Mensagem de resultado — `ParserResultado`

Elemento raiz `pr:ParserResultado`, com três blocos:

```
metadado        → ecoa o metadado da norma
resultado       → falhas, características, saídas, digest, data/hora, nº de diferenças
configuracao    → componentes (nome+versão) do servidor que processou
```

### `resultado`

- **`falhas`** — lista de `falha`, cada uma com `codigoTipoFalha` (int), `nomeTipoFalha`, zero ou mais `posicao`, `descricao` e, opcionalmente, `mensagemUsuario` (texto amigável ao usuário). Lista vazia = sucesso.
- **`caracteristicas`** — lista de `caracteristica` (`descricao`, `presente` boolean, `suportadoLexEdit` boolean) descrevendo traços detectados no documento.
- **`saidas`** — lista de `elementoSaida`, um por saída produzida:
  - atributos `tipoSaida`, `tipoMime`, e `href` (opcional, presente quando `EXTERNO`);
  - `digest` opcional;
  - conteúdo opcional (quando `EMBUTIDO`): `conteudoBinario` (Base64) **ou** `conteudoXML` (XML embutido).
- **`digestFonte`** — digest do documento-fonte.
- **`dataHoraProcessamento`** — `dateTime` ISO-8601 com offset.
- **`numeroDiferencas`** — inteiro opcional (relevante para saídas de *diff*).

MIME de **saída** possíveis (`tipoMime`): os mesmos de entrada, mais `text/xml`, `application/zip`, `application/pdf`, `application/epub+zip`.

### Exemplo

```xml
<ParserResultado xmlns="http://www.lexml.gov.br/parser-ws">
  <metadado>
    <localidade>br</localidade>
    <autoridade>federal</autoridade>
    <tipoNorma>lei.federal</tipoNorma>
  </metadado>
  <resultado>
    <falhas/>
    <caracteristicas/>
    <saidas>
      <elementoSaida tipoSaida="XML_DERIVADO" tipoMime="text/xml">
        <conteudoXML> <!-- ... XML LexML ... --> </conteudoXML>
      </elementoSaida>
    </saidas>
    <digestFonte>…</digestFonte>
    <dataHoraProcessamento>2001-12-31T12:00:00</dataHoraProcessamento>
  </resultado>
  <configuracao>
    <componente nome="parser" versao="1.7.6"/>
  </configuracao>
</ParserResultado>
```

---

## 4. `ParserProfile` — customização do parsing

Dentro de `opcoes/profile`, todos os campos são opcionais; quando ausentes, o servidor usa seus padrões. As listas de regex sobrepõem os padrões de reconhecimento de cada seção do documento.

Listas de regex (`<regexX><regex>…</regex>…</regexX>`):
`regexLocalData`, `regexJustificativa`, `regexAnexos`, `regexLegislacaoCitada`, `regexAssinatura`, `regexEpigrafe`, `regexPosEpigrafe`, `regexPreambulo`.

Demais campos:

| Campo | Tipo | Significado |
|-------|------|-------------|
| `epigrafeObrigatoria` | boolean | exige epígrafe no documento (padrão `true`) |
| `preEpigrafePermitida` | boolean | permite conteúdo antes da epígrafe (padrão `false`) |
| `urnFragTipoNorma` | string | fragmento de URN para o tipo de norma |
| `epigrafeHead` / `epigrafeTail` | string | texto fixo antes/depois da epígrafe |
| `urnFragAutoridade` | string | fragmento de URN para a autoridade |
| `autoridadeEpigrafe` | string opcional | autoridade exibida na epígrafe |
| `ementaAusente` | boolean | indica que a norma não tem ementa (padrão `false`) |

---

## 5. Uso programático (Scala)

Os *case classes* em `br.gov.lexml.parser.pl.ws.data` fazem serialização/desserialização via `scala-xml`. Cada tipo expõe `asXML` e a companion oferece `fromXML`/`fromElem`.

```scala
import br.gov.lexml.parser.pl.ws.data._
import br.gov.lexml.parser.pl.ws.Dependencies

// montar uma requisição
val req = ParserRequisicao(
  metadado = Metadado("br", "senado.federal", "projeto.lei;pls", None),
  tipoMime = ME_RTF,
  textoEntrada = TE_TextoAnexo("fonte"),
  saidas = Vector(Saida(TS_XML_DERIVADO), Saida(TS_PDF_DERIVADO)),
  opcoes = None
)

// expandir dependências (PDF_DERIVADO ⇒ XML_DERIVADO etc.)
val completa = Dependencies.completeDependencies(req)

// serializar para XML
val xml: scala.xml.Elem = completa.asXML

// ler um resultado recebido do servidor
val resultado = ParserResultado.fromXML(scala.xml.XML.loadString(respostaXml))
resultado.saidas.foreach(s => println(s.tipoSaida -> s.href))
```

> Para a fonte embutida use `TE_TextoEmbutido(bytes)` (codifica/decodifica Base64 automaticamente); para fonte externa, `TE_TextoExterno(uri)`.

Existe também o modelo **JAXB** equivalente (gerado dos XSDs em `mvn compile`, pacotes `…ws.data.jaxb` e `…remissoes.jaxb`) para consumidores Java.

---

## 6. Remissões — `Remissoes`

A saída `XML_REMISSOES` segue o esquema `remissoes.xsd` (namespace `rs` + `xlink`). Estrutura:

```
Remissoes
  └─ documento (rs:urn, rs:display, xlink:href, xlink:type)
       └─ fragmento (rs:urn, rs:display, xlink:href, xlink:type)
```

`xlink:type` aceita: `simple`, `extended`, `title`, `resource`, `locator`, `arc`. Cada `documento`/`fragmento` referencia uma norma/dispositivo citado pela URN LexML e pelo `href`. Ver `src/main/scala/br/gov/lexml/parser/pl/ws/data/remissoes.scala`.

---

## Referência rápida de arquivos

- Esquemas: `src/main/xsd/ws/parser-ws.xsd`, `src/main/xsd/remissoes/`
- Modelo Scala: `src/main/scala/br/gov/lexml/parser/pl/ws/data/`
- Grafo de dependências de saída: `…/ws/Dependencies.scala`
- Exemplos de requisição/resultado e scripts curl: `src/test/xml/`
