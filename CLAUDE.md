# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

This is the **API / data-binding library** (a published JAR, not a running server) for the LexML Brazil parser web service. It defines the request (`ParserRequisicao`) and response (`ParserResultado`) contract used to submit Brazilian legislative documents for parsing into LexML XML and to retrieve the derived outputs. The actual parsing service is a separate component; the shell scripts under `src/test/xml` POST against a deployed instance (e.g. `http://localhost:8080/lexml-parser/parse/parseSenado`).

Mixed Scala 2.13 + Java 8 project, built with Maven, published to Sonatype OSSRH (`groupId br.gov.lexml.parser.pl`).

## Commands

```bash
mvn compile                    # generate JAXB sources from XSDs + compile Scala/Java
mvn package                    # build the JAR
mvn clean install              # install to local ~/.m2 for downstream consumers

mvn -P release release:prepare release:perform   # GPG-signed release to OSSRH (release profile)
```

There is **no JUnit/ScalaTest suite** — `mvn test` runs nothing meaningful. `src/test/xml` holds sample request/response XML plus curl-based shell scripts (`postfile.sh`, `testpost.sh`, etc.) for exercising a *running* parser server manually. Set `PARSER_HOST` (default `localhost:8080`) before running them.

## Architecture

The contract is defined **twice, in parallel**, and you usually need to keep both in sync:

1. **XSD schemas** (`src/main/xsd/`) are the canonical wire format. The `jax-maven-plugin` runs `xjc` during `generate-sources` to produce JAXB classes into `target/generated-sources/jaxb`:
   - `ws/parser-ws.xsd` → package `br.gov.lexml.parser.pl.ws.data.jaxb` (request/result types)
   - `remissoes/remissoes.xsd` (+ `xlink.xsd`) → package `br.gov.lexml.parser.pl.remissoes.jaxb` (cross-reference links)
   - All generated WS classes extend the hand-written `Base.java`, which delegates `equals`/`hashCode`/`toString` to Pojomatic (wired in via `xjc:superClass` in the XSD's `globalBindings`).

2. **Hand-written Scala case classes** (`src/main/scala/.../data/`) are a separate, idiomatic model of the same contract, using `scala-xml` rather than JAXB. Each type carries its own `asXML` (serialize) and a companion `fromXML`/`fromElem` (parse). These are the types most code works with:
   - `requisicao.scala` — `ParserRequisicao`, `Metadado`, `Saida`, the `TipoSaida`/`FormatoSaida`/`MimeEntrada` sealed enums, the three `TextoEntrada` variants (embutido/anexo/externo), and `ParserProfile` (regex overrides controlling how the parser segments a document).
   - `resultado.scala` — `ParserResultado`, `Falha`, `Caracteristica`, `ElementoSaida`, `ConteudoSaida` (binary vs. XML), `TipoMimeSaida`.
   - `remissoes.scala` — `Remissoes` / `RemissaoDocumento` / `RemissaoFragmento` with `xlink` attributes.
   - `common.scala` — the three XML namespaces (`prNamespace`, `rsNamespace`, `xlinkNamespace`) all serialization relies on.

Because the JAXB and Scala models are independent, **a change to the wire format means editing the XSD *and* the corresponding Scala case class + its `asXML`/`fromXML` by hand.** The sealed-enum pattern (e.g. `TipoSaida`) requires updating both the `case object` and the lookup `Map` in the companion's `apply`.

`Dependencies.scala` encodes the **output-dependency graph**: requesting a derived output implies its prerequisites (e.g. `PDF_DERIVADO` requires `XML_DERIVADO`; `PDF_DIFF` requires `DOCXDIFF_DERIVADO`). `completeDependencies` expands a request's `saidas` to include all transitive prerequisites, and (when `alwaysParse`) injects `XML_DERIVADO`. Call this before handing a request to the parser.

All identifiers, enum string values, and error messages are in Portuguese (`saída` = output, `entrada` = input, `falha` = failure, `remissão` = cross-reference, `metadado` = metadata).
