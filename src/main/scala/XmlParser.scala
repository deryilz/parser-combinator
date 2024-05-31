case class XmlElement(name: String,
                      properties: Seq[(String, String)],
                      subElements: Seq[XmlElement]) {
  override def toString: String =
    s"""
       |XmlElement(
       |  name: $name
       |  properties: [${properties.map((k, v) => s"$k:$v").mkString(", ")}]
       |  subElements: [${subElements.mkString(", ").linesIterator.map("    " + _).mkString("\n")}]
       |)""".stripMargin
}

/*

<top label="Top">
      <semi-bottom label="Bottom"/>
         <middle>
        <bottom label="Another bottom"/>
    </middle>
</top>

 */

def identifier: Parser[String] =
  Parser
    .charIf(_.isLetter)
    .plus(Parser.charIf(char => char.isLetterOrDigit || char == '-').repeated(0))
    .map((first, rest) => first + rest.mkString(""))

def quoted: Parser[String] =
  Parser
    .charIf(_ != '"')
    .repeated(0)
    .surroundedBy(Parser.str("\""))
    .map(_.mkString(""))

def property: Parser[(String, String)] =
  identifier
    .plusIgnore(Parser.str("=").surroundedBy(Parser.whitespace.repeated(0)))
    .plus(quoted)

def properties: Parser[Seq[(String, String)]] =
  Parser.whitespace.repeated(1).ignorePlus(property).repeated(0)

def singleElement: Parser[XmlElement] =
  Parser
    .str("<")
    .ignorePlus(identifier)
    .plus(properties)
    .plusIgnore(Parser.whitespace.repeated(0))
    .plusIgnore(Parser.str("/>"))
    .map((name, props) => XmlElement(name, props, Seq()))

// <a href="google.com">

def parentElement: Parser[XmlElement] =
  Parser
    .str("<")
    .ignorePlus(identifier.plus(properties))
    .plusIgnore(Parser.str(">"))
    .plus(xmlElement.repeated(0))
    .plusIgnore(Parser.str("</"))
    .plus(identifier)
    .plusIgnore(Parser.str(">"))
    .filter { case (((id1, _), _), id2) => id1 == id2 }
    .map { case (((id, props), subs), _) => XmlElement(id, props, subs) }

def xmlElement: Parser[XmlElement] =
  Parser.wrap(singleElement.or(parentElement).surroundedBy(Parser.whitespace.repeated(0)))
  
def xmlFile: Parser[XmlElement] = xmlElement.plusIgnore(Parser.end)