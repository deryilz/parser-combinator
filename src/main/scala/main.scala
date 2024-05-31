@main def hello(): Unit =
  //  parentElement
  val text =
    """
     <top label="Top">
          <semi-bottom label="Bottom"/>
          <middle>
              <bottom label="Another bottom"/>
          </middle>
      </top>
      """
  val elem = xmlFile.parse(text)
  println(elem)//.toString.replace(",", ",\n"))