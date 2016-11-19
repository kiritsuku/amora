package amora.nlp

import org.parboiled2._
import net.sf.extjwnl.dictionary.Dictionary

object NlParser {

  def parse[A](str: String)(f: NlParser ⇒ Rule1[A]): A = {
    val p = new NlParser(str)
    p.__run(f(p)) match {
      case scala.util.Success(res) ⇒
        res
      case scala.util.Failure(e: org.parboiled2.ParseError) ⇒
        throw new ParseError(s"Couldn't parse input: ${p.formatError(e, new ErrorFormatter(showTraces = true))}")
      case err ⇒
        throw new ParseError(s"Unexpected result: $err")
    }
  }

  def parseSentence(str: String): Sentence =
    parse(str)(_.sentence)
}

final class NlParser(override val input: ParserInput) extends Parser {

  type R1T = Rule1[Tree]
  def ws = rule { atomic(zeroOrMore(anyOf(" \t\n"))) }
  def word = rule { ws ~ capture(oneOrMore(CharPredicate.Visible)) ~> Word }

  def sentence = rule { zeroOrMore(word) ~> Sentence }
}

final class ParseError(msg: String) extends RuntimeException(msg)

trait Tree
case class Sentence(words: Seq[Word]) extends Tree
case class Word(word: String) extends Tree {
  import scala.collection.JavaConverters._
  val meanings = Words.dictionary.lookupAllIndexWords(word).getIndexWordArray.toList
}
object Words {
  val dictionary = Dictionary.getDefaultResourceInstance
}
