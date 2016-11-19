package amora.nlp

import org.parboiled2._
import net.sf.extjwnl.dictionary.Dictionary
import net.sf.extjwnl.data.POS

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
  def word = rule { ws ~ capture(oneOrMore(CharPredicate.Visible)) ~> mkWord _ }

  def sentence = rule { zeroOrMore(word) ~> Sentence }

  def mkWord(word: String): Word = {
    import WordType._
    val meanings = Words.dictionary.lookupAllIndexWords(word).getIndexWordArray.toList

    if (meanings.isEmpty) {
      if (Words.prepositions(word))
        Word(word, word, Seq(Preposition))
      else
        ???
    }
    else {
      val tpes = meanings.map { w ⇒
        w.getPOS match {
          case POS.NOUN ⇒ Noun
          case POS.VERB ⇒ Verb
          case POS.ADJECTIVE ⇒ Adjective
          case POS.ADVERB ⇒ Adverb
        }
      }
      Word(word, meanings.head.getLemma, tpes)
    }
  }
}

final class ParseError(msg: String) extends RuntimeException(msg)

trait Tree
case class Sentence(words: Seq[Word]) extends Tree
case class Word(word: String, base: String, tpes: Seq[WordType.WordType]) extends Tree
object WordType {
  sealed trait WordType
  case object Noun extends WordType
  case object Verb extends WordType
  case object Adjective extends WordType
  case object Adverb extends WordType
  case object Preposition extends WordType
}
object Words {
  val dictionary = Dictionary.getDefaultResourceInstance

  val prepositions = {
    val src = io.Source.fromInputStream(getClass.getResourceAsStream("/prepositions.txt"), "UTF-8")
    val set = src.getLines().toSet
    src.close()
    set
  }
}
