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

  def sentence = rule { word ~ verb ~ word ~ noun ~> Sentence }

  def verb = rule { test(valueStack.peek.asInstanceOf[Word].tpes.contains(WordType.Verb)) ~> ((w: Word) ⇒ Verb(w.stemmed, w.word)) }
  def noun = rule { test(valueStack.peek.asInstanceOf[Word].tpes.contains(WordType.Noun)) ~> ((w: Word) ⇒ Noun(w.stemmed, w.word)) }

  def mkWord(word: String): Word = {
    val meanings = Words.dictionary.lookupAllIndexWords(word).getIndexWordArray.toList

    if (meanings.isEmpty) {
      if (Words.prepositions(word))
        Word(word, word, Seq(WordType.Preposition))
      else
        ???
    }
    else {
      val tpes = meanings.map { w ⇒
        w.getPOS match {
          case POS.NOUN ⇒ WordType.Noun
          case POS.VERB ⇒ WordType.Verb
          case POS.ADJECTIVE ⇒ WordType.Adjective
          case POS.ADVERB ⇒ WordType.Adverb
        }
      }
      Word(word, meanings.head.getLemma, tpes)
    }
  }
}

final class ParseError(msg: String) extends RuntimeException(msg)

trait Tree
case class Sentence(verb: Verb, noun: Noun) extends Tree
case class Word(word: String, stemmed: String, tpes: Seq[WordType.WordType]) extends Tree
case class Verb(word: String, original: String) extends Tree
case class Noun(word: String, unstemmed: String) extends Tree
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
