package amora.nlp

import org.parboiled2._

import net.sf.extjwnl.data.IndexWord
import net.sf.extjwnl.data.POS
import net.sf.extjwnl.data.Synset
import net.sf.extjwnl.dictionary.Dictionary

object NlParser {

  def parse[A](str: String)(f: NlParser ⇒ Rule1[A]): A = {
    val p = new NlParser(str)
    p.__run(f(p)) match {
      case scala.util.Success(res) ⇒
        res
      case scala.util.Failure(e: org.parboiled2.ParseError) ⇒
        throw new ParseError(s"Couldn't parse input: ${p.formatError(e, new ErrorFormatter(showTraces = true))}")
      case scala.util.Failure(e: ParseError) ⇒
        throw e
      case scala.util.Failure(t) ⇒
        throw new IllegalStateException("Unexpected error.", t)
    }
  }

  def parseQuery(str: String): Sentence =
    parse(str)(_.query)
}

final class NlParser(override val input: ParserInput) extends Parser {

  type R1T = Rule1[Tree]
  def ws = rule { atomic(zeroOrMore(anyOf(" \t\n"))) }
  def word = rule { ws ~ capture(oneOrMore(CharPredicate.Visible)) ~> mkWord _ }

  def query = rule { sentence ~ EOI }

  def sentence = rule { verbPhrase ~ nounPhrase ~ optional(prepositionPhrase ~ nounPhrase ~> PrepositionPhrase) ~> Sentence }

  def verbPhrase = rule { word ~ verb }
  def nounPhrase = rule { word ~ noun }
  def prepositionPhrase = rule { word ~ preposition }

  def verb = rule { test(valueStack.peek.asInstanceOf[Word].tpes.contains(WordType.Verb)) ~> ((w: Word) ⇒ Verb(w.stemmed, w.word)) }
  def noun = rule { test(valueStack.peek.asInstanceOf[Word].tpes.contains(WordType.Noun)) ~> ((w: Word) ⇒ Noun(w.stemmed, w.word)) }
  def preposition = rule { test(Words.prepositions(valueStack.peek.asInstanceOf[Word].word)) ~> ((w: Word) ⇒ Preposition(w.word)) }

  def mkWord(word: String): Word = {
    val meanings = {
      val m = Words.memoryDictionary.lookupAllIndexWords(word).getIndexWordArray.toList
      if (m.nonEmpty)
        m
      else
        Words.dictionary.lookupAllIndexWords(word).getIndexWordArray.toList
    }

    if (meanings.isEmpty) {
      if (Words.prepositions(word))
        Word(word, word, Seq(WordType.Preposition))
      else
        throw new ParseError(s"Unknown word: $word")
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
case class Sentence(verb: Verb, noun: Noun, pp: Option[PrepositionPhrase]) extends Tree
case class Word(word: String, stemmed: String, tpes: Seq[WordType.WordType]) extends Tree
case class Verb(word: String, original: String) extends Tree
case class Noun(word: String, unstemmed: String) extends Tree
case class Preposition(word: String) extends Tree
case class PrepositionPhrase(preposition: Preposition, noun: Noun) extends Tree
object WordType {
  sealed trait WordType
  case object Noun extends WordType
  case object Verb extends WordType
  case object Adjective extends WordType
  case object Adverb extends WordType
  case object Preposition extends WordType
}
object Words {
  val memoryDictionary = {
    val d = Dictionary.getResourceInstance("/mem_properties.xml")
    d.edit()
    d.addIndexWord(new IndexWord(d, "def", POS.NOUN, new Synset(d, POS.NOUN)))
    // the word `names` can't be stemmed because it exists in wordnet
    d.addIndexWord(new IndexWord(d, "name", POS.NOUN, new Synset(d, POS.NOUN)))
    d.addIndexWord(new IndexWord(d, "name", POS.VERB, new Synset(d, POS.VERB)))
    d
  }
  val dictionary = Dictionary.getDefaultResourceInstance

  val prepositions = {
    val src = io.Source.fromInputStream(getClass.getResourceAsStream("/prepositions.txt"), "UTF-8")
    val set = src.getLines().toSet
    src.close()
    set
  }
}
