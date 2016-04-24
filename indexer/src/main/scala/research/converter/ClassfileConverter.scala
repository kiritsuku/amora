package research
package converter

import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

final class ClassfileConverter {
  import indexer.{ hierarchy ⇒ h }
  import indexer.hierarchy.{ Attachment ⇒ a }

  private val found = ListBuffer[h.Hierarchy]()

  def convert(bytecode: Array[Byte]): Try[Seq[h.Hierarchy]] = {
    this.found.clear()

    val found = Try {
      val r = new ClassReader(bytecode)
      val v = new CVisitor
      r.accept(v, 0)
      this.found
    }

    found match {
      case Success(found) ⇒
        Success(found.toList)
      case Failure(f) ⇒
        Failure(new RuntimeException(s"Conversion of bytecode failed. See underlying issue for more information.", f))
    }
  }

  private class CVisitor extends ClassVisitor(Opcodes.ASM5) {
    var owner: h.Decl = h.Root

    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      val d = mkDecl(name)
      d.addAttachments(a.Class)
      owner = d
      found += d
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      found += h.Decl(name, owner)
      super.visitField(access, name, desc, signature, value)
    }

    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      if (name == "<init>")
        null
      else {
        val d = h.Decl(name, owner)
        d.addAttachments(a.Def, a.JvmSignature(desc))
        found += d
        new MVisitor(d)
      }
    }
  }

  private class MVisitor(owner: h.Decl) extends MethodVisitor(Opcodes.ASM5) {
    override def visitParameter(name: String, access: Int) = {
      found += h.Decl(name, owner)
    }
  }

  /**
   * Packages are part of the name, divided by slashes. They are split into a
   * `Decl` hierarchy.
   */
  private def mkDecl(name: String) = {
    val parts = name.split('/')
    parts.tail.foldLeft(h.Decl(parts.head, h.Root)) {
      (a, b) ⇒ h.Decl(b, a)
    }
  }
}
