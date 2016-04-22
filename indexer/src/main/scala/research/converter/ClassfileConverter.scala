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

  def convert(bytecode: Array[Byte]): Try[Seq[h.Hierarchy]] = {
    val found = Try {
      val r = new ClassReader(bytecode)
      val v = new Visitor
      r.accept(v, 0)
      v.found
    }

    found match {
      case Success(found) ⇒
        Success(found.toList)
      case Failure(f) ⇒
        Failure(new RuntimeException(s"Conversion of bytecode failed. See underlying issue for more information.", f))
    }
  }

  private class Visitor extends ClassVisitor(Opcodes.ASM5) {
    val found = ListBuffer[h.Hierarchy]()
    var owner: h.Decl = h.Root

    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
      val d = h.Decl(name, owner)
      owner = d
      found += d
    }

    override def visitField(access: Int, name: String, desc: String, signature: String, value: AnyRef): FieldVisitor = {
      found += h.Decl(name, owner)
      super.visitField(access, name, desc, signature, value)
    }

    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
      if (name != "<init>")
        found += h.Decl(name, owner)
      super.visitMethod(access, name, desc, signature, exceptions)
    }

  }
}
