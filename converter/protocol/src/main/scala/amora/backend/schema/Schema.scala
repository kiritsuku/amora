package amora.backend.schema

import java.net.URLEncoder

import amora.converter.protocol._

object Schema {

  def mkShortId(s: Schema): String = s match {
    case Project(name) ⇒
      name
    case Artifact(owner, organization, name, version) ⇒
      s"${mkShortId(owner)}/$organization/$name/$version"
    case File(owner, name) ⇒
      s"${mkShortId(owner)}/$name"
    case Package(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Class(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case AbstractClass(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Object(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Trait(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Def(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Val(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Var(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case LazyVal(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
  }

  def mkId(s: Schema): String = s match {
    case _: Project ⇒
      s"http://amora.center/kb/amora/Project/${mkShortId(s)}"
    case _: Artifact ⇒
      s"http://amora.center/kb/amora/Artifact/${mkShortId(s)}"
    case _: File ⇒
      s"http://amora.center/kb/amora/File/${mkShortId(s)}"
    case _: Package ⇒
      s"http://amora.center/kb/amora/Package/${mkShortId(s)}"
    case _: Class ⇒
      s"http://amora.center/kb/amora/Class/${mkShortId(s)}"
    case _: AbstractClass ⇒
      s"http://amora.center/kb/amora/AbstractClass/${mkShortId(s)}"
    case _: Object ⇒
      s"http://amora.center/kb/amora/Object/${mkShortId(s)}"
    case _: Trait ⇒
      s"http://amora.center/kb/amora/Trait/${mkShortId(s)}"
    case _: Def ⇒
      s"http://amora.center/kb/amora/Def/${mkShortId(s)}"
    case _: Val ⇒
      s"http://amora.center/kb/amora/Val/${mkShortId(s)}"
    case _: Var ⇒
      s"http://amora.center/kb/amora/Var/${mkShortId(s)}"
    case _: LazyVal ⇒
      s"http://amora.center/kb/amora/LazyVal/${mkShortId(s)}"
  }

  def mkDefn(s: Schema): String = s match {
    case _: Project ⇒
      s"http://amora.center/kb/amora/Schema/Project"
    case _: Artifact ⇒
      s"http://amora.center/kb/amora/Schema/Artifact"
    case _: File ⇒
      s"http://amora.center/kb/amora/Schema/File"
    case _: Package ⇒
      s"http://amora.center/kb/amora/Schema/Package"
    case _: Class ⇒
      s"http://amora.center/kb/amora/Schema/Class"
    case _: AbstractClass ⇒
      s"http://amora.center/kb/amora/Schema/AbstractClass"
    case _: Object ⇒
      s"http://amora.center/kb/amora/Schema/Object"
    case _: Trait ⇒
      s"http://amora.center/kb/amora/Schema/Trait"
    case _: Def ⇒
      s"http://amora.center/kb/amora/Schema/Def"
    case _: Val ⇒
      s"http://amora.center/kb/amora/Schema/Val"
    case _: Var ⇒
      s"http://amora.center/kb/amora/Schema/Var"
    case _: LazyVal ⇒
      s"http://amora.center/kb/amora/Schema/LazyVal"
  }

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    val sb = new StringBuilder

    def mk(s: Schema): String = s match {
      case Project(name) ⇒
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case Artifact(owner, organization, name, version) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/organization> "$organization" .
                      |  <$id> <$defn/name> "$name" .
                      |  <$id> <$defn/version> "$version" .
        |""".stripMargin)
        id
      case File(owner, fname) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$fname" .
        |""".stripMargin)
        id
      case Package(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case Class(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case Def(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case Val(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case Var(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
      case LazyVal(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name" .
        |""".stripMargin)
        id
    }

    sb.append("INSERT DATA {\n")
    schemas foreach mk
    sb.append("}")
    sb.toString()
  }

  def mkTurtleString(schemas: Seq[Schema]): String = turtleBuilder {
    (addPrefix, addData) ⇒

    def mk(s: Schema): String = s match {
      case Project(name) ⇒
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Project", defn+"/")
        addData(id, "a", "Project:")
        addData(id, s"Project:name", s""""$name"""")
        id
      case Artifact(owner, organization, name, version) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Artifact", defn+"/")
        addData(id, "a", "Artifact:")
        addData(id, s"Artifact:owner", s"<$oid>")
        addData(id, s"Artifact:organization", s""""$organization"""")
        addData(id, s"Artifact:name", s""""$name"""")
        addData(id, s"Artifact:version", s""""$version"""")
        id
      case File(owner, name) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("File", defn+"/")
        addData(id, "a", "File:")
        addData(id, s"File:owner", s"<$oid>")
        addData(id, s"File:name", s""""$name"""")
        id
      case Package(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Package", defn+"/")
        addData(id, "a", "Package:")
        addData(id, s"Package:owner", s"<$oid>")
        addData(id, s"Package:name", s""""$name"""")
        id
      case Class(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Class", defn+"/")
        addData(id, "a", "Class:")
        addData(id, s"Class:owner", s"<$oid>")
        addData(id, s"Class:name", s""""$name"""")
        id
      case Def(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Def", defn+"/")
        addData(id, "a", "Def:")
        addData(id, s"Def:owner", s"<$oid>")
        addData(id, s"Def:name", s""""$name"""")
        id
      case Val(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Val", defn+"/")
        addData(id, "a", "Val:")
        addData(id, s"Val:owner", s"<$oid>")
        addData(id, s"Val:name", s""""$name"""")
        id
      case Var(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("Var", defn+"/")
        addData(id, "a", "Var:")
        addData(id, s"Var:owner", s"<$oid>")
        addData(id, s"Var:name", s""""$name"""")
        id
      case LazyVal(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        addPrefix("LazyVal", defn+"/")
        addData(id, "a", "LazyVal:")
        addData(id, s"LazyVal:owner", s"<$oid>")
        addData(id, s"LazyVal:name", s""""$name"""")
        id
    }

    schemas foreach mk
  }

  def mkTurtleUpdate(file: File, hierarchies: Seq[Hierarchy]): String = turtleBuilder {
    (addPrefix, addData) ⇒

    def mkTpe(decl: Decl) = {
      if (decl.attachments(Attachment.Lazy) && decl.attachments(Attachment.Val))
        "LazyVal"
      else if (decl.attachments(Attachment.Abstract) && decl.attachments(Attachment.Class))
        "AbstractClass"
      else
        decl.attachments.collectFirst {
          case Attachment.Class ⇒ "Class"
          case Attachment.Object ⇒ "Object"
          case Attachment.Trait ⇒ "Trait"
          case Attachment.Package ⇒ "Package"
          case Attachment.Def ⇒ "Def"
          case Attachment.Val ⇒ "Val"
          case Attachment.Var ⇒ "Var"
        }.getOrElse("Decl")
    }

    def mkFullPath(decl: Decl) = {
      def findArtifact(schema: Schema): Artifact = schema match {
        case a: Artifact ⇒ a
        case p: Package ⇒ findArtifact(p.owner)
        case f: File ⇒ findArtifact(f.owner)
        case _ ⇒ ???
      }
      val shortArtifactId = Schema.mkShortId(findArtifact(file))
      val tpe = mkTpe(decl)
      s"http://amora.center/kb/amora/$tpe/$shortArtifactId/${mkShortPath(decl)}"
    }

    def mkOwnerPath(h: Hierarchy, owner: Decl) = {
      val isTopLevelDecl = {
        def isTopLevelDecl = h.attachments.exists(Set(Attachment.Class, Attachment.Trait, Attachment.Object))
        def isPkg = owner.attachments(Attachment.Package)
        isTopLevelDecl && isPkg
      }
      if (isTopLevelDecl)
        mkId(file)
      else
        mkFullPath(owner)
    }

    def loop(h: Hierarchy): Unit = h match {
      case Root ⇒
      case decl @ Decl(name, owner) ⇒
        val tpe = mkTpe(decl)
        val path = mkFullPath(decl)
        val schemaPath = s"http://amora.center/kb/amora/Schema/$tpe"
        addPrefix(tpe, schemaPath+"/")
        addData(path, "a", s"$tpe:")
        addData(path, s"$tpe:name", s""""$name"""")

        if (h.attachments(Attachment.Param)) {
          addData(path, s"$tpe:flag", "<http://amora.center/kb/amora/Flag/param>")
        }
        if (h.attachments(Attachment.TypeParam)) {
          addData(path, s"$tpe:flag", "<http://amora.center/kb/amora/Flag/tparam>")
        }
        if (h.attachments(Attachment.Constructor)) {
          addData(path, s"$tpe:flag", "<http://amora.center/kb/amora/Flag/constructor>")
        }
        if (h.attachments(Attachment.Implicit)) {
          addData(path, s"$tpe:flag", "<http://amora.center/kb/amora/Flag/implicit>")
        }

        decl.attachments.collectFirst {
          case Attachment.JvmSignature(signature) ⇒
            addData(path, s"$tpe:jvmSignature", s""""$signature"""")
        }

        decl.position match {
          case RangePosition(start, end) ⇒
            addData(path, s"$tpe:posStart", start.toString)
            addData(path, s"$tpe:posEnd", end.toString)
          case _ ⇒
        }

        owner match {
          case Root ⇒
            // The owner of a package is an artifact but this can't be represented
            // in the Hierarchy structure. Thus, we index this information separately.
            if (!decl.attachments(Attachment.Package)) {
              val ownerPath = mkId(file)
              addData(path, s"$tpe:owner", s"""<$ownerPath>""")
            }
          case owner: Decl ⇒
            val ownerPath = mkOwnerPath(decl, owner)
            addData(path, s"$tpe:owner", s"""<$ownerPath>""")

            loop(owner)
          case owner: Scope ⇒
            loop(owner.owner)
          case _: Ref ⇒
        }

      case ref @ Ref(name, refToDecl, owner, qualifier) ⇒
        val declPath = refToDecl match {
          case d: Decl ⇒ mkFullPath(d)
          case _ ⇒
            ???
        }
        val shortFileId = mkShortId(file)
        val path = s"$declPath/$shortFileId${uniqueRef(ref.position)}"
        val schemaPath = s"http://amora.center/kb/amora/Schema/Ref"
        addPrefix("Ref", schemaPath+"/")
        addData(path, "a", "Ref:")
        addData(path, "Ref:name", s""""$name"""")
        addData(path, "Ref:refToDecl", s"""<$declPath>""")

        ref.position match {
          case RangePosition(start, end) ⇒
            addData(path, "Ref:posStart", start.toString)
            addData(path, "Ref:posEnd", end.toString)
          case _ ⇒
        }

        owner match {
          case Root ⇒
            val ownerPath = mkDefn(file)
            addData(path, "Ref:owner", s"""<$ownerPath>""")
          case owner: Decl ⇒
            val ownerPath = mkOwnerPath(ref, owner)
            addData(path, "Ref:owner", s"""<$ownerPath>""")

            loop(owner)
          case owner: Scope ⇒
            owner.owner match {
              case decl: Decl ⇒
                val ownerPath = mkOwnerPath(ref, decl) + "/" + encode(owner.attachmentAsString)
                addData(path, "Ref:owner", s"""<$ownerPath>""")
              case _ ⇒
                ???
            }
            loop(owner.owner)
          case _: Ref ⇒
        }
      case scope: Scope ⇒
        val path = scope.owner match {
          case decl: Decl ⇒
            mkFullPath(decl) + "/" + encode(scope.attachmentAsString)
          case _ ⇒
            ???
        }
        val schemaPath = s"http://amora.center/kb/amora/Schema/Scope"
        addPrefix("Scope", schemaPath+"/")
        addData(path, "a", "Scope:")

        scope.position match {
          case RangePosition(start, end) ⇒
            addData(path, "Scope:posStart", start.toString)
            addData(path, "Scope:posEnd", end.toString)
          case _ ⇒
        }
        scope.owner match {
          case decl: Decl ⇒
            val ownerPath = mkFullPath(decl)
            addData(path, "Scope:owner", s"""<$ownerPath>""")
          case _ ⇒
            ???
        }

        loop(scope.owner)
    }

    hierarchies foreach loop
  }

  private def turtleBuilder(f: ((String, String) ⇒ Unit, (String, String, String) ⇒ Unit) ⇒ Unit) = {
    var prefixe = Map[String, String]()
    var data = Map[String, Map[String, String]]()

    def addPrefix(name: String, url: String) = {
      if (!prefixe.contains(name))
        prefixe += name → url
    }
    def addData(url: String, k: String, v: String) = {
      data.get(url) match {
        case Some(map) ⇒
          data += url → (map + k → v)
        case None ⇒
          data += url → Map(k → v)
      }
    }

    f(addPrefix, addData)

    val stringOrdering = new Ordering[String] {
      def compare(a: String, b: String) = String.CASE_INSENSITIVE_ORDER.compare(a, b)
    }

    val sb = new StringBuilder
    val prefixLen = (if (prefixe.isEmpty) 0 else prefixe.keys.map(_.length).max) + 3
    prefixe.toList.sortBy(_._1)(stringOrdering) foreach {
      case (name, url) ⇒
        sb append "@prefix " append name append ":" append " " * (prefixLen - name.length) append "<" append url append "> .\n"
    }
    val len = (if (data.isEmpty) 0 else data.values.map(_.keys.map(_.length).max).max) + 3
    data.toList.sortBy(_._1)(stringOrdering) foreach {
      case (url, kv) ⇒
        sb append "<" append url append ">\n"
        kv.toList.sortBy(_._1)(stringOrdering) foreach {
          case (k, v) ⇒
            sb append "  " append k append " " * (len - k.length) append v append " ;\n"
        }
        sb append ".\n"
    }
    sb.toString
  }

  def mkSparqlUpdate(schema: Schema, data: Seq[Hierarchy]): String = {
    val sb = new StringBuilder

    def mkTpe(decl: Decl) = {
      if (decl.attachments(Attachment.Lazy) && decl.attachments(Attachment.Val))
        "LazyVal"
      else if (decl.attachments(Attachment.Abstract) && decl.attachments(Attachment.Class))
        "AbstractClass"
      else
        decl.attachments.collectFirst {
          case Attachment.Class ⇒ "Class"
          case Attachment.Object ⇒ "Object"
          case Attachment.Trait ⇒ "Trait"
          case Attachment.Package ⇒ "Package"
          case Attachment.Def ⇒ "Def"
          case Attachment.Val ⇒ "Val"
          case Attachment.Var ⇒ "Var"
        }.getOrElse("Decl")
    }

    def mkFullPath(decl: Decl) = {
      def findArtifact(schema: Schema): Artifact = schema match {
        case a: Artifact ⇒ a
        case p: Package ⇒ findArtifact(p.owner)
        case f: File ⇒ findArtifact(f.owner)
        case _ ⇒ ???
      }
      val a = findArtifact(schema)
      val tpe = mkTpe(decl)
      s"http://amora.center/kb/amora/$tpe/${Schema.mkShortId(a)}/${mkShortPath(decl)}"
    }

    def mkOwnerPath(h: Hierarchy, owner: Decl) = {
      val isTopLevelDecl = {
        def isTopLevelDecl = h.attachments.exists(Set(Attachment.Class, Attachment.Trait, Attachment.Object))
        def isPkg = owner.attachments(Attachment.Package)
        isTopLevelDecl && isPkg
      }
      if (isTopLevelDecl)
        Schema.mkId(schema)
      else
        mkFullPath(owner)
    }

    def loop(h: Hierarchy): Unit = h match {
      case Root ⇒
      case decl @ Decl(name, owner) ⇒
        val tpe = mkTpe(decl)
        val path = mkFullPath(decl)
        val schemaPath = s"http://amora.center/kb/amora/Schema/$tpe"
        sb.append(s"  <$path> a <$schemaPath/> .\n")
        sb.append(s"""  <$path> <$schemaPath/name> "$name" .""" + "\n")

        if (h.attachments(Attachment.Param)) {
          sb.append(s"""  <$path> <$schemaPath/flag> <http://amora.center/kb/amora/Flag/param> .""" + "\n")
        }
        if (h.attachments(Attachment.TypeParam)) {
          sb.append(s"""  <$path> <$schemaPath/flag> <http://amora.center/kb/amora/Flag/tparam> .""" + "\n")
        }
        if (h.attachments(Attachment.Constructor)) {
          sb.append(s"""  <$path> <$schemaPath/flag> <http://amora.center/kb/amora/Flag/constructor> .""" + "\n")
        }
        if (h.attachments(Attachment.Implicit)) {
          sb.append(s"""  <$path> <$schemaPath/flag> <http://amora.center/kb/amora/Flag/implicit> .""" + "\n")
        }

        decl.attachments.collectFirst {
          case Attachment.JvmSignature(signature) ⇒
            sb.append(s"""  <$path> <$schemaPath/jvmSignature> "$signature" .""" + "\n")
        }

        decl.position match {
          case RangePosition(start, end) ⇒
            sb.append(s"  <$path> <$schemaPath/posStart> $start .\n")
            sb.append(s"  <$path> <$schemaPath/posEnd> $end .\n")
          case _ ⇒
        }

        owner match {
          case Root ⇒
            // The owner of a package is an artifact but this can't be represented
            // in the Hierarchy structure. Thus, we index this information separately.
            if (!decl.attachments(Attachment.Package)) {
              val ownerPath = Schema.mkId(schema)
              sb.append(s"  <$path> <$schemaPath/owner> <$ownerPath> .\n")
            }
          case owner: Decl ⇒
            val ownerPath = mkOwnerPath(decl, owner)
            sb.append(s"  <$path> <$schemaPath/owner> <$ownerPath> .\n")

            loop(owner)
          case _: Ref ⇒
          case _: Scope ⇒
        }

      case ref @ Ref(name, refToDecl, owner, qualifier) ⇒
        val declPath = refToDecl match {
          case d: Decl ⇒ mkFullPath(d)
          // TODO replace this with a real implementation
          case _ ⇒ "???"
        }
        val path = s"$declPath/${Schema.mkShortId(schema)}${uniqueRef(ref.position)}"
        val schemaPath = s"http://amora.center/kb/amora/Schema/Ref"
        sb.append(s"  <$path> a <$schemaPath/> .\n")
        sb.append(s"""  <$path> <$schemaPath/name> "$name" .""" + "\n")
        sb.append(s"  <$path> <$schemaPath/refToDecl> <$declPath> .\n")

        ref.position match {
          case RangePosition(start, end) ⇒
            sb.append(s"  <$path> <$schemaPath/posStart> $start .\n")
            sb.append(s"  <$path> <$schemaPath/posEnd> $end .\n")
          case _ ⇒
        }

        owner match {
          case Root ⇒
            val ownerPath = Schema.mkDefn(schema)
            sb.append(s"  <$path> <$schemaPath/owner> <$ownerPath> .\n")
          case owner: Decl ⇒
            val ownerPath = mkOwnerPath(ref, owner)
            sb.append(s"  <$path> <$schemaPath/owner> <$ownerPath> .\n")

            loop(owner)
          case _: Ref ⇒
          case _: Scope ⇒
        }

      case _: Scope ⇒
    }

    sb.append("INSERT DATA {\n")
    data foreach loop
    sb.append("}")
    sb.toString
  }

  private def uniqueRef(pos: Position) = pos match {
    case RangePosition(start, _) ⇒
      s"/$start"
    case _ ⇒
      ""
  }

  private def encode(str: String): String =
    URLEncoder.encode(str, "UTF-8")

  private def mkShortPath(decl: Decl) = {
    val Decl(name, owner) = decl
    val n = encode(name)
    val ownerPath = owner match {
      case Root ⇒
        ""
      case _: Decl | _: Scope ⇒
        encode(owner.asString).replace('.', '/')
      case _: Ref ⇒
        val path = encode(owner.asString).replace('.', '/')
        val h = uniqueRef(owner.position)
        s"$path/$h"
    }
    val sig = decl.attachments.collectFirst {
      case Attachment.JvmSignature(signature) ⇒ encode(signature)
    }.getOrElse("")
    val paramAtt = encode(decl.attachments.collectFirst {
      case Attachment.Param ⇒ "<param>"
      case Attachment.TypeParam ⇒ "<tparam>"
    }.getOrElse(""))
    val originPath = if (ownerPath.isEmpty) "" else ownerPath + "/"
    val fullPath = s"$originPath$paramAtt$n$sig"
    fullPath
  }
}
