package amora.converter.protocol

trait Schema
final case class Project(name: String) extends Schema
final case class Artifact(owner: Project, organization: String, name: String, version: String) extends Schema
final case class File(owner: Schema, name: String) extends Schema
final case class Package(name: String, owner: Schema) extends Schema
final case class Class(name: String, owner: Schema) extends Schema
final case class AbstractClass(name: String, owner: Schema) extends Schema
final case class Object(name: String, owner: Schema) extends Schema
final case class Trait(name: String, owner: Schema) extends Schema
final case class Def(name: String, owner: Schema) extends Schema
final case class Val(name: String, owner: Schema) extends Schema
final case class Var(name: String, owner: Schema) extends Schema
final case class LazyVal(name: String, owner: Schema) extends Schema
