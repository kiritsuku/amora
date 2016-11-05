package amora.backend.indexer

import org.junit.Test

import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class ScalaSourceRegionIndexerTest extends RestApiTest {

  @Test
  def classes() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Class/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class [[A]]
        class [[B_?]]
        class [[??]]
        class [[`hello world`]]
        object O
        abstract class AC
      """)
  }

  @Test
  def abstract_classes() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/AbstractClass/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class A
        object O
        abstract class [[AC]] {}
      """)
  }

  @Test
  def classes_with_body() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Class/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class [[A]] {}
        class [[B_?]] {
          def f = 0
        }
        class [[??]] { /* comment*/ }
        class [[`hello world`]] {
          def g = 0
        }
      """)
  }

  @Test
  def objects() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Object/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        object [[`x y`]] {
          def g = 0
        }
      """)
  }

  @Test
  def traits() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Trait/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        trait [[B]] {}
      """)
  }

  @Test
  def packages() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Package/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package [[a]].[[b]].[[c]]
        class A
      """)
  }

  @Test
  def defs() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]]A {
          def [[meth]] = 0
          def [[meth2]] = {
            def [[meth3]] = {
              def [[meth4]] = 0
              meth4
            }
            meth3
          }
        }
      """)
  }

  @Test
  def vals() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          val [[v1]] = 0
          val [[v2]] = {
            val [[v3]] = {
              val [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def vars() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Var/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          var [[v1]] = 0
          var [[v2]] = {
            var [[v3]] = {
              var [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def lazy_vals() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/LazyVal/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          lazy val [[v1]] = 0
          lazy val [[v2]] = {
            lazy val [[v3]] = {
              lazy val [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def private_class_parameters() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select distinct ?name ?start ?end where {
          [c:flag "param"] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A([[value1]]: Int, [[`second val`]]: String)
      """)
  }

  @Test
  def method_parameters() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select * where {
          [c:flag "param"] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f([[param1]]: Int)([[`p a r a m`]]: String)([[p]]: Int) = 0
        }
      """)
  }

  @Test
  def multiple_lambda_decls() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f([[i]]: Int ⇒ Int) = i
          f([[v]] ⇒ v)
          f([[v]] ⇒ v)
        }
      """)
  }

  @Test
  def default_constructor() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]]
      """)
  }

  @Test
  def constructor_with_parameter() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select distinct * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]]([[value]]: Int)
      """)
  }

  @Test
  def auxiliary_constructor() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]] {
          def [[this]]([[value]]: Int) {
            this()
          }
        }
      """)
  }

  // ====================================================================
  // Ref tests
  // ====================================================================

  @Test
  def return_type_at_members() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val a: [[Int]] = 0
          var b: [[Int]] = 0
          def c: [[Int]] = 0
          lazy val d: [[Int]] = 0
        }
      """)
  }

  @Test
  def return_type_at_nested_members() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def x: [[Int]] = {
            val a: [[Int]] = {
              val a: [[Int]] = 0
              [[a]]
            }
            var b: [[Int]] = {
              var a: [[Int]] = 0
              [[a]]
            }
            def c: [[Int]] = {
              def a: [[Int]] = 0
              [[a]]
            }
            [[a]]
          }
        }
      """)
  }

  @Test
  def return_type_at_nested_lazy_vals() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          lazy val a: [[Int]] = {
            lazy val a: [[Int]] = {
              lazy val a: [[Int]] = 0
              [[a]]
            }
            [[a]]
          }
        }
      """)
  }

  @Test
  def member_ref() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val [[!Int]]a = 0
          def [[!Int]]b = [[a]]
          var [[!Int]]c = [[b]]
          lazy val [[!Int]]d = [[c]]
        }
      """)
  }

  @Test
  def classOf_ref() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val [[!Class]]a = [[classOf]][ /* Int */ [[Int]] ]
        }
      """)
  }

  @Test
  def refs_of_imports() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        import [[scala]].[[collection]].[[mutable]].[[Buffer]]
        import [[scala]].[[collection]].[[mutable]].[[ListBuffer]]
        class [[!AnyRef]]X {
          [[Buffer]]
          [[ListBuffer]]
        }
      """)
  }

  @Test
  def refs_of_rename_imports() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        import [[scala]].[[collection]].[[mutable]].{ [[Buffer]] ⇒ [[B]], [[ListBuffer]] }
        class [[!AnyRef]]X {
          [[B]]
          [[ListBuffer]]
        }
      """)
  }

  @Test
  def self_ref_with_fully_qualified_name() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait [[!AnyRef]]X {
          self: [[scala]].[[collection]].[[mutable]].[[AbstractSet]][ [[java]].[[io]].[[File]] ] ⇒
        }
      """)
  }

  @Test
  def refs_in_if_expr() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val [[!Boolean]]b1 = true
          val [[!Boolean]]b2 = true
          val [[!Boolean]]b3 = true
          def [[!Boolean]]f = if ([[b1]]) [[b2]] else [[b3]]
        }
      """)
  }

  @Test
  def refs_of_single_method() = {
    indexRegionData("""
        prefix def:<http://amora.center/kb/amora/Schema/Def/>
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          ?def def:jvmSignature "(IF)I" .
          [a ref:] ref:owner ?def ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = i
          def [[!Int]]f(i: Int, s: Float) = [[i]]
        }
      """)
  }

  @Test
  def refs_of_parameter() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a ref:] ref:refToDecl [decl:flag "param"] ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = {
            [[i]]
          }
        }
      """)
  }

  @Test
  def refs_of_local_value_with_same_name_as_parameter() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a ref:] ref:refToDecl [decl:flag "param"] ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = {
            val i = 0
            // i doesn't point to the parameter
            i
          }
        }
      """)
  }

  @Test
  def refs_to_local_value_when_parameter_of_same_name_exists() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix v:<http://amora.center/kb/amora/Schema/Val/>
        select * where {
          ?val a v: .
          FILTER NOT EXISTS {
            ?val v:flag "param" .
          }
          [a ref:] ref:refToDecl ?val ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = {
            val i = 0
            [[i]]
          }
        }
      """)
  }

  @Test
  def refs_of_type_parameter() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a ref:] ref:refToDecl [decl:flag "tparam"] ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait X[A] {
          def f[B](a: [[A]], b: [[B]]): [[A]]
        }
      """)
  }

  @Test
  def refs_of_type_parameter_without_shadowed_type_parameter_refs() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        prefix t:<http://amora.center/kb/amora/Schema/Trait/>
        select * where {
          # find type parameter
          ?tparam decl:owner [a t:] ; decl:flag "tparam" .
          # find references of type parameter
          [a ref:] ref:refToDecl ?tparam ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait X[A] {
          def f(a: [[A]], b: [[A]]): [[A]]
          def f[A](a: A): A
        }
      """)
  }

  @Test
  def refs_of_shadowed_type_parameter() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        prefix def:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          # find type parameter
          ?tparam decl:owner [a def:; def:jvmSignature "(Ljava/lang/Object;)Ljava/lang/Object;"] ; decl:flag "tparam" .
          # find references of type parameter
          [a ref:] ref:refToDecl ?tparam ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait X[A] {
          def f(a: A, b: A): A
          def f[A](a: [[A]]): [[A]]
        }
      """)
  }

  @Test
  def refs_of_type_parameter_when_parameter_of_same_name_exists() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        prefix def:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          # find type parameter
          ?tparam decl:owner [a def:] ; decl:flag "tparam" .
          # find references of type parameter
          [a ref:] ref:refToDecl ?tparam ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def [[!A]]f[A](A: [[A]]) = {
            A
          }
        }
      """)
  }

  @Test
  def refs_of_type_parameter_when_local_val_decl_of_same_name_exists() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        prefix def:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          # find type parameter
          ?tparam decl:owner [a def:] ; decl:flag "tparam" .
          # find references of type parameter
          [a ref:] ref:refToDecl ?tparam ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f[A](A: [[A]]) = {
            val A = 0
            A
          }
        }
      """)
  }

  @Test
  def multiple_calls_to_def() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def [[!Int]]f(i: [[Int]]) = 0
          [[f]](0)
          [[f]](0)
        }
      """)
  }

  @Test
  def multiple_blocks_with_same_name() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def [[!Int]]f(i: [[Int]]) = 0
          [[f]]({val [[!Int]]i = 0; [[i]]})
          [[f]]({val [[!Int]]i = 0; [[i]]})
        }
      """)
  }

  @Test
  def explicit_apply_method() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val [[!Option]]a = [[Option]].[[!Int]][[apply]](1)
        }
      """)
  }

  @Test
  def implicit_apply_method() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          val [[!Option]]a = [[!apply]][[!Int]][[Option]](1)
        }
      """)
  }

  @Test
  def class_annotation() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        @[[Ann]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        class [[!AnyRef]]X
        class Ann(arr: [[Array]][ [[Class]] [_] ]) extends [[scala]].[[annotation]].[[StaticAnnotation]]
      """)
  }

  @Test
  def multiple_annotations() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        @[[Ann1]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        @[[Ann2]]
        @[[Ann1]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        class [[!AnyRef]]X
        class Ann1(arr: [[Array]][ [[Class]] [_] ]) extends [[scala]].[[annotation]].[[StaticAnnotation]]
        class Ann2 extends [[scala]].[[annotation]].[[StaticAnnotation]]
      """)
  }

  @Test
  def refs_of_lambda_decl() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def [[!Function1]]f([[!Function1]]i: [[Int]] ⇒ [[Int]]) = [[i]]
        }
      """)
  }

  @Test
  def refs_of_function_decl() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def [[!Function1]]f([[!Function1]]i: [[Function1]][ [[Int]], [[Int]] ]) = [[i]]
        }
      """)
  }

  @Test
  def multiple_lambda_refs() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!AnyRef]]X {
          def [[!Function1]]f([[!Function1]]i: [[Int]] ⇒ [[Int]]) = [[i]]
          [[f]]([[!Int]]v ⇒ [[v]])
          [[f]]([[!Int]]value ⇒ [[value]])
        }
      """)
  }

  @Test
  def ref_with_qualifier() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "f1.scala" → """
        package a.b
        import [[d]].[[e]]
        class [[!AnyRef]]X {
          val f: [[e]].[[Y]] = null
        }
      """,
      "f2.scala" → """
        package d.e
        class [[!AnyRef]]Y
      """)
  }

  @Test
  def ref_to_val_from_within_another_val() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a ref:] ref:name "xs" ; ref:refToDecl [decl:name ?name ; decl:posStart ?start ; decl:posEnd ?end] .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          val [[xs]] = 0
          val ys = xs
        }
      """)
  }

  @Test
  def this_ref_points_to_class() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a ref:] ref:name "this" ; ref:refToDecl [decl:name ?name ; decl:posStart ?start ; decl:posEnd ?end] .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[X]] {
          val value = this
        }
      """)
  }

  @Test
  def this_ref_has_correct_position() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
        select * where {
          [a ref:] ref:name "this" ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          val value = [[this]]
        }
      """)
  }
}
