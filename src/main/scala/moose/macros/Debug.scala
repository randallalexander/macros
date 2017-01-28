package moose.macros

import autocomplete._
import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("debug macros not expanded")
class debug extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn: Defn.Def =>
        val printlnStatements = defn.paramss.flatten.map { param =>
          q"""println(
                ${param.name.syntax} + ": " +
                ${Term.Name(param.name.value)})"""
        }
        val body: Term = q"""
          { ..$printlnStatements }
          val start = _root_.java.lang.System.currentTimeMillis()
          val result = ${defn.body}
          val elapsed = _root_.java.lang.System.currentTimeMillis() - start
          println("Method " + ${defn.name.syntax} + " ran in " + elapsed + "ms")
          result
          """
        defn.copy(body = body)
      case _ =>
        abort(s"@debug most annotate a def:${defn}")
    }
  }
}

@compileTimeOnly("debug macros not expanded")
class paramLog extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn: Defn.Def =>
        val params = defn.paramss.flatten.map {
          param =>
            q"""print(
               ${Term.Name(param.name.value)}
              )"""
        }
        val methodName = defn.name.syntax
        val body: Term = q"""
          {
            print($methodName + "(")
            ..$params
            println(")")
          }
          ${defn.body}
          """
        defn.copy(body = body)
      case _ =>
        abort(s"@debug most annotate a def:${defn}")
    }
  }
}