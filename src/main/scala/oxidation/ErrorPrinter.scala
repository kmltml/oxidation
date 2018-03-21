package oxidation

import tool.Compile.{ ParseError, ValidatorError, AssemblerError, LinkerError, AnalysisErrors }
import tool.TypedAstPrettyprint.{ prettyPrintType, prettyprintTypeName }
import parse.{ IndexTranslator, Span }
import analyze.{ TyperError, ExpectedType, Type, MatchSet }

import cats._
import cats.data._
import cats.implicits._

object ErrorPrinter {

  def showSymbol(sym: Symbol): String = sym match {
    case Symbol.Local(name) => s"Local $name"
    case Symbol.Global(parts) => s"Global ${parts mkString ","}"
    case Symbol.Unresolved(parts) => s"Unresolved ${parts mkString ","}"
  }

  def showMatchSet(ms: MatchSet): Vector[String] = ms match {
    case MatchSet.Any(_) => Vector("_")
    case MatchSet.Sum(sets) => sets.flatMap(showMatchSet).toVector
    case MatchSet.Empty => Vector("()")
    case MatchSet.Bool(member) => Vector(member.toString)
    case MatchSet.Struct(members) =>
      Vector(members.map { case (n, ms) => s"n = ${showMatchSet(ms) mkString " | "}" }.mkString("{ ", ", ", " }"))
    case MatchSet.Enum(variants) =>
      variants.map {
        case (n, MatchSet.Struct(Nil)) => n
        case (n, s) => s"$n ${showMatchSet(s).head}"
      }.toVector
  }

  def printError(error: CompileError)(implicit indexTranslator: IndexTranslator): Unit = {
    import Console.err
    error match {
      case ParseError(msg) =>
        err.println("[ERROR] Parsing error: ")
        err.println(msg)
      case ValidatorError(error, after) =>
        err.println(show"[ERROR] Ir validation error after pass $after: ")
        err.println(error) // TODO better errors here
      case AssemblerError(message) =>
        err.println("[ERROR] Assembler error: ")
        err.println(message)
      case LinkerError(message) =>
        err.println("[ERROR] Linker error: ")
        err.println(message)
      case AnalysisErrors(errors) =>
        def header(loc: Span): Unit = {
          err.println(show"[ERROR] $loc")
        }
        def line(l: String): Unit = err.println("   " + l)
        errors.toList.foreach {
          case TyperError.CantMatch(expected, found, loc) =>
            header(loc)
            line("Can't match types:")
            val e = expected match {
              case ExpectedType.Undefined => "Any"
              case ExpectedType.Appliable => "Appliable"
              case ExpectedType.Value => "Any value type"
              case ExpectedType.Numeric(Some(tpe)) => s"Numeric wider than ${prettyPrintType(tpe)}"
              case ExpectedType.Numeric(None) => "Numeric"
              case ExpectedType.Specific(tpe) => prettyPrintType(tpe)
            }
            line(s" Expected: $e")
            val f = prettyPrintType(found)
            line(s" Found: $f")
          case TyperError.SymbolNotFound(symbol, loc) =>
            header(loc)
            line(s"Symbol not found: ${showSymbol(symbol)}")
          case TyperError.WrongNumberOfArguments(expected, found, loc) =>
            header(loc)
            line(s"Wrong number of arguments, expected: $expected, got: $found")
          case TyperError.MemberNotFound(name, in, loc) =>
            header(loc)
            line(s"There's no member $name in type:")
            line(s" ${prettyPrintType(in)}")
          case TyperError.NotAStruct(typ, loc) =>
            header(loc)
            line(s"Struct literal requires a struct constructor, found:")
            line(s" ${prettyPrintType(typ)}")
          case TyperError.WrongStructMembers(expected, found, loc) =>
            header(loc)
            line(s"Wrong members in struct literal.")
            val overspecified = found diff expected
            if(overspecified.nonEmpty) {
              line(s"Extraneous members: ${overspecified mkString ","}")
            }
            val underspecified = expected diff found
            if(underspecified.nonEmpty) {
              line(s"Unspecified members: ${underspecified mkString ","}")
            }
          case TyperError.ImmutableAssign(symbol, loc) =>
            header(loc)
            line(s"Tried to assign to immutable variable: ${showSymbol(symbol)}")
          case TyperError.NotAnLVal(expr, loc) =>
            header(loc)
            line("The expression cannot be assigned to")
          case TyperError.TooManyParamsForPointerDereference(found, loc) =>
            header(loc)
            line(s"Pointers can only be dereferenced with zero or one parameter, found $found")
          case TyperError.ExternNoExplicitType(loc) =>
            header(loc)
            line("Extern functions cannot have inferred return type.")
          case TyperError.NotASingletonType(found) =>
            err.println(s"[ERROR] ${prettyprintTypeName(found)} is not a singleton type.")
          case TyperError.NonexhaustivePatternMatch(unhandled, loc) =>
            header(loc)
            line("Unhandled patterns:")
            showMatchSet(unhandled).foreach(l => line(s" $l"))
          case TyperError.AlternativePatternBindingsMismatch(leftBindings, rightBindings, loc) =>
            header(loc)
            line("Both alternatives in the | pattern have to match the same variables.")
            val colLeft = leftBindings diff rightBindings
            val colRight = rightBindings diff leftBindings
            if(colLeft.nonEmpty) {
              line("Colliding variables in the left alternative:")
              colLeft.foreach { case (n, term) =>
                line(s" ${n.name}: ${prettyPrintType(term.typ)}")
              }
            }
            if(colRight.nonEmpty) {
              line("Colliding variables in the right alternative:")
              colRight.foreach { case (n, term) =>
                line(s" ${n.name}: ${prettyPrintType(term.typ)}")
              }
            }
          case TyperError.NoVariantSpecified(loc) =>
            header(loc)
            line("Variant has to be specified when matching an enum.")
          case TyperError.VariantNotFound(enum, name, loc) =>
            header(loc)
            line(s"Not a valid enum variant name: ${name.name}.")
            line(s"Available variants are: ${enum.variants.map(_.name.name).mkString(", ")}")

          case error => err.println("[ERROR] " + error.toString)
        }
    }
  }

}
