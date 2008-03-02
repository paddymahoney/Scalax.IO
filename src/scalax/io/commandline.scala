// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.io
import java.io._
import scala.collection.immutable
import scala.collection.mutable._
import scalax.data._
import scalax.data.Implicits._

/** A utility for parsing GNU-style command-line arguments.
 *
 * <p>Example usage:</p>
 *
 * <pre>
 * object Options extends CommandLineParser {
 *     val exclude = new StringOption('x', "exclude", "Exclude the given file") with AllowAll
 *     val version = new Flag("version", "Show version info") with AllowNone
 *     override def helpHeader = """
 *         |  SomeTool v0.1
 *         |  (c) 1908 SomeCorp
 *         |
 *         |""".stripMargin
 * }
 *
 * Options.parseOrHelp(argv) { cmd =&gt;
 *     if(cmd(Options.version)) {
 *         ...
 *     }
 * }
 * </pre>
 */
class CommandLineParser {
	/** Override this to refuse non-option arguments. */
	def permitNonOptions = true

	/** Parsed representation of the command line. */
	class Result(val arguments : List[Argument]) {
		private val flags = new HashMap[Flag, Int]
		private val vopts = new PolyHashMap[OptionType, ListBuffer]
		private val nonOpts = new ListBuffer[String]
		arguments.foreach {
			case f : Flag =>
				flags(f) = flags.get(f).getOrElse(0) + 1
			case o : OptionValue[t] =>
				vopts.get(o.option) match {
					case Some(l) => l += o.value
					case None =>
						val l = new ListBuffer[t]
						l += o.value
						vopts(o.option) = l
				}
			case n : NonOption =>
				nonOpts += n.value
		}

		/** List of non-option argument values. */
		val nonOptions = nonOpts.toList

		/** Returns the number of times the given flag was used. */
		def all(f : Flag) : Int = flags.get(f).getOrElse(0)

		/** Returns all the values supplied with this option. */
		def all[A](o : OptionType[A]) : List[A] =
			vopts.get(o) match {
				case Some(l) => l.toList
				case None => Nil
			}

		/** Was the given flag supplied? */
		def apply(f : Flag) : Boolean = all(f) > 0

		/** Returns the first value for the given option. */
		def apply[A](o : OptionType[A]) : Option[A] = all(o).headOption
	}

	/** Interface for the actual received arguments. */
	sealed trait Argument {
		/** The OptionSpec for this argument, or None if this is a
		 * non-option. */
		def spec : Option[OptionSpec]
	}

	/** Superclass of the specifications of permitted options. */
	sealed abstract class OptionSpec {
		def shortName : Option[Char]
		def longName : String
		def description : String

		/** Specifies which other options may be used in conjunction with this
		 * one. This option must appear in its own allow list if it is allowed
		 * to appear more than once. */
		def allow : collection.Set[OptionSpec]

		// Voodoo to obtain a list of the declared options
		opts += this
		if(longs.contains(longName)) throw new IllegalArgumentException("Duplicate option: --"+longName+".")
		longs(longName) = this
		for(s <- shortName) {
			if(shorts.contains(s)) throw new IllegalArgumentException("Duplicate option: -"+s+".")
			shorts(s) = this
		}
	}
	private val opts = new ListBuffer[OptionSpec]
	private val longs = new HashMap[String, OptionSpec]
	private val shorts = new HashMap[Char, OptionSpec]
	private lazy val allOpts = immutable.HashSet(opts : _*)

	/** Superclass of OptionSpecs which accept a parameter. */
	abstract class OptionType[A] extends OptionSpec {
		def parse(value : String) : Bistate[OptionValue[A], String]
	}

	/** Instantiate one of these for each flag-type option. */
	abstract class Flag(val shortName : Option[Char], val longName : String,
			val description : String) extends OptionSpec with Argument {
		def this(s : Char, l : String, d : String) = this(Some(s), l, d)
		def this(l : String, d : String) = this(None, l, d)
		def spec = Some(this)
	}

	/** Instantiate one of these for each option taking a parameter. */
	abstract class StringOption(val shortName : Option[Char], val longName : String,
			val description : String) extends OptionType[String] {
		def this(s : Char, l : String, d : String) = this(Some(s), l, d)
		def this(l : String, d : String) = this(None, l, d)

		def parse(value : String) = Positive(OptionValue(this, value))
	}

	/** An option variant that insists that the parameter is an integer. */
	abstract class IntOption(val shortName : Option[Char], val longName : String,
			val description : String) extends OptionType[Int] {
		def this(s : Char, l : String, d : String) = this(Some(s), l, d)
		def this(l : String, d : String) = this(None, l, d)

		def parse(value : String) = value.toOptInt match {
			case Some(i) => Positive(OptionValue(this, i))
			case None => Negative("Please supply a valid integer")
		}
	}

	/** Mix this into an option definition to mark it compatible with every
	 * option (including itself). */
	trait AllowAll extends OptionSpec {
		def allow = allOpts
	}

	/** Mix this into an option definition to mark it incompatible with every
	 * option. */
	trait AllowNone extends OptionSpec {
		def allow = Set.empty[OptionSpec]
	}

	/** Mix this in to mark only the specified options as incompatible. */
	trait AllowAllBut extends OptionSpec {
		def allow = allOpts -- exclude
		def exclude : Set[OptionSpec]
	}

	/** Mix this into an option definition to mark it compatible with every
	 * option except itself. */
	trait AllowAllButSelf extends OptionSpec {
		def allow = allOpts - this
	}

	/** An OptionSpec paired with the actual value received. */
	case class OptionValue[A](option : OptionType[A], value : A) extends Argument {
		def spec = Some(option)
	}

	/** A received non-option argument. */
	case class NonOption(value : String) extends Argument {
		def spec = None
	}

	/** Parse the given command line. */
	def parse(argv : Array[String]) : Bistate[Result, String] = {
		// This is all mutually recursive, so the stack might overflow.
		// Hopefully this isn't a problem in practice. The outer loop could be
		// inverted if necessary, but it would make the code uglier.
		type Maybe = Bistate[List[Argument], String]
		def errorJoin(x : Argument, res : Maybe) = res.posMap(x :: _)
		def missingArg(name : String) = Negative("Missing argument for option "+name+".")
		def unrecognised(name : String) = Negative("Unrecognised option: "+name+".")
		def checkParse(name : String, opt : OptionType[_], v : String, res : => Maybe) =
			opt.parse(v) match {
				case Positive(ov) => errorJoin(ov, res)
				case Negative(e) => Negative(e+" for option "+name+".")
			}
		def handleLong(arg : String, rest : List[String]) =
			arg.indexOf("=") match {
				case -1 =>
					// Plain --name
					val name = arg.substring(2)
					longs.get(name) match {
						case Some(opt : OptionType[_]) =>
							rest match {
								case x :: xs if !x.startsWith("-") =>
									checkParse("--"+name, opt, x, top(xs))
								case _ => missingArg("--"+name)
							}
						case Some(opt : Flag) =>
							errorJoin(opt, top(rest))
						case None => unrecognised("--"+name)
					}
				case eqPos =>
					// --name=value
					val name = arg.substring(2, eqPos)
					longs.get(name) match {
						case Some(opt : OptionType[_]) =>
							checkParse("--"+name, opt, arg.substring(eqPos + 1), top(rest))
						case Some(opt : Flag) => missingArg("--"+name)
						case None => unrecognised("--"+name)
					}
			}
		def handleShort(arg : String, i : Int, rest : List[String]) : Maybe =
			if(i < arg.length) {
				val name = arg.charAt(i)
				shorts.get(name) match {
					case Some(opt : OptionType[_]) =>
						if(i + 1 < arg.length) {
							// -xvalue
							checkParse("-"+name, opt, arg.substring(i + 1), top(rest))
						} else {
							// -x value
							rest match {
								case x :: xs if !x.startsWith("-") =>
									checkParse("-"+name, opt, x, top(xs))
								case _ => missingArg("-"+name)
							}
						}
					case Some(opt : Flag) =>
						// -xyz
						errorJoin(opt, handleShort(arg, i + 1, rest))
					case None => unrecognised("-"+name)
				}
			} else {
				top(rest)
			}
		def top(argv : List[String]) : Maybe = argv match {
			case arg :: rest =>
				arg match {
					case "-" =>
						errorJoin(NonOption("-"), top(rest))
					case "--" =>
						Positive(rest.map(NonOption))
					case _ if arg.startsWith("--") =>
						handleLong(arg, rest)
					case _ if arg.startsWith("-") =>
						handleShort(arg, 1, rest)
					case _ =>
						errorJoin(NonOption(arg), top(rest))
				}
			case Nil =>
				Positive(Nil)
		}
		def checkAllowed(args : List[Argument], rest : List[Argument], pos : Int) : Bistate[Result, String] =
			rest match {
				case x :: xs =>
					def loop(rest2 : List[Argument], pos2 : Int) : Bistate[Result, String] = rest2 match {
						case y :: ys => (x.spec, y.spec) match {
							case (Some(xsp), Some(ysp)) if pos != pos2 && !xsp.allow.contains(ysp) =>
								Negative("Cannot combine --"+xsp.longName+" with --"+ysp.longName+".")
							case _ => loop(ys, pos2 + 1)
						}
						case Nil =>
							checkAllowed(args, xs, pos + 1)
					}
					loop(args, 0)
				case Nil =>
					Positive(new Result(args))
			}

		top(argv.toList) match {
			case Positive(args) =>
				if(!permitNonOptions && args.exists(_.spec.isEmpty))
					Negative("This command does not accept any non-option arguments.")
				else
					checkAllowed(args, args, 0)
			case e @ Negative(_) => e
		}
	}

	/** Override this to define a header for the help message. */
	def helpHeader = ""

	/** Parses the command line and shows a help message if parsing fails. */
	def parseOrHelp[A](argv : Array[String])(body : Result => A) : Bistate[A, String] =
		parse(argv) match {
			case Positive(res) =>
				Positive(body(res))
			case Negative(e) =>
				showError(e)
				Negative(e)
		}

	/** Displays a formatted help message. */
	def showHelp(out : PrintStream) : Unit = {
		var width = 0
		for(i <- opts) {
			val w = i.longName.length
			if(w > width) width = w
		}
		width += 2

		out.print(helpHeader)
		for(i <- opts) {
			out.print("  -")
			out.print(i.shortName)
			out.print("  --")
			out.print(i.longName)
			for(j <- Iterator.range(0, width - i.longName.length))
				out.print(' ')
			out.println(i.description)
		}
	}

	/** Displays a formatted help message and an error message to stderr. */
	def showError(msg : String) : Unit = {
		showHelp(System.err)
		System.err.println("\n")
		System.err.println("  "+msg)
		System.err.println
	}
}
