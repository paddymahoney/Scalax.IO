import scalax.testing._
import scalax.logging._

object LoggingTests extends TestSuite("Logging") {
	"loggerName" is {
		assert("org.something.Main" == Logging.loggerNameForClass("org.something.Main"))
		assert("org.something.Main" == Logging.loggerNameForClass("org.something.Main$"))
	}
}

// vim: set ts=4 sw=4 noet:
