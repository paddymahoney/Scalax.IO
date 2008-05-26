package scalax.logging

import org.slf4j._

trait Logging {
	val logger = Logging.getLogger(this)
}

object Logging {
	def loggerNameForClass(className: String) = {
		if (className endsWith "$") className.substring(0, className.length - 1)
		else className
	}
	
	def getLogger(logging: AnyRef) = LoggerFactory.getLogger(loggerNameForClass(logging.getClass.getName))
}

// vim: set ts=4 sw=4 noet:
