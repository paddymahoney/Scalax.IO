package scalax.io

trait URI {
	def authority: String
	def rawAuthroty: String
	def fragment: String
	def rawFragment: String
	def host: String
	def path: String
	def rawPath: String
	def port: String
	def query: String
	def rawQuery: String
	def scheme: String
	def schemeSpecificPart: String
	def isAbsolute: Boolean
	def isOpaque: Boolean
	def normalize: URI
	def parseServerAuthority: URI
	def relative(uri: URI): URI
	def resolve(str: String): URI
	def resolve(uri: URI): URI
	def toASCII: String
}
