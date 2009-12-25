package net.tegla.flickr

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		println(flickr.auth.getToken.method)
	}
}
