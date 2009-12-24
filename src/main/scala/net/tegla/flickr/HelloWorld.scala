package net.tegla.flickr
import java.net.URL
import scala.xml.XML

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp(Some("72157622931806485-5285ff7f60695ef3"))
		val loginLink = flickr.getLoginLink(
			"72157622939479697-6776e136f19598ce-971749",
			"read")
		println(loginLink)
		val auth = flickr.getToken(
			"72157622939479697-6776e136f19598ce-971749")
		println(auth)
	}
}
