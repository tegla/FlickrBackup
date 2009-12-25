package net.tegla.flickr
import java.net.URL
import scala.xml.XML

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		val auth = flickr.auth.checkToken(System.getProperty("FLICKR_AUTH_TOKEN"))
		val photosets = flickr.photosets.getList(auth.user)
		println(photosets)
		println(photosets.length)
	}
}
