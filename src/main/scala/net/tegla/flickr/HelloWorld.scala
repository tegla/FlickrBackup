package net.tegla.flickr
import java.net.URL
import scala.xml.XML

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		val photosets = flickr.getPhotoSets(None)
		for(photoset <- photosets) {
			println(photoset.id + ": " + photoset.title)
		}
	}
}
