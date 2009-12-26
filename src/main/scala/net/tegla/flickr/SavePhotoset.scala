package net.tegla.flickr

object SavePhotoset {
	def download(source:String, target:java.io.File) {
		println(source)
		println(target)
	}

	def main(args : Array[String]) : Unit = {
		val photoset_id = args(0).toLong
		val flickr = Flickr.ProbaApp
		val pwd = new java.io.File(System.getProperty("user.dir"))

		// first, figure out the directory to write the files
		val photoset = flickr.photosets.getInfo(photoset_id)
		val target_dir = new java.io.File(pwd, photoset.title.replace('/',' '))
		if (!target_dir.exists) {
			target_dir.mkdir()
			println("Creating " + target_dir)
			assert(target_dir.exists)
		}

		val photos = flickr.photosets.getPhotos(args(0).toLong)
		var i = 1
		photos.foreach { photo =>
			val target = new java.io.File(target_dir, "%05d.jpg".format(i))
			val source = flickr.photos.getSizes(photo)("Large").source
			download(source, target)
			i += 1
		}
	}
}
