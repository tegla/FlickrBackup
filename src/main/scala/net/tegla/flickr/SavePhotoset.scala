package net.tegla.flickr

object SavePhotoset {
	def download(source:String, target:java.io.File, tmp:java.io.File) {
		print("Downloading " + source +" ")
		val inputStream = new java.net.URL(source).openStream
		try {
			val outputStream = new java.io.FileOutputStream(tmp)
			val bytes = new Array[Byte](4096)
			var i = inputStream.read(bytes)
			while(i > 0) {
				print(".")
				outputStream.write(bytes, 0, i)
				i = inputStream.read(bytes)
			}
			outputStream.close()
			tmp.renameTo(target)
			println
			println("Written as: " + target)
		} finally {
			inputStream.close()
			tmp.delete()
		}
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
			val tmp = new java.io.File(target_dir, "%05d.jpg.downloading".format(i))
			download(source, target, tmp)
			i += 1
		}
	}
}
