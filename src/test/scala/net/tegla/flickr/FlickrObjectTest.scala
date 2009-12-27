package net.tegla.flickr

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import scala.xml.XML

class FlickrObjectTest extends FunSuite {
	test("Photosets") {
		val photosets = new Photosets(XML.loadString(
				"""<photosets cancreate="1">
				<photoset videos="0" photos="25" farm="3" server="2678" secret="aaaaaaaaaa" primary="4334643" id="847293749204829">
					<title>Fooobar</title>
					<description></description>
				</photoset>
				<photoset videos="0" photos="8" farm="3" server="2628" secret="bbbbbbbbbb" primary="54363245" id="94884738293746">
					<title>Barfooo</title>
					<description>Ich mag Roesti</description>
				</photoset>
				</photosets>"""))
		photosets should have ('length (2))
		photosets should have ('cancreate (true))
		photosets(0) should have ('id (847293749204829L))
		photosets(1) should have ('id (94884738293746L))
	}

	test("Photo comparison works") {
		val photo1 = new Photo(XML.loadString("""<photo id="1234"/>"""))
		val photo2 = new Photo(XML.loadString("""<photo id="1234"/>"""))
		val photo3 = new Photo(XML.loadString("""<photo id="12345"/>"""))
		val photoset1 = new Photoset(XML.loadString("""<photoset id="1234"/>"""))
		val photoset2 = new Photoset(XML.loadString("""<photoset id="1234"/>"""))

		photo1 should have ('hashCode (photo2.hashCode))
		photo1 should not have ('hashCode (photo3.hashCode))

		photo1 should equal (photo2)
		photo1 should not equal (photo3)
		photoset1 should equal (photoset2)
		photo1 should not equal (photoset1)
	}

	test("Photoset from photosets") {
		val photoset = new Photoset(XML.loadString(
			"""<photoset videos="0" photos="42" farm="3" server="2207" secret="9999999999" primary="1638510036" id="72157602576373121">
				<title>2007.09.30. Schaffhausen</title>
				<description></description>
			</photoset>"""))
		photoset should have ('videos (0))
		photoset should have ('photos (42))
		photoset should have ('farm (3))
		photoset should have ('server (2207))
		photoset should have ('secret ("9999999999"))
		photoset should have ('primary (1638510036L))
		photoset should have ('id (72157602576373121L))
	}

	test("Photoset from getPhotos") {
		val photoset = new PhotosetList(XML.loadString(
			"""<photoset total="42" pages="5" perpage="10" per_page="10" page="1" ownername="tegla" owner="10686481@N00" primary="1638510036" id="72157602576373121">
				<photo isprimary="0" title="HPIM1165.JPG" farm="3" server="2230" secret="9999999999" id="1638384240"></photo>
				<photo isprimary="0" title="HPIM1166.JPG" farm="3" server="2180" secret="9999999999" id="1637516201"></photo>
			</photoset>"""))
		photoset should have ('length (2))
		photoset(0) should have ('id (1638384240L))
		photoset(1) should have ('id (1637516201L))
		photoset should have ('pages (5))
		photoset should have ('page (1))
		photoset should have ('ownername ("tegla"))
		photoset should have ('owner ("10686481@N00"))
		photoset should have ('primary (1638510036L))
		photoset should have ('id (72157602576373121L))
	}

	test("Sizes") {
		val sizes = new Sizes(XML.loadString(
			"""<sizes candownload="1" canprint="1" canblog="1">
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/sq/" source="http://farm3.static.flickr.com/2260/1638461734_9ec8669f35_s.jpg" height="75" width="75" label="Square"></size>
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/t/" source="http://farm3.static.flickr.com/2260/1638461734_9ec8669f35_t.jpg" height="75" width="100" label="Thumbnail"></size>
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/s/" source="http://farm3.static.flickr.com/2260/1638461734_9ec8669f35_m.jpg" height="179" width="240" label="Small"></size>
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/m/" source="http://farm3.static.flickr.com/2260/1638461734_9ec8669f35.jpg" height="373" width="500" label="Medium"></size>
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/l/" source="http://farm3.static.flickr.com/2260/1638461734_9ec8669f35_b.jpg" height="763" width="1024" label="Large"></size>
				<size media="photo" url="http://www.flickr.com/photos/10686481@N00/1638461734/sizes/o/" source="http://farm3.static.flickr.com/2260/1638461734_4340d0a3df_o.jpg" height="1920" width="2576" label="Original"></size>
			</sizes>"""))
		sizes should have ('candownload (true))
		sizes should have ('canprint (true))
		sizes should have ('canblog (true))
		val keyset = Set() ++ sizes.keySet
		keyset should be (Set("Square","Thumbnail","Small","Medium","Large","Original"))
		val large = sizes("Large")
		large should have ('label ("Large"))
		large should have ('media ("photo"))
		large should have ('url ("http://www.flickr.com/photos/10686481@N00/1638461734/sizes/l/"))
		large should have ('source ("http://farm3.static.flickr.com/2260/1638461734_9ec8669f35_b.jpg"))
		large should have ('height (763))
		large should have ('width (1024))
	}
}
