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

}
