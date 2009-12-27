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
					<description>Ich mag Ršesti</description>
				</photoset>
				</photosets>"""))
		photosets should have ('length (2))
		photosets should have ('cancreate (true))
		photosets(0) should have ('id (847293749204829L))
		photosets(1) should have ('id (94884738293746L))
	}
}
