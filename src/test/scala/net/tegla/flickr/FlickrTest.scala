package net.tegla.flickr

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

class FlickrTest extends FunSuite {

	class MockTransport extends Transport {
		override def get(url:String):java.io.InputStream = {
			null
		}
	}

	test("api_sig works, as in flickr docs") {
		val flickr = new Flickr("", "000005fab4534d05", None, null)
		val signature = flickr.sign(Map(
			"method" -> "flickr.auth.getFrob",
			"api_key" -> "9a0554259914a86fb9e7eb014e4e5d52"))
		signature should be ("8ad70cd3888ce493c8dde4931f7d6bd0")		
	}

}

