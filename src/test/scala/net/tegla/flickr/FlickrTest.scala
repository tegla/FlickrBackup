package net.tegla.flickr

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import scala.xml.XML

class FlickrTest extends FunSuite {

	class MockTransport extends Transport {
		override def get(url:String):java.io.InputStream = {
			null
		}
	}

	test("api_sig works, as in flickr docs") {
		val flickr = new Flickr(null, "000005fab4534d05", null, null)
		val signature = flickr.sign(Map(
			"method" -> "flickr.auth.getFrob",
			"api_key" -> "9a0554259914a86fb9e7eb014e4e5d52"))
		signature should be ("8ad70cd3888ce493c8dde4931f7d6bd0")		
	}

	test("parseResponse can handle failure") {
		val flickr = new Flickr(null, null, null, null)
		val doc = XML.loadString(
			"""<?xml version="1.0" encoding="UTF-8"?>
			<rsp stat="fail">
        			<err code="96" msg="Invalid signature"/>
			</rsp>""")
		val exception = intercept[RuntimeException] {
			flickr.parseResponse(doc, "frob")
		}
		exception should have ('message ("(96,Invalid signature)"))
	}


	test("parseresponse gets frob right") {
		val flickr = new Flickr(null, null, null, null)
		val doc = XML.loadString(
			"""<?xml version="1.0" encoding="UTF-8"?>
			<rsp stat="ok">
				<frob>72157622937385745-e9e798d6cd643682-520711</frob>
			</rsp>""")
		val frob = flickr.parseResponse(doc, "frob")
		frob should be (<frob>72157622937385745-e9e798d6cd643682-520711</frob>)
	}
}

