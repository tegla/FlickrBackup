package net.tegla.flickr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class SavePhotosetSpec extends Spec with ShouldMatchers {
	// these tests are a bit over-zealous, I got carried away with trying out BDD

	describe ("an empty directory") {
		val dirlist = List[String]()
		describe ("with an empty photoset") {
			val ids = List[Long]()
			val commands = SavePhotoset.resolve(dirlist, ids)
			it("should delete nothing") { commands.deleteFiles should be ('empty) }
			it("should move nothing") { commands.moveFiles should be ('empty) }
			it("should download nothing") { commands.downloadIds should be ('empty) }
		}

		describe ("with a 2-piece photoset") {
			val ids = List(10L, 20L)
			val commands = SavePhotoset.resolve(dirlist, ids)
			it("should delete nothing") { commands.deleteFiles should be ('empty) }
			it("should move nothing") { commands.moveFiles should be ('empty) }
			it("should download both") {
				commands.downloadIds should be (Map(
					1 -> 10L,
					2 -> 20L
				))
			}
		}

		describe ("with 3-piece photoset") {
			def downloadIds(ids:List[Long]) = SavePhotoset.resolve(dirlist, ids).downloadIds
			it ("should download permutation 123") {
				downloadIds(List(10L,20L,30L)) should be (Map(1 -> 10L, 2->20L, 3-> 30L))
			}
			it ("should download permutation 213") {
				downloadIds(List(20L,10L,30L)) should be (Map(1 -> 20L, 2->10L, 3-> 30L))
			}
			it ("should download permutation 312") {
				downloadIds(List(30L,10L,20L)) should be (Map(1 -> 30L, 2->10L, 3-> 20L))
			}
		}
	}

	describe("a 2-piece directory") {
		val dirlist = List("00001_10.jpg", "00002_20.jpg")
		describe("with empty photoset") {
			val commands = SavePhotoset.resolve(dirlist, List())
			it ("should delete everything") {
				commands.deleteFiles.sort(_<_) should be (dirlist)
			}
			it("should move nothing") { commands.moveFiles should be ('empty) }
			it("should download nothing") { commands.downloadIds should be ('empty) }
		}
		
		describe("and a photoset that has one more added") {
			val commands = SavePhotoset.resolve(dirlist, List(10,20,30))
			it("should delete nothing") { commands.deleteFiles should be ('empty) }
			it("should move nothing") { commands.moveFiles should be ('empty) }
			it("should download that one") { commands.downloadIds should be (Map(3->30L)) }
		}
		
		describe("and a photoset that has one more added at the beginning") {
			val commands = SavePhotoset.resolve(dirlist, List(30,10,20))
			it("should delete nothing") { commands.deleteFiles should be ('empty) }
			it("should move all one forward") {
				commands.moveFiles should be (Map(
						"00001_10.jpg" -> "00002_10.jpg",
						"00002_20.jpg" -> "00003_20.jpg"))
			}
			it("should download that one") { commands.downloadIds should be (Map(1->30L)) }
		}
	}
}
