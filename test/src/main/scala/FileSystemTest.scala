package test.io

import java.nio.file.Path
import java.nio.file.Files
import java.util.stream.Collectors

import com.todesking.scalanb.io.FileSystem

import scala.collection.JavaConverters._

trait FileSystemBasicTest extends org.scalatest.FunSpec {
  def withFS(f: FileSystem => Unit): Unit

  describe("FileSystem") {
    it("should access to target FS")(withFS { fs =>
      assert(fs.list("").isEmpty)
      fs.writeString("foo", "this is foo")
      fs.writeString("bar/1", "this is bar/1")
      assert(fs.exists("foo"))
      assert(fs.exists("bar/1"))
      assert(fs.list("").sorted == Seq("bar", "foo"))
      assert(fs.readString("foo") == "this is foo")
      assert(fs.readString("bar/1") == "this is bar/1")
    })
  }
}

object FileSystemTestUtil {
  def rm_r(p: Path): Unit = {
    if (Files.isDirectory(p)) {
      Files.list(p).collect(Collectors.toList()).asScala.foreach(rm_r)
    }
    Files.delete(p)
  }

  def withTmpDir(f: Path => Unit): Unit = {
    val tmp = Files.createTempDirectory("scalanb_test")
    try {
      f(tmp)
    } finally {
      rm_r(tmp)
    }
  }
}
