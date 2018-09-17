package test.spark.io

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.spark.io.HdfsFileSystem

import test.io.FileSystemBasicTest
import test.io.FileSystemTestUtil

class HdfsTest extends org.scalatest.FunSpec with FileSystemBasicTest {
  import FileSystemTestUtil.withTmpDir
  override def withFS(f: FileSystem => Unit): Unit = withTmpDir { tmp =>
    val fs = new HdfsFileSystem(s"file://${tmp.toString}")
  }
}
