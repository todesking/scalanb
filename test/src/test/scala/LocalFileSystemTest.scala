package test.io

import com.todesking.scalanb.io.FileSystem
import com.todesking.scalanb.io.LocalFileSystem

class LocalFileSystemTest extends FileSystemBasicTest {
  import FileSystemTestUtil.withTmpDir
  override def withFS(f: FileSystem => Unit): Unit = withTmpDir { tmp =>
    f(new LocalFileSystem(tmp.toString))
  }
}
