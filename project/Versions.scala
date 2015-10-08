import sbt._

object Versions {

  val buildNumber = "0.0.1-SNAPSHOT"

  val akkaVersion = "2.3.9"

  val sprayVersion = "1.3.3"

  val playJsonVersion = "2.3.2"

  val slickVersion = "2.1.0"

  def getFileTree(f: File): Stream[File] =
    f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
    else Stream.empty)
}
