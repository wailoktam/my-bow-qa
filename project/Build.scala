import sbt._
import sbt.Keys._
import java.io.{File, FileInputStream, BufferedInputStream, ByteArrayOutputStream, ObjectOutputStream}
import org.rauschig.jarchivelib.ArchiverFactory
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.apache.commons.io.FileUtils.deleteQuietly
import org.apache.commons.io.FileUtils.copyURLToFile

object qaBuild extends Build {

  // need to download a file if it does not exist
  def needToDownload(destinationFile: File)(implicit logger: Logger) =
    !destinationFile.exists()

  // need to unpack a file if it does not exist or it is old
  def needToUnpack(archiveFile: File, destinationFile: File)(implicit logger: Logger) =
    !destinationFile.exists() ||
      archiveFile.lastModified() > archiveFile.lastModified()

  // download a resource file
  def downloadResource(url: URL, destinationFile: File)(implicit logger: Logger) {
    try {
      if (needToDownload(destinationFile)) {
        logger.info("Downloading " + destinationFile + " from " + url + " ...")
        copyURLToFile(url, destinationFile)
      }
    } catch {
      case e: Throwable =>
        logger.error("Error in downloadResource: deleting " + destinationFile + " ...")
        deleteQuietly(destinationFile)
        throw e
    }
  }

  // unpack an archive file
  def unpackArchive(archiveFile: File, destinationFile: File)(implicit logger: Logger) {
    try {
      if (needToUnpack(archiveFile, destinationFile)) {
        logger.info("Extracting " + destinationFile + " ...")
        destinationFile.getParentFile.mkdirs()
        ArchiverFactory.createArchiver(archiveFile).extract(archiveFile, destinationFile.getParentFile)
      }
    } catch {
      case e: Throwable =>
        logger.error("Error in unpackArchive: deleting " + destinationFile + " ...")
        deleteQuietly(destinationFile)
        throw e
    }
  }
}
