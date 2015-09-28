import java.io.File

import org.apache.commons.io.FileUtils.{copyURLToFile, deleteQuietly}
import org.rauschig.jarchivelib.ArchiverFactory
import sbt._



object qaBuild extends Build {

  val resourcePacked = settingKey[File]("Directory in which packed resources are located.")

  val dataDirectory = settingKey[File]("Directory in which data os located.")

  val unpackResources = taskKey[Unit]("Unpack resources.")

  val wiki1File = settingKey[File]("Location of unpacked wiki files part 1")

  val wiki2File = settingKey[File]("Location of unpacked wiki files part 2")

  val wiki3File = settingKey[File]("Location of unpacked wiki files part 3")

  val wiki4File = settingKey[File]("Location of unpacked wiki files part 4")

  val indexFile = settingKey[File]("Location of index created for Wiki")



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

  def indexWiki(wikiFile: File, indexFile: File)(implicit logger: Logger) {
    val scalacCommand = "scalac " + "src/main/scala/Indexing.scala"
//    logger.error(scalacCommand.!!)
    val scalaCommand = "scala " + "qa.main.ja.Indexing.scala " + wikiFile + " " + indexFile
//     logger.error(scalaCommand.!!)
  }

  def wrapper(cleanTool: File, preProcessedFile: File, processedFile: File)(implicit logger: Logger) {
    if (!processedFile.exists()) {
      val pythonCommand = "python " + cleanTool + " -o " + processedFile + " " + preProcessedFile
      logger.error(pythonCommand.!!)
    }
  }

}

