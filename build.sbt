ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "SysArch"

val chiselVersion = "6.7.0"
val scalaVersionMajor = "2.13"

import java.nio.file.{Files, Paths, StandardCopyOption}
import java.text.SimpleDateFormat
import java.util.Date
import sbt.IO

import java.nio.file._
import scala.collection.JavaConverters._

lazy val root = (project in file("."))
  .settings(
    name := "RISC-V",
    // autoAPIMappings := true,
    apiMappings ++= {
      val cp: Seq[Attributed[File]] = (Compile / fullClasspath).value
      def findManagedDependency(organization: String, name: String): File = {
        (for {
          entry <- cp
          module <- entry.get(moduleID.key)
          if module.organization == organization
          if module.name.startsWith(name)
          jarFile = entry.data
        } yield jarFile).head
      }
      Map(
        findManagedDependency("org.chipsalliance", "chisel") -> url(
          s"https://javadoc.io/doc/org.chipsalliance/chisel_$scalaVersionMajor/$chiselVersion"
        )
      )
    },
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % "test",
      "io.circe" %% "circe-core" % "0.14.13",
      "io.circe" %% "circe-generic" % "0.14.13",
      "io.circe" %% "circe-parser" % "0.14.13",
      "com.softwaremill.sttp.client4" %% "core" % "4.0.3",
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    )
  )

// Define the task key for copying the jars and dependencies
val copyDependencies = taskKey[Unit](
  "Copy JAR, test JAR, and all (test) dependencies to a single folder"
)

copyDependencies := {
  val log = streams.value.log

  // Define the output directory
  val outputDir = target.value / "dependency-jars"
  IO.createDirectory(outputDir)

  // Helper function to copy a file if it is not a directory
  def copyFileIfNotDirectory(source: File, targetDir: File): Unit = {
    if (source.isFile) {
      val targetFile = targetDir / source.getName
      IO.copyFile(source, targetFile)
      log.info(s"Copied ${source.getName} to: $targetFile")
    }
  }

  // Copy main JAR
  val mainJar = (Compile / packageBin).value
  copyFileIfNotDirectory(mainJar, outputDir)

  // Copy test JAR
  val testJar = (Test / packageBin).value
  copyFileIfNotDirectory(testJar, outputDir)

  // Copy all compile dependencies
  val compileDeps = (Compile / dependencyClasspath).value.map(_.data)
  compileDeps.foreach(file => copyFileIfNotDirectory(file, outputDir))

  // Copy all test dependencies
  val testDeps = (Test / dependencyClasspath).value.map(_.data)
  testDeps.foreach(file => copyFileIfNotDirectory(file, outputDir))

  log.info(s"All dependencies copied to: $outputDir")
}

lazy val listClasses = taskKey[List[String]]("List all classes")

listClasses := {
  val targetDirectory = (Compile / classDirectory).value
  val classFiles = Files
    .walk(targetDirectory.toPath)
    .iterator()
    .asScala
    .filter(path => path.toString.endsWith(".class"))
    .toList

  classFiles.map { path =>
    targetDirectory.toPath
      .relativize(path)
      .iterator()
      .asScala
      .map(_.toString())
      .mkString(".")
      .stripSuffix(".class")
  }
}

lazy val check = taskKey[Unit]("check")

check := {
  val definedClasses = listClasses.value
  val neededClasses = Set(
    "RISCV.implementation.Core",
    "RISCV.implementation.RV32I.ALU",
    "RISCV.implementation.RV32I.BranchUnit",
    "RISCV.implementation.RV32I.ControlUnit",
    "RISCV.implementation.RV32I.Decoder",
    "RISCV.implementation.RV32I.RV32I",
    "RISCV.implementation.generic.GenericCore",
    "RISCV.implementation.generic.ProgramCounter",
    "RISCV.implementation.generic.RegisterFile",
    "RISCV.implementation.RV32B.BasicBitManipulationUnit",
    "RISCV.implementation.RV32B.BitPermutationUnit",
    "RISCV.interfaces.RV32I.AbstractALU",
    "RISCV.interfaces.RV32I.AbstractBranchUnit",
    "RISCV.interfaces.RV32I.AbstractControlUnit",
    "RISCV.interfaces.RV32I.AbstractDecoder",
    "RISCV.interfaces.RV32I.ALUInterface",
    "RISCV.interfaces.RV32I.BranchUnitInterface",
    "RISCV.interfaces.RV32I.ControlUnitInterface",
    "RISCV.interfaces.RV32I.DecoderInterface",
    "RISCV.interfaces.generic.AbstractExecutionUnit",
    "RISCV.interfaces.generic.AbstractProgramCounter",
    "RISCV.interfaces.generic.AbstractRegisterFile",
    "RISCV.interfaces.generic.ResetInterface",
    "RISCV.interfaces.generic.PCInterface",
    "RISCV.interfaces.generic.RegisterInterface",
    "RISCV.interfaces.generic.InstructionInterface",
    "RISCV.interfaces.generic.DataInterface",
    "RISCV.interfaces.generic.TrapInterface",
    "RISCV.interfaces.generic.ExecutionUnitInterface",
    "RISCV.interfaces.generic.RVFIInterface",
    "RISCV.utils.assembler.ObjectUtils",
    "RISCV.utils.assembler.RISCVAssembler",
    "RISCV.utils.assembler.RISCVAssemblerException",
    "RISCV.utils.assembler.InstType",
    "RISCV.utils.assembler.Instruction",
    "RISCV.utils.assembler.Instructions",
    "RISCV.utils.assembler.PseudoInstructions",
    "RISCV.utils.assembler.PreProcess",
    "RISCV.utils.assembler.LineParser",
    "RISCV.utils.assembler.InstructionParser",
    "RISCV.utils.assembler.FillInstruction",
    "RISCV.utils.assembler.RegMap",
    "RISCV.utils.assembler.CsrMap",
    "RISCV.utils.PermBuilder",
    "RISCV.model.REG_WRITE_SEL",
    "RISCV.model.ALU_OP_1_SEL",
    "RISCV.model.ALU_OP_2_SEL",
    "RISCV.model.NEXT_PC_SELECT",
    "RISCV.model.STALL_REASON",
    "RISCV.model.ALU_CONTROL",
    "RISCV.model.TRAP_REASON",
    "RISCV.model.RISCV_OP",
    "RISCV.model.RISCV_FUNCT3",
    "RISCV.model.RISCV_FUNCT7",
    "RISCV.model.RISCV_FUNCT12",
    "RISCV.model.RISCV_TYPE",
    "RISCV.model.InstructionSets",
    "bitmanipulation.AbstractGeneralizedReverser",
    "bitmanipulation.GeneralizedReverser",
    "bitmanipulation.AbstractLeadingZerosCounter",
    "bitmanipulation.LeadingZerosCounter",
    "bitmanipulation.AbstractFixedRotater",
    "bitmanipulation.FixedRotater",
    "bitmanipulation.AbstractSequentialRotater",
    "bitmanipulation.SequentialRotater",
    "bitmanipulation.AbstractShuffler",
    "bitmanipulation.Shuffler"
  )
  val allClassesDefined = neededClasses.subsetOf(definedClasses.toSet) match {
    case true => println("All classes defined")
    case false =>
      throw new Exception(
        "Some classes are not defined:\n" + (neededClasses.toSet -- definedClasses.toSet)
          .mkString("\n")
      )
  }
}

check := check.dependsOn(Compile / compile).value

lazy val createSubmission = taskKey[Unit]("Create a submission zip file")

createSubmission := {
  val baseDirectoryValue = baseDirectory.value
  val srcDirectory = baseDirectoryValue / "src" / "main"
  val targetDirectory = baseDirectoryValue / "submissions"
  val contributionsFile = baseDirectoryValue / "CONTRIBUTIONS.md"
  val timestamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date())
  val zipFileName = s"submission_$timestamp.zip"

  // Create the submissions directory
  IO.createDirectory(targetDirectory)

  // Zip the main folder
  val zipFile = targetDirectory / zipFileName
  val allFiles = (srcDirectory ** "*").get
  val mappings = allFiles.map(file =>
    (file, baseDirectoryValue.toPath.relativize(file.toPath).toString)
  ) :+ (
    contributionsFile,
    baseDirectoryValue.toPath.relativize(contributionsFile.toPath).toString
  )
  IO.zip(mappings, zipFile, None)

  println(s"Submission created: ${zipFile.getPath}")
}

lazy val checkAndSubmission =
  taskKey[Unit]("Create a submission zip file after checking the project")

checkAndSubmission := {
  createSubmission.value
}

checkAndSubmission := checkAndSubmission.dependsOn(check).value
