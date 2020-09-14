lazy val root = (project in file(".")).settings(
  organization := "ch.scalajos",
  name := "faultor",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.13.3",
  isSnapshot := true
)

lazy val os: String = Option(System.getProperty("os.name", ""))
  .map(_.substring(0, 3).toLowerCase) match {
  case Some("win") => "windows"
  case Some("mac") => "macos"
  case _           => "linux"
}

lazy val zioVersion        = "1.0.1"

javacOptions ++= Seq("-source", "11", "-target", "11", "-Xlint")

resolvers ++= Seq(
  Resolver.mavenLocal,
  DefaultMavenRepository,
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  // ----------------------------------------------------------------------------------
  // ZIO SECTION
  // ----------------------------------------------------------------------------------

  "dev.zio" %% "zio" % zioVersion,

  // ----------------------------------------------------------------------------------
  // TEST SECTION
  // ----------------------------------------------------------------------------------

  "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.3.0.0-SNAP2" % Test
  )

excludeDependencies += "org.scala-lang.modules" % "scala-xml_2.13.0-M2" // because of scalatest

scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  //"-language:existentials",            // ??? still required ??? Existential types (besides wildcard types) can be written and inferred
  //"-language:experimental.macros",     // ??? still required ??? Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // ??? still required ??? Allow higher-kinded types
  "-language:implicitConversions",     // ??? still required ??? Allow definition of implicit functions called views
  "-language:postfixOps",
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
  "-Ymacro-annotations"               // allow macro annotation. replace "org.scalamacros" % "paradise"  % "2.1.1"
)

// Note that the REPL can’t really cope with -Ywarn-unused:imports or -Xfatal-warnings so you should turn them off for the console.
scalacOptions in (Compile, console) ~= (_.filterNot(Set(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)))

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")