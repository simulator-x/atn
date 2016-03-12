libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
   	"net.sf.jung" % "jung-api" % "2.0.1",
   	"net.sf.jung" % "jung-graph-impl" % "2.0.1",
   	"net.sf.jung" % "jung-algorithms" % "2.0.1",    	
   	"net.sf.jung" % "jung-visualization" % "2.0.1",    
	"org.scala-lang.modules" %% "scala-swing" % "1.0.2"
)

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

compileOrder := CompileOrder.JavaThenScala

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")
