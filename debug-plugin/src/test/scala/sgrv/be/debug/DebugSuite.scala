package sgrv.be.debug

import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.{Runtime, Task, Unsafe}
import zio.http.{Method, Path, Status}

class DebugSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  test("loads the separately compiled plugin object and exposes its route"):
    val loaded = run(RouteDiscovery.load(Debug.getClass.getName, getClass.getClassLoader))
    val discovered = run(RouteDiscovery.discover(CapabilityRegistry.empty, classLoader = getClass.getClassLoader))

    assertEquals(loaded.id, "debug")
    assert(Debug.routes.routes.exists(_.routePattern.matches(Method.GET, Path("/debug"))))
    assert(discovered.exists {
      case PluginStatus.Skipped("debug", _, missing) => missing.exists(_.id == "session-store")
      case _                                         => false
    })

  test("collects every requested system-signature section"):
    val signature = run(Debug.collect(Seq("9000", "line\nbreak", "slash\\value", "carriage\rreturn", "tab\tvalue")))

    Seq(
      "=== Operating system ===",
      "=== Disk space ===",
      "=== Memory ===",
      "=== Environment variables ===",
      "=== Backend command-line arguments ===",
      "=== Java runtime ===",
      "=== nginx reverse proxy ==="
    ).foreach(section => assert(signature.contains(section), clues(section)))

    assert(signature.contains("[0]=9000"))
    assert(signature.contains("[1]=line\\nbreak"))
    assert(signature.contains("[2]=slash\\\\value"))
    assert(signature.contains("[3]=carriage\\rreturn"))
    assert(signature.contains("[4]=tab\\tvalue"))
    assert(signature.contains(s"Java version: ${java.lang.System.getProperty("java.version")}"))

  test("serves the signature response"):
    val response = run(Debug.response)
    val content = run(response.body.asString.orDie)

    assertEquals(response.status, Status.Ok)
    assert(content.contains("=== Operating system ==="))
    assert(content.contains("Backend command-line arguments ===\n(none)"))
