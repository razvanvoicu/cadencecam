package sgrv.be.core

import io.github.classgraph.ClassGraph
import scala.jdk.CollectionConverters.*
import zio.{Task, ZIO}

/** Finds and loads the Scala `object`s under `sgrv.be` that implement a nominal host interface.
  *
  * Shared by every kind of extension point so they are all discovered identically: [[BackendPlugin]] contributes
  * routes, [[SessionListener]] reacts to a session starting or ending, [[CurrentUserContributor]] adds to `/me`. Adding
  * a further kind means defining its interface and calling this, not writing another classpath scan.
  */
private[core] object ModuleDiscovery:
  def implementations(interface: Class[?], classLoader: ClassLoader): Task[Seq[String]] =
    ZIO.scoped:
      for
        result <- ZIO.acquireRelease(
          ZIO.attemptBlocking:
            new ClassGraph()
              .enableClassInfo()
              .acceptPackages("sgrv.be")
              .overrideClassLoaders(classLoader)
              .scan()
        )(scan => ZIO.attemptBlocking(scan.close()).ignore)
        classNames <- ZIO.attemptBlocking:
          result
            .getClassesImplementing(interface.getName)
            .asScala
            .map(_.getName)
            .toSeq
            .sorted
      yield classNames

  /** Resolves a discovered class name to its module instance, rejecting anything that is not the expected interface. */
  def load[A](className: String, interface: Class[A], classLoader: ClassLoader): Task[A] =
    ZIO.attemptBlocking:
      val moduleClass = Class.forName(className.stripSuffix("$") + "$", true, classLoader)
      // noinspection IllegalNull
      val module = moduleClass.getField("MODULE$").get(null)
      if interface.isInstance(module) then interface.cast(module)
      else
        throw new IllegalArgumentException(
          s"Discovered module ${module.getClass.getName} does not implement ${interface.getName}"
        )
