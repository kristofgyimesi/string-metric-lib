package utils

import java.util.{ HashMap => JHashMap }
import scala.collection.immutable.HashMap
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

object ConversionUtils {

  /** Converts a Java `HashMap` to a Scala immutable `HashMap`.
    */
  implicit def javaHashMapToScalaHashMap[A, B](javaMap: JHashMap[A, B]): HashMap[A, B] =
    HashMap.from(javaMap.asScala)

}
