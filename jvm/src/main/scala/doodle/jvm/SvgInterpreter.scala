package doodle.jvm

import java.nio.charset.Charset
import java.nio.file.{Files, Paths, StandardOpenOption}

import scalatags.Text
import scalatags.Text.implicits._
import scalatags.Text.svgTags
import scalatags.Text.svgTags.SeqFrag
import scalatags.Text.svgAttrs
import doodle.backend.{BoundingBox, Finalised, Formats, Interpreter}
import doodle.core.{DrawingContext, Image, Point}
import doodle.core.font.Font
import doodle.core.transform.Transform

object SvgInterpreter extends Interpreter[Formats.Svg, svgTags.ConcreteHtmlTag[String]] {

  // TODO can we even predict font metrics? we should, right?
  private val metrics: (Font, String) => BoundingBox = (_, _) => BoundingBox(42, 42, 42, 42)
  private val dc = DrawingContext.blackLines

  override def interpret(image: Image): svgTags.ConcreteHtmlTag[String] = {
    val finalised = Finalised.finalise(image, dc, metrics)
    svgTags.svg(interpret(finalised, origin = Point.zero, transforms = List.empty))
  }

  private def interpret(finalised: Finalised, origin: Point, transforms: List[Transform]): List[Text.TypedTag[String]] = {
    finalised match {
      case b @ Finalised.Beside(l, r) =>
        val box = b.boundingBox
        val lBox = l.boundingBox
        val rBox = r.boundingBox

        // Beside aligns the y coordinate of the origin of the bounding boxes of l and r. We need to calculate the x coordinate of the origin of each bounding box, remembering that the origin may not be the center of the box. We first calculate the the x coordinate of the center of the l and r bounding boxes and then displace the centers to their respective origins

        // The center of the l and r bounding boxes in the current coordinate system
        val lCenterX = origin.x + box.left  + (lBox.width / 2)
        val rCenterX = origin.x + box.right - (rBox.width / 2)

        // lBox and rBox may not have their origin at the center of their bounding
        // box, so we transform accordingly if need be.
        val lOrigin =
        Point.cartesian(
          lCenterX - lBox.center.x,
          origin.y
        )
        val rOrigin =
          Point.cartesian(
            rCenterX - rBox.center.x,
            origin.y
          )
        interpret(l, lOrigin, transforms) ++ interpret(r, rOrigin, transforms)
      case a @ Finalised.Above(t, b) =>
        val box = a.boundingBox
        val tBox = t.boundingBox
        val bBox = b.boundingBox

        val tCenterY = origin.y + box.top - (tBox.height / 2)
        val bCenterY = origin.y + box.bottom + (bBox.height / 2)

        val tOrigin =
          Point.cartesian(
            origin.x,
            tCenterY - tBox.center.y
          )
        val bOrigin =
          Point.cartesian(
            origin.x,
            bCenterY - bBox.center.y
          )

        interpret(t, tOrigin, transforms) ++ interpret(b, bOrigin, transforms)
      case Finalised.On(o, u) =>
        interpret(o, origin, transforms) ++ interpret(u, origin, transforms)
      case Finalised.Empty => List.empty
      case Finalised.ClosedPath(context, elements, boundingBox) =>
        val dAttr = Svg.toSvgPath(elements) ++ "Z"
        val style = Svg.toStyle(context)
        List(svgTags.path(svgAttrs.transform:=(Transform.translate(origin.toVec) +: transforms).map(Svg.toSvgTransform).reverse.mkString, svgAttrs.style:=style, svgAttrs.d:=dAttr))
      case Finalised.OpenPath(context, elements, boundingBox) =>
        val dAttr = Svg.toSvgPath(elements) ++ "Z"
        val style = Svg.toStyle(context)
        List(svgTags.path(svgAttrs.transform:=(Transform.translate(origin.toVec) +: transforms).map(Svg.toSvgTransform).reverse.mkString, svgAttrs.style:=style, svgAttrs.d:=dAttr))
      case Finalised.Text(context, characters, boundingBox) =>
          val style = Svg.toStyle(context)
          // SVG x and y coordinates give the bottom left corner of the text. Our
          // bounding box origin is at the center of the text.
          val bottomLeft = Transform.translate(-boundingBox.width / 2, -boundingBox.height / 2)
          val fullTx = (Transform.translate(origin.toVec) +: Transform.horizontalReflection +: transforms :+ bottomLeft).reduceLeft(_ andThen _)
          val svgTx = transforms.reverse.mkString ++ " " ++ Svg.toSvgTransform(fullTx)
          // TODO
//          val font = FontMetrics.toCss(context.font.get)
          List(svgTags.text(svgAttrs.style := style,
            svgAttrs.x := 0,
            svgAttrs.y := 0,
            svgAttrs.transform := svgTx,
            // TODO
            //            Text.cssFont:=font,
            characters))
      case Finalised.Transform(tx, i) =>
        interpret(i, origin, tx :: transforms)
      case _: Finalised.Draw =>
        throw new UnsupportedOperationException("Side-effecting draw to canvas not supported in this interpreter")
      }
  }

  implicit class InterpreterMapper[Format, A](val interpreter: Interpreter[Format, A]) extends AnyVal {
    def map[T](f: A => T): Interpreter[Format, T] = (image: Image) => f(interpreter.interpret(image))
  }

  private def saveToFile(payload: Array[Byte])(filename: String): Unit = {
    Files.write(Paths.get(filename), payload, StandardOpenOption.WRITE, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  implicit val save: Interpreter.Save[Formats.Svg] =
    SvgInterpreter
      .map(_.render.getBytes(Charset.forName("UTF-8")))
      .map(saveToFile)
}
