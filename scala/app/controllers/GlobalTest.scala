package controllers

import java.io.{InputStreamReader, StringReader}

import dele.book.common.NNet
import org.apache.commons.compress.utils.IOUtils
import play.api.Play

import scala.io.Source

/**
  * Created by dele on 2016-10-06.
  */
object GlobalTest {
  var nnet:NNet = {
    val strm = getClass.getClassLoader.getResourceAsStream("Resources/layer-params.json")
    val src = Source.fromInputStream(strm)
    val j = src.mkString
    NNet.fromJson(j)
  }
}
