package org.ditw.mateng.utils

/**
  * Created by jiaji on 2016-09-15.
  */

import java.util.regex.Pattern

import MatEngError._

trait MatEngError extends Throwable {
  val throwable:Throwable

  def describe:String = throwable.getMessage

  lazy val chain:List[MatEngError] = {
    var tmp = throwable
    if (tmp.getCause != null) {
      val cause = new MatEngErrorFromThrowable(tmp.getCause)
      cause :: cause.chain
    }
    else List(this)
  }
  lazy val stacks:Array[String] = {
    val c = Math.min(MaxStackTraceDepth, throwable.getStackTrace.length)
    val t = throwable.getStackTrace.take(c)
    t.map(stackTraceElement2String)
  }
  def describeAll = chain.map(_.describe)
  private def showStacks(regex:Pattern):String = {
    stacks.map{ st =>
      if (regex.matcher(st).matches) "\n\t" + st
      else "."
    }.mkString
  }

  def showStackChain(stackTraceFilter:String):String = {
    val regex = stackTraceFilter.r.pattern
    chain.map(_.showStacks(regex)).mkString("\n---Caused by:---\n")
  }
}

object MatEngError {
  val NotImplemented = new MatEngErrorBase("Not Implemented")

  def Todo(task:String) = new MatEngErrorBase(task)

  val MaxStackTraceDepth = 20

  def stackTraceElement2String(e:StackTraceElement):String = s"${e.getClassName}::${e.getMethodName}  --  line ${e.getLineNumber}"

  class MatEngErrorFromThrowable(val throwable:Throwable) extends MatEngError {
  }

  implicit def Throwable2MatEngError(th:Throwable) = new MatEngErrorFromThrowable(th)

  class MatEngErrorBase(msg:String) extends {
    val throwable = new Exception(msg)
  } with MatEngError

  def handle(th:Throwable, filter:String = "org.ditw.mateng.*"):String = {
    th.printStackTrace
    th match {
      case he:MatEngError => {
        s"Matching Engine Error: ${he.describe}\n%s".format(he.showStackChain(filter))
      }
      case t:Throwable => {
        val matengError:MatEngError = t
        s"Unknown Error: ${t.getMessage}\n%s".format(matengError.showStackChain(filter))
      }
    }
  }
}