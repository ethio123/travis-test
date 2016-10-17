package org.ditw.mateng

import java.nio.ByteBuffer
import java.security.MessageDigest

import org.ditw.mateng.matchers.TMatcher

/**
  * Created by jiaji on 2016-02-09.
  */
class AtomSeqMatch(val resultPool: TMatchResultPool, val range:Range, val matcher:TMatcher, val subMatches:List[AtomSeqMatch]) {
  override def toString = trace(true)
  def trace(includeId:Boolean) = {
    val atms = range.map(idx => (idx, resultPool.input.atoms(idx))).map(p => "%s(%d)".format(p._2.text, p._1)).toArray
    val offset = range.start
    subMatches.foreach(m => {
      val start = m.range.start - offset
      val end = m.range.end - offset
      atms(start) = "<" + atms(start)
      atms(end) += ">"
    })
    if (includeId) "[%s]: %s".format(if (matcher.id.nonEmpty) matcher.id.get else "None", atms.mkString(" ")) else atms.mkString(" ")
  }
  import AtomSeqMatch._
  protected val subMatchesOrderedByHashBytes = subMatches.sortBy(_.md5HashBytes)
  protected val md5HashBytes:Array[Byte] = {
    val inputRangeBytes = int2Bytes(resultPool.hashCode(), range.start, range.end)
    val in:Array[Byte] = inputRangeBytes ++ subMatchesOrderedByHashBytes.flatMap(_.md5HashBytes)
    md5Hash(in)
  }
  def allSubMatches(filter:AtomSeqMatch => Boolean):List[AtomSeqMatch] = {
    val children = subMatches.flatMap(_.allSubMatches(filter))
    if (filter(this)) List(this) ++ children else children
  }
  //val leafMatches:Set[AtomSeqMatch] = {
  //  if (subMatches.isEmpty) Set(this)
  //  else subMatches.flatMap(_.leafMatches).toSet
  //}
  val depth:Int = {
    val subDepths = subMatches.map(_.depth)
    if (subDepths.isEmpty) 1
    else subDepths.max + 1
  }

  def atoms:IndexedSeq[TAtom] = resultPool.input.atoms.slice(range.start, range.end+1)

  /*
  def covers(m2:AtomSeqMatch):Boolean = {
    val overlap = range.intersect(m2.range)
    if (overlap != m2) false
    else {
      if (subMatches.size < m2.subMatches.size) false
      else {
        if (m2.subMatches.exists(m2m => !subMatches.exists(_.covers(m2m)))) false
        else true
      }
    }
  }
  */

  private val _hash = md5Hash2Int(md5HashBytes)

  def isOverlap(ano:AtomSeqMatch):Boolean = range.intersect(ano.range).nonEmpty

  override def hashCode:Int = _hash //input.hashCode() + range.hashCode() + subMatches.map(_.hashCode).sum
  override def equals(a:Any):Boolean = {
    if (a.isInstanceOf[AtomSeqMatch]) md5HashBytes.sameElements(a.asInstanceOf[AtomSeqMatch].md5HashBytes)
    else false
    /* optimized comparison: no pattern match to avoid anonymous functions
    a match {
      case m1:AtomSeqMatch => {
        ByteArrayOrdering.equiv(md5HashBytes, m1.md5HashBytes)
      }
      case _ => false
    }
    */
  }
}

object AtomSeqMatch {

  implicit val ByteArrayOrdering:Ordering[Array[Byte]] = new Ordering[Array[Byte]] {
    override def compare(x: Array[Byte], y: Array[Byte]): Int = {
      if (x.length != y.length) throw ErrorHandling.NotImplemented("Ordering for byte array of diff size")
      else {
        val diff = x.indices.map(idx => x(idx)-y(idx))
        diff.find(_ != 0).getOrElse(0)
      }
    }
  }

  private val MD5 = MessageDigest.getInstance("MD5")
  def md5Hash(in:Array[Byte]) = {
    MD5.reset()
    MD5.digest(in)
  }

  def int2Bytes(in:Int*):Array[Byte] = {
    val bf = ByteBuffer.allocate(4*in.length)
    in.indices.foreach{ idx=>
      bf.putInt(4*idx, in(idx))
    }
    bf.array()
  }

  def md5Hash2Int(hash:Array[Byte]):Int = {
    val bf = ByteBuffer.allocate(4)
    (0 to 3).foreach(i => bf.put(hash(i*4+i)))
    bf.getInt(0)
  }

  var t = 0
  val EmptySubMatches = List[AtomSeqMatch]()


  def from(magnet:MatchFromMagnet):magnet.Result = magnet()
  /*
  def from(input:TInput, index:Int, matcher:TMatcher):AtomSeqMatch = new AtomSeqMatch(input, index to index, matcher, EmptySubMatches)
  def from(input:TInput, matcher:TMatcher, subMatches:List[AtomSeqMatch]):AtomSeqMatch = {
    val start = subMatches.map(_.range.start).min
    val end = subMatches.map(_.range.end).max
    new AtomSeqMatch(input, start to end, matcher, subMatches)
  }
  */
}