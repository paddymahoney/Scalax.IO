package scalax.data

import scala.collection.{mutable => mut}

// TODO: Is there a way to avoid having to explicitly handle both Iterators and
// Iterables?

// TODO: Pretty much anywhere I take a number, I'd like to make the facility
// more general, but without (say) incurring the reflection costs associated
// with structural typing.

object collection {

  //
  // Tuples
  //

  /**
   * Swap the elements of a tuple.
   */
  def swap[a,b](p: (a,b)) = (p._2, p._1)

  implicit def pair2xpair(p: (Int,Int)) = new XPair(p._1,p._2)
  implicit def xpair2pair(p: XPair) = (p.x,p.y)

  case class XPair(x: Int, y: Int) {
    /**
     * Add a pair of Ints.
     */
    def +(p: (Int, Int)) = Pair(x+p.x, y+p.y)
  }

  //
  // Strings
  //

  // TODO: Make faster.
  /**
   * Split a String into words. Use this when possible, since its
   * implementation may become more efficient than the regex-based
   * String.split() in the future.
   */
  def words(x: String) = x.trim.split("\\s+")

  /**
   * Convert camel case to lower case with the given inter-word glue string.
   * <p>
   * <code>camelToLower("helloWorld", "_") == "hello_world"</code>
   * <p>
   * <code>camelToLower("HelloWorld", "_") == "_hello_world"</code>
   */
  def camelToLower(s: String, sep: String) = {
    val xs =
      for (c <- s)
        yield if (c.isUpperCase) sep + c.toLowerCase else c
    xs mkString ""
  }
  def camelToUnder(s: String) = camelToLower(s, "_")
  def camelToHyphen(s: String) = camelToLower(s, "-")
  def camelToSpaced(s: String) = camelToLower(s, " ")

  /**
   * Convert a spaced word to a hyphenated word.
   */
  def spacedToHyphen(s: String) = s replaceAll (" ", "-")

  /**
   * Rot-n-encode a string, but where all characters are rotated, not just
   * alphabetical characters.
   */
  def rot(n: Int, s: String) = s map (_ + n toChar) mkString

  //
  // Views of sequences
  //

  // TODO: Is there a way to "chain" views? Then Iterable2FilterMap is
  // unnecessary and can be removed.
  implicit def Iterable2FilterMap[T](s: Iterable[T]) = new FilterMap(s.elements)
  implicit def Iterable2Iterator[T](s: Iterable[T]) = s.elements
  implicit def Iterator2FilterMap[T](s: Iterator[T]) = new FilterMap(s) 

  class FilterMap[T](s: Iterator[T]) { 
    /**
     * Given the partial function f, filter out elements over which f is
     * undefined, and map the remaining elements with f. Note that this
     * evaluates f twice for each element.
     */
    def filterMap[B](f: PartialFunction[T,B]) = 
      s filter (f.isDefinedAt) map f
    /**
     * Given the function f, filter out elements for which f returns None, and
     * map the remaining elements with f, extracting the value from the Some.
     * This evaluates f once for each element.
     */
    def filterMapOption[B](f: Function[T,Option[B]]) =
      s map f filter (None !=) map (_.get)
  }

  implicit def Iterator2XIterator[T](s: Iterator[T]) = new XIterator(s)
  // TODO: Following doesn't compile due to recursive call. How?
  // implicit def XIterator2Iterator[T](x: XIterator[T]) = x.i

  class XIterator[a](i: Iterator[a]) {
    /**
     * Iterate over the iterator and return the last element.
     */
    def last = {
      var last = i.next
      for (val x <- i) { last = x }
      last
    }
  }

  implicit def MapToRichMap[k,v](m: Map[k,v]) = new RichMap(m)
  implicit def RichMapToMap[k,v](m: RichMap[k,v]) = m.m

  class RichMap[k,v](val m: Map[k,v]) {
    def toArray = {
      val a = new Array[(k,v)](m size)
      m copyToArray (a,0)
      a
    }
  }

  implicit def StreamToRichStream[a](s: Stream[a]) = new RichStream(s)
  implicit def RichStreamToStream[a](s: RichStream[a]) = s.s

  class RichStream[a](val s: Stream[a]) {
    // TODO: Fix this; it's dangerously inefficient and could cause a stack
    // overflow (see StreamRecursion test in sandbox)!
    // TODO: Rename; confusing enough with groupBy != Haskell's groupBy, which
    // is in turn == groupByPairwise.
    /**
     * This splits up the stream by using spanBy
     * <p>
     * <code>[1..9].groupBy(_/3) == [[1,2],[3,4,5],[6,7,8]]</code>
     */
    def groupBy[b](f: a => b) = {
      def r(s: Stream[a]): Stream[Stream[a]] = s match {
        case Stream.cons(h,t) => {
          val cur = f(h)
          // TODO: The next line induces a bug in Scala.
          // val (x,y): (Seq[a],Seq[a]) = spanBy(s)(f(_) == cur)
          val (x,y) = spanBy(s)(f(_) == cur)
          Stream.cons(
            Stream fromIterator x.elements,
            r(Stream fromIterator y.elements)
          )
        }
        case Stream.empty => Stream.empty
      }
      r(s)
    }
  }

  // TODO: Perhaps this belongs in a different module.

  //
  // Simple statistics
  //

  def sum(xs: Iterator[Int]) = xs.foldLeft(0)(_+_)
  def sum(xs: Seq[Double]) = xs reduceLeft ((x:Double,y:Double)=>x+y)
  def sum(xs: Iterator[Double]) = xs reduceLeft ((_:Double)+(_:Double))
  def mean(xs: Seq[Int]) = sum(xs.elements) / xs.size
  def mean(xs: Seq[Double]) = sum(xs.elements) / xs.size
  def median(xs: Seq[Long]) = xs(xs.length / 2)

  import scala.util.Sorting

  /**
   * Destructively sort the array, and returns a tuple of the (mean, median,
   * standard deviation, variance, minimum, and maximum) of a list.
   */
  def sortStats(xs: Array[Double]) = {
    Sorting quickSort xs
    val (mean, variance) = meanAndVariance(xs)
    val (min, median, max) = (xs(xs.length / 2), xs(0), xs.last)
    val sdev = Math sqrt variance
    (mean, median, sdev, variance, min, max)
  }

  /**
   * Return a tuple of the mean and variance of the given sequence.
   */
  def meanAndVariance(xs: Seq[Double]) = {
    val mean = sum(xs) / xs.length
    val variance = sum( xs map (x => Math pow (x - mean, 2)) ) / xs.length
    (mean, variance)
  }

  // TODO: Can I write functions that are generic over n-tuples?
  /**
   * Return the two smallest elements.
   */
  def mins(xs: Iterable[Long]) = {
    var min1 = java.lang.Long.MAX_VALUE // smallest
    var min2 = java.lang.Long.MAX_VALUE // second smallest
    for (x <- xs) {
      if (x < min1) { min2 = min1; min1 = x }
      else if (x > min1 && x < min2) { min2 = x }
    }
    (min1,min2)
  }

  // TODO: Rename.
  /**
   * Destructively sort the array by the tuples' second elements, and return
   * the array.
   */
  def sortCounts[a](xs: Array[(a,Int)]) = {
    Sorting.stableSort(xs, (p: (a,Int), q: (a,Int)) => p._2 > q._2)
    xs
  }

  // TODO: Rename.
  /**
   * Return an array of the mappings in the Map, sorted by their entry-values.
   */
  def sortCounts[a](h: mut.Map[a,Int]): Seq[(a,Int)] =
    sortCounts(h toArray)

  /**
   * Given an iterator, return a HashMap from each distinct element to its
   * count.
   */
  def hist[a](xs: Iterator[a]) = {
    val h = new mut.HashMap[a,Int] { override def default(k: a) = 0 }
    for (x <- xs) { h(x) = h(x) + 1 }
    h
  }

  /**
   * Return a list of the most popular elements along with their histogram
   * counts.
   */
  def topHist[a](xs: Iterator[a], n: Int): (mut.HashMap[a,Int], Seq[(a,Int)]) = {
    val h = hist(xs)
    val sorted = sortCounts(h)
    val took = take(sorted, n)
    (h, took)
  }

  /**
   * Return a list of the most popular element. An arbitrary element may be
   * returned in case of ties.
   */
  def mostPopular[a](xs: Iterator[a]) = topHist(xs, 1)._2 toList 0 _1

  //
  // Logic
  //

  /**
   * Negat a predicate function.
   */
  def not[a](pred: a => Boolean)(x: a) = !pred(x)

  /**
   * Same as <code>xs forall (x =&gt; x)</code>
   */
  def all(xs: Seq[Boolean]) = xs forall (x => x)

  //
  // Sequences
  //

  /**
   * Return pairs of elements.
   * <p>
   * <code>
   * pairs([a,b,c,d])   == [(a,b),(c,d)]
   * <p>
   * pairs([a,b,c,d,e]) == [(a,b),(c,d)]
   * </code>
   */
  def pairs[a](xs: Seq[a]): Stream[(a,a)] = {
    xs match {
      case Seq()                => Stream empty
      case Seq(a, b, rest @ _*) => Stream cons ((a,b), pairs(rest))
    }
  }

  /**
   * Return an iterator that yields invocations of f until it returns null.
   * Useful for, e.g., Java's IO stream abstractions.
   */
  def untilNull[a](f: => a) = new Iterator[a] {
    var upcoming = f
    override def hasNext = upcoming != null
    override def next = {
      val emit = upcoming
      upcoming = f
      emit
    }
  }

  /**
   * For each x in xs, if p(x), then x is a header, and it owns all the xs
   * after it until the next x for which p(x) holds. The return value is the
   * header and its body.
   * <p>
   * <code>
   * groupByHeaders([1,2,3,4,5,6,7,8,9])(_%3==0) == [[3,4,5],[6,7,8],[9]]
   * </code>
   */
  def groupByHeaders[a](xs: Seq[a])(p: a => Boolean) = {
    val ys = new mut.ArrayBuffer[mut.ArrayBuffer[a]]
    for (x <- xs) {
      if (p(x)) ys += new mut.ArrayBuffer[a]
      if (!ys.isEmpty) ys.last += x
    }
    ys
  }

  /**
   * Given a list of sublists, return pairs of the first element of each
   * sublist along with the remaining elements. Empty sublists are ignored.
   * <p>
   * <code>
   * separateHeads([[a,b,c],[d,e,f,g],[],[h]])
   *   == [(a,[b,c]),(d,[e,f,g]),(h,[])]
   * </code>
   */
  def separateHeads[a](xss: Seq[Seq[a]]) =
    for (xs <- xss; if !xs.isEmpty; (Seq(a),b) = span(1)(xs)) yield (a,b)

  /**
   * Construct a multimap out of the given key-value pairs; the result maps
   * keys to sets of values.
   */
  def multimap[a,b](xs: Iterable[(a,b)]) = {
    val h = new mut.HashMap[a,mut.Set[b]] with mut.MultiMap[a,b] {
      override def makeSet = new mut.HashSet[b]
    }
    for ((k,v) <- xs) {
      h add (k,v)
    }
    h
  }

  /**
   * Construct a multimap out of xs, preserving the order of values as they
   * were input.
   */
  def orderedMultimap[a,b](xs: List[(a,b)]) = {
    val h = new mut.HashMap[a, mut.ArrayBuffer[b]] {
      override def default(k: a) = {
        this(k) = new mut.ArrayBuffer[b]
        this(k)
      }
    }
    for ((k,v) <- xs) h(k) += v
    h
  }

  /**
   * Convert an iterator to array.
   */
  def iterator2array[a](xs: Iterator[a]) = xs.toList.toArray

  /**
   * A combination of take and drop.
   * <p>
   * <code>span(3)([a,b,c,d,e]) == ([a,b,c],[d,e])</code>
   */
  def span[a](n: Int)(xs: Seq[a]) = (xs take n, xs drop n)

  /**
   * Same as span but for Strings, which are treated as an array of chars.
   * <p>
   * <code>spanstr(3)("abcde") == ("abc","de")</code>
   */
  def spanstr[a](n: Int)(xs: String) =
    (xs take n mkString ("","",""), xs drop n mkString ("","",""))

  /**
   * A combination of takeWhile and dropWhile.
   * <p>
   * <code>spanBy([1,2,3,4,5])((_ != 3)) == ([1,2],[3,4,5])</code>
   */
  def spanBy[a](xs: Seq[a])(pred: a => Boolean): (Seq[a],Seq[a]) = {
    val take = xs takeWhile pred
    val rest = xs dropWhile pred
    (take, rest)
  }

  /**
   * Split up the sequence by the given predicate. Similar to String.split, but
   * matching is per-element.
   * <p>
   * <code>
   * splitBy([1..10], (_ % 4 &lt; 2)) == [[2,3],[6,7],[8,9]]
   * <p>
   * [1,3,5,6,7,8] odd == ([1,3,5],[6,7,8])
   * <p>
   * [] _ == ([],[])
   * </code>
   */
  def splitBy[a](xs: Seq[a])(pred: a => Boolean): Stream[Seq[a]] = {
    val consider = xs dropWhile pred
    if (consider.length == 0) {
      Stream.empty
    } else {
      val (take, rest) = spanBy(consider)(not(pred))
      Stream.cons(take, splitBy(rest)(pred))
    }
  }

  // TODO: What's a better name?
  /**
   * Same as splitBy, but uses the negation of the pred. Note that this is
   * quite different from Haskell's groupBy.
   * <p>
   * <code>groupBy([1,2,3,4,5], (_ % 4 &lt; 2)) == [[1],[4,5],[8,9]]</code>
   */
  def groupBy[a](xs: Seq[a])(pred: a => Boolean): Stream[Seq[a]] =
    splitBy(xs)(not(pred))

  // TODO: this was renamed from slices. Make sure nothing was broken.
  /**
   * Return all length-n "chunks" of xs as a stream.
   * <p>
   * <code>chunks(3)([a..h]) == [[a,b,c],[d,e,f],[g,h]]</code>
   */
  def chunks[a](n: Int)(xs: Seq[a]): Stream[Seq[a]] = {
    if (xs.length == 0) Stream.empty
    else Stream.cons(xs take n, chunks(n)(xs drop n))
  }

  // TODO Type-check this code; is it actually a projection?
  /**
   * Return all length-n slices of xs.
   * <p>
   * <code>slice([a,b,c,d,e], 3) == [[a,b,c],[b,c,d],[c,d,e]]</code>
   */
  def slices[a](xs: Seq[a], n: Int) =
    0 to (xs.length - n) map { i => xs slice (i,i+n) }

  /**
   * Same as <code>slices(_,2)</code> but yields tuples.
   * <p>
   * <code>
   * pairwise([a,b,c]) == [(a,b),(c,d)]
   * <p>
   * pairwise([a]) == []
   * <p>
   * pairwise([]) == []
   * </code>
   */
  def pairwise[a](xs: Iterable[a]) = xs.elements zip (xs.elements drop 1)

  /**
   * Same as Haskell's groupBy.  Groups together equal elements, letting the
   * user supply a custom equality test.  Note this is quite different from
   * groupBy/splitBy.
   * <p>
   * <code>
   * groupPairwiseBy([1..9], (_/3 == _/3)) == [[1,2],[3,4,5],[6,7,8],[9]]]
   * </code>
   */
  def groupPairwiseBy[a](xs: Seq[a])(pred: (a,a) => Boolean): List[List[a]] =
    (xs foldLeft List(List[a]())) { (a,y) => a match {
      case List(List()) => List(List(y))
      case (x :: xs) :: os if x == y => (y :: x :: xs) :: os
      case xs => List(y) :: xs
    }}

  /**
   * @deprecated Use <code>Iterable mkString ""</code>
   */
  def str(xs: Iterable[Char]) = xs mkString ""

  /**
   * Return an infinite stream, where each element is an evaluation of gen.
   */
  def repeat[a](gen: => a): Stream[a] = Stream.cons(gen, repeat(gen))

  /**
   * Return a stream of n elements, where each element is an evaluation of gen.
   * <p>
   * Note that gen may be evaluated more than n times. See
   * <a href="http://homepages.inf.ed.ac.uk/wadler/papers/lazyinstrict/lazyinstrict.ps">
   * "How to add laziness to a strict language, without even being odd"
   * </a>
   */
  def replicate[a](n: Int, gen: => a): Stream[a] = repeat(gen) take n

  /**
   * Return a stream of length at least n whose first elements are from the
   * given iterator, but if the iterator has fewer than n elements, then the
   * remaining elements are repeat(gen).
   */
  def pad[a](n: Int, e: => a, s: Iterator[a]) =
    Stream.concat(Stream.fromIterator(s), repeat(e)) take n

  /**
   * Truncate elements off the end of the sequence that satisfy the given
   * predicate.  A reverse dropWhile.
   * <p>
   * <code>truncateWhile([1,2,3,4,5], (_ != 3)) == [1,2,3]</code>
   */
  def truncateWhile[a](seq: Seq[a])(pred: a => Boolean) =
    (seq.reverse dropWhile pred).reverse

  /**
   * Find the index of the last element in the sequence that satisfies the
   * given predicate.
   */
  def lastIndexWhere[a](seq: Seq[a])(pred: a => Boolean) = {
    val i = seq.reverse findIndexOf pred
    if (i < 0) i else seq.length - i - 1
  }

  def argmax(xs: Seq[Int]) =
    xs.elements.zipWithIndex reduceLeft {
      (a:(Int,Int),b:(Int,Int)) => (a,b) match {
        case ((x,i),(y,j)) => if (x > y) a else b
      }
    }
  def argmin(xs: Seq[Int]) =
    xs.elements.zipWithIndex reduceLeft {
      (a:(Int,Int),b:(Int,Int)) => (a,b) match {
        case ((x,i),(y,j)) => if (x < y) a else b
      }
    }

  /**
   * Given an Iterator of pairs, return two Iterators over the first and second
   * elements of the pairs.  Note that this operates by duplicating the
   * iterator and traversing each copy.
   */
  def unzip[a,b](pairs: Iterator[(a,b)]): (Iterator[a],Iterator[b]) = {
    val (i,j) = pairs.duplicate
    (i map (_._1), j map (_._2))
  }

  /**
   * Given a Stream of pairs, return two Streams over the first and second
   * elements of the pairs.
   */
  def unzip[a,b](pairs: Stream[(a,b)]): (Stream[a],Stream[b]) =
    (pairs map (_._1), pairs map (_._2))

  /**
   * Indexes the result of groupBy.
   * <p>
   * <code>
   * <p>
   * [0 1 2 3 4 5 6 7 8 9]
   * <br>
   * [a,b,c,c,c,d,d,e,f,f] == [[0],[1],[2,3,4],[5,6],[7],[8,9]]
   * <p>
   * [] == []
   * </code>
   */
  def indexGroups[a,b](xs: Seq[a])(f: a => b) = {
    val i = Iterator from 0
    Stream fromIterator xs.elements groupBy f map (_ map (x => i.next))
  }

  /**
   * Zip or unzip some lists (the zip width is dynamic, so no tuples).  The
   * length of the resulting list is the length of the shortest input list.
   * <p>
   * <code>
   * zipx([[a,b,c,d,e],[f,g,h,i,j],[k,l,m,n]])
   *   == [[a,f,k],[b,g,l],[c,h,m],[d,i,n]]
   * <p>
   * unzipx([[a,f,k],[b,g,l],[c,h,m],[d,i,n]])
   *   == [[a,b,c,d],[f,g,h,i],[k,l,m,n]]
   * </code>
   */
  def zipx[a](xss: Seq[Iterable[a]]): Stream[Seq[a]] = {
    val is = xss map (_.elements)
    def rec: Stream[Seq[a]] =
      if (is forall (_.hasNext)) Stream.cons(is map (_.next), rec)
      else Stream.empty
    rec
  }

  /**
   * Zip or unzip some lists (the zip width is dynamic, so no tuples).  The
   * length of the resulting list is the length of the shortest input list.
   */
  def zipx[a](xss: Iterable[Seq[a]]) = {
    val width = xss.elements.next.length
    val iters =
      for (i <- 0 until width)
        yield xss.elements map (_(i))
    List(iters: _*)
  }

  /**
   * Concatenate iterators.
   */
  def concat[a](iters: Seq[Iterator[a]]) =
    iters reduceLeft ((x: Iterator[a], y: Iterator[a]) => x ++ y)

  /**
   * Eliminate consecutive duplicates.
   */
  def uniq[a](xs: Seq[a]) = groupPairwiseBy(xs)(_==_) map (_.head)

  /**
   * A specialization of argmin that uses the identity function as the
   * comparison key.
   * <p>
   * <code>
   * argminId([b,a,c]) == (a,1)
   * <p>
   * argminId([]) == error
   * </code>
   */
  def argminId[a <: Ordered[a]](xs: Iterator[a]) = argmin(xs)(x => x)
  def argmin[a, b <: Ordered[b]](xs: Iterator[a])(f: a => b) = {
    val i = Iterator from 0
    val first = xs.next
    ((first, f(first), i.next) /: xs) { (p:(a,b,Int),y:a) =>
      val fy = f(y)
      val iy = i.next
      if (p._2 < fy) p else (y,fy,iy)
    }
  }

  /**
   * Sort-merge streams. Assumes that the stream are each sorted already.
   */
  def merge[a, b <: Ordered[b]](streams: Seq[BufferedIterator[a]])(f: a => b): Stream[a] = {
    def loop: Stream[a] = {
      val nonempty = streams filter (_ hasNext)
      if (nonempty isEmpty) {
        Stream empty
      } else {
        val (miniter,minvalue,minindex) = argmin( nonempty )( x => f(x head) )
        val p = miniter.head
        nonempty(minindex).next
        Stream cons (p, loop)
      }
    }
    loop
  }

  /**
   * Get the i-th element of a flatten list of lists.
   * <p>
   * <code>coord(7, [[a,b],[],[c,d,e],[f],[g,h,i],[j,k]]) == h</code>
   * @deprecated Use instead: <code>xss.flatMap(x=>x)(0)</code>.
   */
  def coord[a](i: Int, xss: Iterable[Seq[a]]): (Int, Int) = {
    var x = i
    var y = 0
    for (val xs <- xss) {
      if (x <= xs.length) { return (x,y) }
      else { x -= (xs.length + 1); y += 1 }
    }
    (x,y)
  }

  /**
   * Convenience constructor for converting an Iterator into an ArrayBuffer.
   * @deprecated Use instead: <code>xs.toElements</code>.
   */
  def newArrayBuffer[a](xs: Iterator[a]) = {
    val buf = new mut.ArrayBuffer[a]
    buf ++= xs
    buf
  }

  //
  // Trees
  //

  /**
   * A simple tree class where each node (Tree) is a Leaf or a Branch, and each
   * Branch contains a list of other Trees.
   */
  object Tree {
    /**
     * A simple tree class where each node (Tree) is a Leaf or a Branch, and each
     * Branch contains a list of other Trees.
     */
    abstract class Tree[a] {
      /**
       * Show the tree as a string.
       * <code>
       * <p>
       * Leaf("a") -><p>
       * a<p>
       * <p>
       * Branch(List(Leaf("a"), Leaf("b"))) -><p>
       *   a<p>
       *   b<p>
       * <p>
       * Branch(List(Leaf("a"), Branch(List(Leaf("b"), Leaf("c"))), Leaf("d")))<p>
       * -><p>
       *   a<p>
       *     b<p>
       *     c<p>
       *   d<p>
       * </code>
       */
      def show(showLeaf: a => String): String = {
        def r(t: Tree[a]): Stream[String] = t match {
          case Branch(ts) => Stream concat (ts map r) map ("  " + _)
          case Leaf(x) => List(showLeaf(x)) toStream
        }
        r(this) mkString "\n"
      }
      /**
       * Access a node via the given index path.
       */
      def get(path: Seq[Int]): a = (this, path) match {
        case (Leaf(x),    Seq()        ) => x
        case (Branch(ts), Seq(x, xs@_*)) => ts(x) get xs
        case _ => throw new Exception("invalid path")
      }
      /**
       * Flatten the leaves into a single stream.
       */
      def flatten: Stream[a] = this match {
        case Branch(ts) => Stream concat (ts map (_.flatten))
        case Leaf(x)    => Stream cons (x, Stream empty)
      }
    }
    case class Branch[a](ts: Seq[Tree[a]]) extends Tree[a]
    case class Leaf[a](x: a) extends Tree[a]

    /**
     * Build a tree whose i-th level branch has a fanout of xs(i).
     */
    def treeFromFanouts[a](gen: => a, fanouts: Seq[Int]): Tree[a] =
      fanouts match {
        case Seq() => Leaf(gen)
        case Seq(fanout, rest@_*) =>
          Branch(replicate(fanout, treeFromFanouts(gen, rest)).toArray)
      }
  }

  case class TreeNode[a](value: a, children: Seq[TreeNode[a]]) {
    def show = {
      import misc._
      def r(n: TreeNode[a], lvl: Int): Stream[String] = {
        Stream cons (
          "  " * lvl + n.value,
          Stream concat (n.children map (n => r(n, lvl+1)))
        )
      }
      r(this,0) mkString "\n"
    }
  }

  /**
   * A boolean expression tree contains nodes corresponding to And, Or, Not,
   * and Leaf. And, Or contain any number of children.
   */
  abstract class BoolTree[a] {
    def sat(f: a => Boolean): Boolean = this match {
      case And(ts) => ts forall (_ sat f)
      case Or(ts)  => ts exists (_ sat f)
      case Not(t)  => !(t sat f)
      case Leaf(x) => f(x)
    }
    def map[b](f: a => b): BoolTree[b] = this match {
      case And(ts) => And(ts map (_ map f))
      case Or(ts)  => Or(ts map (_ map f))
      case Not(t)  => Not(t map f)
      case Leaf(x) => Leaf(f(x))
    }
    def flatten[b](f: a => b): Stream[b] = this match {
      case And(ts) => Stream concat (ts map (_ flatten f))
      case Or(ts)  => Stream concat (ts map (_ flatten f))
      case Not(t)  => t flatten f
      case Leaf(x) => Stream cons (f(x), Stream empty)
    }
    // TODO: this looks a candidate for some nicer monadic-style sum.
    /**
     * f returns false if the leaf is to be pruned out, and true otherwise.
     */
    def prune[b](f: a => Option[b]): Option[BoolTree[b]] =
      this match {
        case And(ts) => {
          val out = ts map (_ prune f) filter (_ isDefined) map (_ get)
          if (out isEmpty) None else Some(And(out))
        }
        case Or(ts) => {
          val out = ts map (_ prune f) filter (_ isDefined) map (_ get)
          if (out isEmpty) None else Some(Or(out))
        }
        case Not(t) => t prune f match {
          case Some(x) => Some(Not(x))
          case None => None
        }
        case Leaf(x) => f(x) match {
          case Some(y) => Some(Leaf(y))
          case None => None
        }
      }
  }
  case class And[a](ts: Seq[BoolTree[a]]) extends BoolTree[a]
  case class Or[a](ts: Seq[BoolTree[a]]) extends BoolTree[a]
  case class Not[a](t: BoolTree[a]) extends BoolTree[a]
  case class Leaf[a](x: a) extends BoolTree[a]

  //
  // Performance work-arounds.
  //

  /**
   * The standard library Iterator.dropWhile is inefficient and non-scalable,
   * as it is recursive.  This is an efficient (tail-recursive) implementation.
   */
  def dropWhile[a](iter: Iterator[a])(p: a => Boolean) = {
    def loop: Iterator[a] = {
      if (iter hasNext) {
        val x = iter.next
        if (p(x)) loop
        else Iterator single x append iter
      } else {
        iter
      }
    }
    loop
  }

  /**
   * Take an elements from a list.
   */
  def take[a](src: Seq[a], n: Int) = {
    val count = n min src.length
    val dst = new Array[a](count)
    src.elements take count copyToArray (dst, 0)
    dst
  }

  //
  // Misc
  //

  /**
   * Return the positive (unsigned int) modulo.
   */
  def mod(n: Int, m: Int) = {
    val r = n % m
    if (r < 0) r + m else r
  }

  /**
   * Return a map that assigns unique IDs to distinct objects.
   */
  class IdMapper[a] extends mut.HashMap[a,Int] {
    val i = Iterator from 0
    override def default(k: a) = {
      val n = i.next
      this(k) = n
      n
    }
  }

  /**
   * Serialize an Int to a String of four bytes (little-endian).
   */
  def serializeInt(i: Int) =
    Array(
      (i >>>  0) & 0xff,
      (i >>>  8) & 0xff,
      (i >>> 16) & 0xff,
      (i >>> 24) & 0xff
    ).map(_.toChar).mkString
}
