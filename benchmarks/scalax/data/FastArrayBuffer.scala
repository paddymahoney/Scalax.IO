import scalax.testing.SizedBenchmark;
import scalax.data.FastArrayBuffer;
import scala.collection.mutable.ArrayBuffer;

object Utils{
  type Buffer = RandomAccessSeq.Mutable[Int]

  def newFastArrayBuffer(size : Int) = {
    val buffer = new FastArrayBuffer[Int]();
    (0 until size).foreach(i => buffer += i);
    buffer;
  }


  def newArrayBuffer(size : Int) = {
    val buffer = new ArrayBuffer[Int]();
    (0 until size).foreach(i => buffer += i);
    buffer;
  }
}

import Utils._;

object InplaceReverse extends SizedBenchmark{
  

  abstract class Reverse(name : String) extends Benchmark[Buffer](name){
    def test(t : Buffer){
      var i = 0;

      while (i < t.length  / 2){
        val temp = t(i);
        val other = t.length - i - 1;
        t(i) = t(other);
        t(other) = temp;
        i += 1;
      }
    }
  }


  new Reverse("ArrayBuffer"){
    def data(size : Int) = newArrayBuffer(size);
  }

  new Reverse("FastArrayBuffer"){
    def data(size : Int) = newFastArrayBuffer(size);
  }
}

object SelfAppend extends SizedBenchmark{
  new Benchmark[(FastArrayBuffer[Int], FastArrayBuffer[Int])]("FastArrayBuffer"){
    def data(size : Int) = (newFastArrayBuffer(size), newFastArrayBuffer(size))
    def test(x : (FastArrayBuffer[Int], FastArrayBuffer[Int])) = x._1 ++= x._2
  }

  new Benchmark[(ArrayBuffer[Int], ArrayBuffer[Int])]("ArrayBuffer"){
    def data(size : Int) = (newArrayBuffer(size), newArrayBuffer(size))
    def test(x : (ArrayBuffer[Int], ArrayBuffer[Int])) = x._1 ++= x._2
  }
}
