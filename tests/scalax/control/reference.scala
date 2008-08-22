import scalax.control._
import scalax.testing._

object RefTests extends TestSuite("Ref"){
  "initial value" is {
    val ref = Ref(1);
    assertEq(ref(), 1);
  }

  "set" is {
    val ref = Ref(1);
    ref() = 2;
    assertEq(ref(), 2);    
  }

  "withValue" is {
    val ref = Ref(1);

    ref.withValue(3){
      assertEq(ref(), 3)
    }

    assertEq(ref(), 1);
  }

  "modify" is {
    val ref = Ref(1);
  
    ref.modify(_+1);

    assertEq(ref(), 2);
  }
}
