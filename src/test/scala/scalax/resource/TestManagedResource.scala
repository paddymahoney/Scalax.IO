package scalax.resource

import org.junit._
import Assert._

class TestManagedResource {

   @Test
   def mustOpenAndCloseResource() {
      object TestResource {
          var wasOpened = false
          var openCount = 0
          def open() = {
             wasOpened = true
             openCount += 1
             this
          }
          def close() {
             openCount -= 1
          }
          def tryToDoSomething() = if(!wasOpened) error("Tried to access unopened resource!") else ()
           
      }
      for(r <- ManagedResource(TestResource.open())) {
          r.tryToDoSomething()
      }
      assertEquals("Failed to close all connections!", 0, TestResource.openCount)
   }

   @Test
   def mustOpenAndCloseResourceSeveralTimes() {
      object TestResource {
          var wasOpened = false
          var openCount = 0
          def open() = {
             wasOpened = true
             openCount += 1
             this
          }
          def close() {
             openCount -= 1
          }
          def tryToDoSomething() = if(!wasOpened) error("Tried to access unopened resource!") else ()
           
      }
      for(r <- ManagedResource(TestResource.open()); r2 <- ManagedResource(TestResource.open())) {
          assertEquals("Failed to open appropriate number of connections", 2, TestResource.openCount)
          r.tryToDoSomething()
          r2.tryToDoSomething()
      }
      assertEquals("Failed to close all connections!", 0, TestResource.openCount)
   }
}
