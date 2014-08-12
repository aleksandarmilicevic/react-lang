package react.verification

import react._
import java.nio.ByteBuffer

//To close the systems and simulate elements which are not in the systems, e.g. user-input.
//TODO extends Robot to streamline the process ??
//TODO move the ghost to the compiler
abstract class GhostAgent {

    def length(world: World): Int
    def serialize(world: World, out: ByteBuffer): Unit
    def deserilize(world: World, in: ByteBuffer): Unit

}
