open Core_kernel

let pong =
  { bodies = [ `Left_right ; `Left_right; `Collision ] }

(*
   n dimensional game state -> n dimensional configuaration space.
   could factor along bodies. e.g.
   pong = 1 + 1 + 2 dimensional and have two 1 dimensional configuration spaces
   and one 2 dimensional space, but could have one 4d space.

   concepts:
   - map faithfullly into other configuration spaces
   - project lossfully onto other configuration spaces
     - e.g., the sum of all the positions? idk
   - map OSTENSIBLY lossfully onto other configuration spaces, but in a way
     which is actually lossless due to the determinsm of the game (e.g., the clock
     in pong )
   - a list of discrete state changes.
*)

(* Each 'game' maps out into an abstract configuration space.
   Each abstract configuration space can first be factored into its
   dimensions/disinct manifold factors in multiple ways. Then, once it is
   factored into its configuration space components, each configuration space
   can be represented in a concrete way.

   Some examples of concrete representations
   - (x : R1) -> red 150x80 rectangle with position (x, 20)
   - (x : S1) -> x-colored rectangle with position x
   - (x : S1) -> x-colored rectangle with position (0, 0)
   - (x : R1) -> x as a string, black on white background

   Clearly there is an algebra of these representation maps, which would
   allow us to easily make many different ones. Then, exposing the structure
   of this algebra visually becomes another possibility for an interpretation.
   E.g., instead of a read rectangle, the text 'red rectangle'. Although this
   is already itself a representation in the former sense, so maybe nothing
   new is achieved... On the other hand, "nothing new is achieved" in principle
   after defining a turing machine, which gets at the psychological question of
   ease of interpretation etc later.
*)

(* The configuration spaces set-of-interpretations shows one place where
   things are up for interpretation (going from abstract state to visual)
   but it does not expose any of the higher level structure of the game as
   it evolves over time. E.g., the update logic.

   Each game should also ideally map into a stream of all the data needed
   to reconstruct the config space state. A stream of discrete updates and
   random choices.
*)

(* Another interpretation:
   The kolmogorov interpretation.

   Some games may be determinstic, in which case their 'kolmogorov interpretation'
   is either unit or their code (depending on how you want to look at it).

   Some games may be randomized, in which case their 'kolmogorov interpretation'
   is either their seed randomness or their code plus their seed randomness
   (depending on how you want to look at it).

   Even then it is a big question what is meant by 'their code': is it the ocaml
   program? Is it the ocaml program + the ocaml compiler? + the OS? + the spec for
   the cpu? ocaml IR? assembly/machine code? the video of me producing the code?
   thinking about the code? me? my past experiences and personal history?
*)

(* Overall, I guess the question of the piece is what does each game mean? and what
   does it mean to mean something? the kolmogorov interpretation explores the
   question in one way: the formal way of computability theory where only computability
   in principal matters. 

   The configuration space interpretations explore more 'human-centric' or psychological
   aspects of the question. Some configuration-space maps are more meaningful than others
   because of our psychologies, what metaphors are easier or more comfortable for us to work
   with.
   E.g., hue-pong vs position-pong vs text-pong.

   It also explores the question of forgetting information, of summarizing, discarding etc.
   The pong-clock makes this very clear, but that act of forgetting is inherent in all
   processing of information. When you view the 'full' interpretation of pong, the 4 dimensional
   interpretation that is, you actually see something much higher dimensional. You see it
   displayed on a screen, with random flickerings and imperfections that live in a much higher
   dimensional space. In viewing it you imagine modding out all these higher dimensional imperfections
   and just remember the 4 dimensional summary.
*)

(* We may also have a natural language (e.g., English) description that I write before starting
   programming each game as one of the possible interpretations. In some sense it is the 'source code'
   of the game before being run through the interpreter of my body (mind/fingers). *)

(* Need access to the data of the bodies *)
module Pong = struct
  module State = struct
  end
end

module Descriptive = struct
  let x = 
    object (self)
      method x = self
    end

  let _ = x#x#x#x#x#x#x#x#x

  module Update = struct
    type ('s, 'ds) t =
      | New_state of 's
      | Delta of 'ds
  end
end

module Pos = struct
  type t = float * float
end

module Physical = struct
  module Update = struct
    type 's t =
      | New_state of 's
  end

  module Program = struct
    module Body = struct
      type ('s, 'whole) body =
        { update : 's -> 'whole -> 's
        ; init   : 's
        ; pos    : 's -> Pos.t
        }

      type _ t =
        T : 's Kind.t * ('s, 'whole) body -> 'whole t
    end
  end

  class t = 
    object (self)
      method 
    end
end
