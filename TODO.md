
In addition to the many XXX, FIXME, and TODO comments scattered through the
code, the "Roadmap" outlined in the README, and the issues in the tracker,
there are still these other things that need to be done.

* Set up continuous integration on Github
* Generate and publish the documentation automatically
* Get help for how to have cleaner looking test modules (tests/Round.hs is
  quite messy)
  * Clean up the visibility of names, too many things are currently exposed.

## Code cleanliness things

* Explore using `Reader` in `Engine` (for getting the `PlayerSet`)
* Explore using `Engine` in tests to get sequences of `Round`s and `Event`s.
* See where `error` is used and try to eliminate what we can
* Experiment with splitting the `Player` type so that `Active` and `Inactive`
  are their own things.
* Is there a better way of saying "this state (really, pattern match)
  shouldn't be possible" than `error`? Specifically one that gives some kind
  of moral equivalent of a stack trace, since it's _always_ a programming
  error.
* `Haverer.Round` exports `Event` and `Result` as concrete types with all of
  their constructors. This is super-useful for pattern matching, but feels
  like it's leaking too much
