

# 0.2.0.0 - 2015-06-08

* `Deck` uses associated types for whether or not it is complete
* New `FullType` alias for `Deck 'Complete`
* Burn card is the *first* card from the deck, rather than the first card
  after players have been dealt.
* Burn card is now available for finished rounds (`Haverer.Round.getBurnCard`)
* A list of survivors of each round can be obtained, once the round is over
  (`Haverer.Round.survivors`)

# 0.1.0.0 - 2015-06-07

Initial release.
