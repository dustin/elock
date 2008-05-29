# Phase 1: Basic semantics

* Auto-free locks on disconnect.
* Timeout locks.
* Register for a lock release notification so you can wait for a lock
  (and let 'em block the client for a while).

# Commands need to be defined

lock(timeout?)

## stats

* Overall stats
* Individual lock stats (Who's got it, how long, etc...)

# Phase 2: Durability/Fault Tolerance

* Store lock states in mnesia.
* Demonstrate various failure modes and global lock consistency.