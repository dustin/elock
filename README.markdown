# A Simple Distributed Lock Server

This is still an early project, but the goal is pretty simple:

A simple, distributed, fault-tolerant lock server.

# Commands:

## lock

Lock has two forms.  The first is a non-blocking form that either tells you
the lock is already taken, or acquires it.

    lock lock_name

The second form allows you to wait for a certain number of seconds for the
lock to become available:

    lock lock_name timeout_in_seconds

Possible return values:

* 200 - Lock has been acquired
* 409 - The resource is locked -- you lose

Any acquired lock will be held until it's specifically unlocked, unlocked with
unlock\_all, or the client disconnects.

## unlock

Unlock releases the lock with the given key if the current client has acquired
that lock:

    unlock lock_name

Possible return values:

* 200 - The lock has been released
* 403 - You do not own this lock (therefore you didn't release it)

## unlock\_all

Release any locks this client may be holding.

Possible return values:

* 200 - It just blows it all away

## quit

Disconnect your connection.

