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

## conn\_id

Retrieve your current session ID.  This may be resumed later.

Returns:

* 200 [some\_string]

## conn\_id [string]

Attempts to set your connection ID to the given value.

This allows you to resume a session if you get disconnected.

Returns:

* 200 - Resumed
* 403 - Could not resume connection (you either already have a new one, or
        that one belongs to someone else)

## set\_timeout [integer]

Set the amount of time after disconnect (in milliseconds) before your locks
are all automatically freed.  Default is 30,000 (30 seconds).

Returns:

* 200 - the new value was set

## quit

Disconnect your connection.

## stats

Get some server stats.

Returns:

* 200 STATS - beginning of a stat stream (see below)

The stats output is multi-line, so the protocol handling is a little different
from all of the other commands.

The initial response is

    200 STATS

which is followed by several values in the following form:

    STAT [key] [value]

and then one terminating value:

    END

A complete example is as follows:

    200 STATS
    STAT clients 0
    STAT locks 0
    STAT monitoring 0
    END
