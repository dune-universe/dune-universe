# builder - scheduling and executing jobs

This consists of three programs, a worker, a server, and a client. The single
server contains a queue of jobs, which are consumed by a worker. Any number of
worker can be connected to the server. The client can modify the schedule:
add/remove/modify jobs, also observe a concrete job.

The server keeps persistent state of the job queue (so restarts / crashes are
dealt with). A worker connects, provides some information about itself, and
then waits for a job. Once a job is read and accepted, it is executed by the
worker. Resulting artifacts can be transferred by the client to the server.

The client has the ability to schedule jobs at regular intervals - similar to
crontab - but workers are usually executed in sandboxes/ jailed environments.

Handled and unhandled error conditions:
- worker execution fails (timeout, restart, killed): not handled, but server has a timeout
- worker execution gets a signal: reported to server
- worker can't write job data files -> failure is captured and reported
- worker can't read job output -> logged to client's console (without artifacts gathered)
- worker errors when submitting console output -> ignored
- worker fails communication with server -> job is ignored (the server is responsible for restarting)

A templating mechanism is available, look for `orb-build.template` as examples.
Currently FreeBSD, Debian and Ubuntu are supported, and a repository that
receives jobs is live at https://builds.robur.coop/
