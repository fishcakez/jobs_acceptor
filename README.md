jobs_acceptor
=============

`jobs` rate limited `acceptor_pool` acceptor. Wraps an `acceptor` callback
module to regulated accepting of new sockets. `jobs_acceptor` is an `acceptor`
with arguments `{JobsQueue, Module, Args}`, where `JobsQueue` is the `jobs`
queue. `Module` and `Args` are the `acceptor` callback module and argument.
