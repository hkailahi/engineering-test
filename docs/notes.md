# Notes

## Initial Brainstorm

While reading prompt the following thoughts popped into my mind…
* Concurrency = STM
* Tracking jobs = Queue
* Limit to concurrently executing jobs = Bounded
* Some sort of bounded queue with stm and forking threads to execute tasks
* Tracking executions, handling cancellations, independent executions

This also reminded me of some previous work:
* I wrote some words on `async` > `forkIO` on internal wiki at a previous job.
  * I also remember watching a related Snoyman talk on tips for avoiding (async) exceptional control flow.
* This task is similiar to a minimal version of the `ingest-tracker` and `ingest-driver` codebases I maintained at a previous job.

## Initial Questions

Who owns the task definition? Is this all internal usage?

Is the executor calling a callback? Executing tasks itself? Where/How are these tasks defined? Is deciding this myself the point of the assignment?

Maybe we provide base functionality / DSL and they can compose there own tasks with? Which we execute?

Or handle call-backs or code-bundles with an executable entrypoint? Or maybe we
need all of the above? That's what we needed at TVision for `ingest-tracker` which caused a fair bit of pain.

## Assumptions

1. This is an MVP, whereas the actual product will (use):
   1. Protocol: REST APIs
      1. Final version will likely be a backend service with API that tracks & persists task queue and execution states in a database
         1. MVP can use single in-memory executor, while Final might include multiple executors (and drivers) coordinating over some durable, central data store
      2. Actual product won't use stdin/stdout
2. Task Definition
   1. We own task primitives ("steps"), users (internal and external) use our task language to define tasks which our task runner executes
      1. No arbitrary code bundles for executing arbitrary code, no callback for running process elsewhere (aside from the ones we provide for tracking)
   2. Users define their own tasks by composing task primitives ("steps") into larger jobs, which they could request by name/id by with API calls for scheduling execution and polling for results
      1. For now, we'll just expose the DSL for creating tasks but not worry about supporting them with dynamic task library + request parsing
3. Tasks IDs are assigned by executor rather than user-defined
4. This is an application for internal use, rather than public library
   1. Custom preludes and heavier dependencies aren't a huge deal in this context

## Tradeoffs

1. Closed app vs Open library
   1. Dependencies like `lens`, `generic-opts`, `relude`, etc add some weight & complexity, but those were tradeoffs I'm was willing to make for automate boilerplate + quickly pull in machinery
      1. Within this task specifically, since I don't have to care about things like optimizing compile-times, beginner-friendliness, etc

## TODOs

1. Better I/O (if stdin/stdout remain necessary)
   1. Use a proper logger instead of `putStrLn`
   2. Stop using `getLine`
   3. Stream stdin/stdout with resource-handling patterns (via conduit & resourcet)
2. Address unhandled edge cases pointed out in FIXMEs and TODOs
3. Missing functionality I ran out of time to implement
   1. > Upon task completion, the executor will promptly notify the driver of ... any task-specific result value generated.
      1. Right now result values beyond status are thrown into the `void`, but it should be straightforward to bubble up result value
   2. > For long-running tasks, the executor will periodically send status updates to the driver, ensuring transparency in task progress.
      1. I have ideas for different ways of doing this, but I didn't get around to implementing them
