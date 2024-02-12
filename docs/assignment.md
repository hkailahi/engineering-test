# Engineering take home assignment

## Overview

Your objective is to create a concurrent task executor that interfaces with a driver for task execution. The executor and driver continously exchange messages 
according to a to-be-developed protocol with the goal to get tasks communicated from the driver to the executor. You can assume the driver is a server
process running on a different machine, scheduling tasks on many executors. The tasks sent to the executor will vary across a spectrum of business cases and 
demand specific input parameters for each task. To optimize resource utilization, the executor must have the capability to execute multiple tasks simultaneously.

As the lead engineer you have the liberty to design the communication protocol between the driver and the executor with the following requirements:

* An executor is a server process that continously exchanges messages with the driver.
* Executor and driver communicate over STDIN and STDOUT channels for simplicity.
* An executor should have a maximum amount of tasks it can execute concurrently.
* Each task carries a unique identifier, the task type, and input parameters, depending on the type of task.
* Upon task completion, the executor will promptly notify the driver of the task result, which may include success or failure status, along with any 
  task-specific result value generated.
* For long-running tasks, the executor will periodically send status updates to the driver, ensuring transparency in task progress.
* The driver reserves the right to request task cancellation mid-execution, adding flexibility to task management.
* Cancellation or failure of one task shoudn't influence other tasks running concurrently.

For this assignment's purposes, you may employ task types like "Bubble Bath Optimization," "Squirrel Patrol," and "Unicorn Wrangling," each executed 
with a simulated delay using a sleep function. Duration of the sleep could be denoted as per-task input parameter. 

## Presentation 

Once you've wrapped up the project, be sure to push it to a GitHub repository for presentation. Remember to extend invitations to @hollyos, @justinwoo, @ken-scarf and @alexbiehl 
to collaborate on the repository.

## Guideline 

* Allocate a maximum of 1.5 hours to this task. While delivering a fully polished assignment is appreciated, it's not mandatory for progressing to the next round.

* Embrace pragmatism. Taking shortcuts is not only acceptable but necessary. 

* Document and be able to discuss your decisions. We value communication over code.

* Feel free to utilize any library available on Hackage to bolster your implementation.
