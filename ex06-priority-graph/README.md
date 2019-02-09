Priority Graph - a task prioritization library
====================

Overview
--------------------

The basic idea of this library is that completing a task `a` may bring
us closer to completing some partn tasks `p1`, `p2`, etc. For each of
the parents task `a` has it's parent-relative priority. This relation
may be represented as a graph:

```hs
type Priority = Double
type Parent = PriorityGraph
data PriorityGraph a = (a, [(Parent a, Priority)])
```

Top-level tasks (tasks with no parent) can be represented as:

```hs
(a, [(), Priority])
```
to give them some base priority.

The effective priority of the task `a` is a sum of it's parents
priorities, multiplied by relative priorities from `a` to given parent.
For simplicity priorities should be expressed as a number from 0 to 1.
