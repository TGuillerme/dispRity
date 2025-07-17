# Road to 1.10

 * `multi.ace` updates
 * `dispRity` can now handle abundance data through the `abundance` argument in all main functions.
  - [ ] allow new internal structure object `"$abundance"` which is a list of matrices of taxa * site?
  - [ ] modify internal architecture to pull from $abundance for calculations (replicating rows relative to abundance or to bootstrap)
  - [ ] allow plots to show abundance (points size scale with abundance)
