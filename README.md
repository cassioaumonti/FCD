# FCD
Control queues in the forest transportation via fuzzy inference system.

Run scripts in this order:
1. fcontroler.R
     sets up fuzzy inference system as designed in the publication;
3. queue_sim.R
     run the queue simulator using the output of the optimization model as parameters;
5. queue_fsim.R
     run the queue simulator using the output of the optimization model as parameters and the fuzzy inference system generates the re-routes for the vehicles;
