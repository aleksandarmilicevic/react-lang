package react.codegen.ros

/*

assume a bash shell

fist we need to start the master:
$ roscore
furthermore, Aleks also had a react_core service

to start context (a ros node)
-sourcing some config variables (in Aleks example)
$ source /opt/ros/DISTRONAME/setup.bash

-launch the process corresponding to the context
$ rosrun <package> <executable>


we should generate the appropriate script to make the process easy:
-one script to setup the core and run the main context
-for each context, one script to start that context


*/
