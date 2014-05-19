$fa = 4;
$fs = 1;

body();

module body() {
	union() {
		//frame
		hull() {
			centered_cube([10,20,10]);
			translate([-7.5,0,2.5]) centered_cube([5,5,5]);
		}
		translate([-12.5,0,2.5]) centered_cube([5,5,5]);
		//wheels
		translate([0,11,-2]) wheel(5,2);
		translate([0,-11,-2]) wheel(5,2);
		translate([-12.5,0,0]) castor_wheel(3,2,3,4);
		//sensor
		translate([5,0,2.5]) centered_cube([1,2,1]);
	}
}


module sensor(v) {
	centered_cube(v);
}

module wheel(radius = 5, width = 1) {
	translate([0, width/2,0]) rotate([90,0,0]) cylinder(r = radius, h = width);
}

module castor_wheel(radius = 5, width = 1, delta_x = 5, delta_z = 6) {
	translate([-width/2,-width/2,-1]) cube([width,width,1]);
	translate([-delta_x, 0,-delta_z]) wheel(radius, width);
	hull(){
		translate([-width/2,width/2,-1]) cube([width,0.5,1]);
		translate([-width/2-delta_x,width/2,-1-delta_z]) cube([width,0.5,1]);
	}
	hull(){
		translate([-width/2,-width/2-0.5,-1]) cube([width,0.5,1]);
		translate([-width/2-delta_x,-width/2-0.5,-1-delta_z]) cube([width,0.5,1]);
	}
}

//helper
module centered_cube(v) {
	translate(-v/2) cube(v);
}
