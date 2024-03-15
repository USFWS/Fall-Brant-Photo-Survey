####################################
## R scripts for analysis of fall photographic waterfowl surveys, Izembek 
# Lagoon, Alaska, 2017-2019

# Suggested citation: Weiser, E.L. 2022. R scripts for analysis of fall 
# photographic waterfowl surveys, Izembek Lagoon, Alaska, 2017-2019. U.S. 
# Geological Survey software release, https://doi.org/10.5066/P9HNU1WE.

# Developed for:
	# Weiser, E.L., P.L. Flint, D.K. Marks, B.S. Shults, H.M. Wilson, S.J. 
	# Thompson, and J.B. Fischer. 2022. Optimizing surveys of fall-staging 
	# geese using aerial imagery and automated counting. Wildlife Society 
	# Bulletin (in press).

####################################
## This script sets up a function to calculate the footprint of an aerial 
# photo. 

## Software used to develop this script:
	# R version 4.0.3
	# R package glmmTMB_1.0.2.1

# Required input for the function:
	# 1. Altitude of the camera over the ground
	# 2. Focal length at which the photo was taken
	# 3. Tilt of the camera (degrees from vertical)
	# 4. Sensor dimensions: vertical and horizontal.
# The function does not use information on pitch or roll.
# The function returns the area of the photo in the same units (squared) as
# the input altitude value. For example, if altitude is given in meters, the 
# areal estimate is in square meters.

####################################
## Definitions:
# Vertical = parallel to the plane's movement. The "vertical" dimension of 
	# the sensor is the sensor's height if the top of the camera is facing 
	# the nose of the plane.
# Horizontal = perpendicular to the plane's movement.

####################################
## Helper functions:

# Convert radians to degrees:
fn_rad2deg <- function(x) {(x*180)/pi}
# Convert degrees to radians:
fn_deg2rad <- function(x) {(x*pi)/180}

####################################
## Main function:

fn_photoarea <- function(
	alt,		# height of camera above ground in any units (same as 
				# altitude if flying over sea level)
	foc,		# camera/lens focal length in mm
	tilt=5.6,	# absolute value of camera tilt in degrees from vertical
	sensorv=24, # camera sensor vertical dimension in mm
	sensorh=36	# camera sensor horizontal dimension in mm
	){
		## Calculate horizontal and vertical field of view in degrees:
		hfv <- fn_rad2deg(2*atan(sensorh/(2*foc)))
		vfv <- fn_rad2deg(2*atan(sensorv/(2*foc)))
		
		## When a plumb line is drawn from camera to ground, the tilted 
			# field of view will consist of two right-angle triangles. Here 
			# we calculate the angle (from plumb line) at the camera lens
			# and the length of the opposite side (ground distance) for each
			# triangle. Here I'm assuming the camera is tilted to the right,
			# but the resulting ground distance (and thus photo area) will
			# be identical when tilted to the left.
		
		# For the larger triangle (right-hand side of image if camera is 
			# tilted to the right):
		theta1 <- fn_deg2rad(hfv/2 + tilt)	# angle from plumb line
		b1 <- tan(theta1)*alt	# length of opposite side (ground distance)

		# For the smaller triangle:
		theta2 = fn_deg2rad(hfv/2 + tilt - hfv)	
		b2 <- tan(theta2)*alt	# length of opposite side (ground distance)
		# Here, b2 is negative if the field of view is left of the plumb 
			# line, or positive if it's right of the plumb line. But if it's
			# left, it represents additional area of the photo; if it's
			# right, it indicates what is excluded from the photo. Flip the
			# sign accordingly:
		b2 <- -1*b2
		
		# Add the two distances to get the total horizontal ground distance: 
		h <- b1 + b2
		
		# Now calculate the vertical distance covered by the image. This is 
			# not affected by tilt, which may be counterintuitive. If you 
			# have uniformly sized objects, you can compare one from the far
			# left edge with one from the far right edge to confirm that
			# they're the same (vertical) size even if your camera is tilted.
		v <- 2*alt*tan(fn_deg2rad(vfv)/2) 	

		# Calculate photo area as length x width:
		area <- h*v		# in (units of alt)^2
		
		return(area)	
}	# end main function

# Example:
#fn_photoarea(alt=450, foc=200, tilt=5.6, sensorv=24, sensorh=36)
# returns 4416.4 sq m for a photo taken at 100 m above ground, focal length 
	# 200 mm, tilt 5.6 degrees, sensor 24 x 36 mm

