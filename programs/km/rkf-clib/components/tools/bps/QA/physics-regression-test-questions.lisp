;;
;; $Id: physics-regression-test-questions.lisp,v 1.3 2006/05/16 18:57:00 jfan Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(setq *PHYSICS-TEST-QUESTION-LIST* '(				     
                        "There is a move.
                         The distance of the move is 100 m.
                         The duration of the move is 5 s.
                         What is the velocity of the move?"

			"There is a move.
                         The distance of the move is 100 m.
                         The duration of the move is 5 s.
                         What is the average velocity of the move?"

			"There is a move.
                         The distance of the move is 100 m.
                         The duration of the move is 5 s.
                         The initial velocity of the move is 0 m/s.
                         What is the acceleration of the move?"

			"There is a move.
                         The acceleration of the move is 8 meter per second squared.
                         The duration of the move is 5 s.
                         The final velocity of the move is 40 m/s.
                         What is the distance of the move?"

			"There is a fall.
                         The distance of the fall is 490 m.
                         What is the duration of the fall?"

			"There is a move.
                         The distance of the move is 230 km.
                         The duration of the move is 3.25 h.
                         What is the average velocity of the move?"

			"There is a move.
                         The duration of the move is 6 s.
                         The initial velocity of the move is 12 m/s.
                         The final velocity of the move is 25 m/s.
                         What is the acceleration of the move?"

			"There is a move.
                         The duration of the move is 6 s.
                         The initial velocity of the move is 12 m/s.
                         The final velocity of the move is 25 m/s.
                         What is the distance of the move?"

			"There is a fall.
                         The duration of the fall is 3.5 s.
                         What is the distance of the fall?"

			"There is a move.
                         The vertical distance of the move is 7.5 m.
                         The initial horizontal velocity of the move is 4.5 m/s.
                         The initial vertical velocity of the move is 0 m/s.
                         What is the horizontal distance of the move?"

			"There is a move.
                         The vertical distance of the move is 56 m.
                         The horizontal distance of the move is 45 m.
                         The initial vertical velocity of the move is 0 m/s.
                         What is the initial horizontal velocity of the move?"

			"There is a move.
                         The initial horizontal velocity of the move is 22.2 m/s.
                         The horizontal distance of the move is 36 m.
                         The initial vertical velocity of the move is 0 m/s.
                         What is the vertical distance of the move?"

			"A tiger leaps.
                         The vertical distance of the leap is 7.5 m.
                         The initial horizontal velocity of the leap is 4.5 m/s.
                         The initial vertical velocity of the leap is 0 m/s.
                         What is the horizontal distance of the leap?"

			"A ball is thrown.
                         The initial vertical velocity of the throw is 0 m/s.
                         The vertical distance of the throw is 56 m.
                         The horizontal distance of the throw is 45 m.
                         What is the initial horizontal velocity of the throw?"

			"A ball is thrown.
                         The initial vertical velocity of the throw is 0 m/s.
                         The initial horizontal velocity of the throw is 22.2 m/s.
                         The horizontal distance of the throw is 36.0 m.
                         What is the vertical distance of the throw?"

			"A package is thrown.
                         The initial horizontal velocity of the throw is 160 km/h.
                         The initial vertical velocity of the throw is 0 km/h.
                         The vertical distance of the throw is 160 m.
                         What is the duration of the throw?"

			"An auto is moving.
                         The initial velocity of the moving is 12 m/s.
                         The final velocity of the moving is 25 m/s.
                         The duration of the moving is 6.0 s.
                         The acceleration of the moving is constant.
                         What is the acceleration of the moving?"

			;;;Henceforth are Won Ng's physics questions.

"A car moves.
The initial velocity of the car is 12 m/s.
The final velocity of the car is 25 m/s.
The duration of the move is 6.0 s.
What is the distance of the move? "

"A ball is dropped from a tower.
The height of the tower is 70 m.
The ball falls for 1 s.
What is the distance of the fall?"

"A stone is dropped from the top of a cliff.
The stone falls to the ground.
The duration of the fall is 3.50 s.
What is the height of the cliff?"

"A tiger leaps from a rock.
The height of the rock is 7.5 m.
The initial horizontal velocity of the leap is 4.5 m/s.
The initial vertical velocity of the leap is 0 m/s.
What is the horizontal distance of the leap?"

"A football is kicked from the ground.
The direction of the kick is 37 degrees.
The initial velocity of the kick is 20.0 m/s.
The football falls to the ground.
What is the duration of the kick?"

			;;;Henceforth are John Thompson's physics questions.

"An object is moving along a straight line.
The speed of the moving is constant.
What is the acceleration of the moving?"

"An object is moving along a circle.
The speed of the moving is constant.
What is the acceleration of the moving?"

"An object is moving.
The object is a projectile.
The force on the object is gravity.
What is the acceleration of the moving?"

"An object is traveling.
The distance of the traveling is 230 km.
The duration of the traveling is 3.25 h.
What is the average speed of the traveling?"

"A bird is flying.
The speed of the flying is 25 km/h.
The distance of the flying is 15 km.
What is the duration of the flying?"

"A person is driving along a straight road.
The speed of the driving is 110 km/h.
The duration of the driving is 2.0 s.
What is the distance of the driving?"

"A car is moving.
The duration of the moving is 6.2 s.
The initial velocity of the moving is 0 m/s.
The final velocity of the moving is 95 km/h.
What is the average acceleration of the moving?"

"A car is moving.
The initial velocity of the moving is 12 m/s.
The final velocity of the moving is 25 m/s.
The duration of the moving is 6.0 s.
The acceleration of the moving is constant.
What is the acceleration of the moving?"

"A car is moving.
The initial velocity of the moving is 20 m/s.
The final velocity of the moving is 0 m/s.
The distance of the moving is 85 m.
The acceleration of the moving is constant.
What is the acceleration of the moving?"

"A plane is moving.
The initial velocity of the moving is 0 m/s.
The final velocity of the moving is 30 m/s.
The acceleration of the moving is constant.
The acceleration of the moving equals 3.0 m/s^2.
What is the distance of the moving?"

"A car is falling from the top of a cliff.
The initial vertical velocity of the falling is 0 km/h.
The final vertical velocity of the falling is 90 km/h.
What is the duration of the falling?"

"A stone falls from the top of a cliff.
The initial vertical velocity of the falling is 0 m/s.
The duration of the falling is 3.50 s.
What is the distance of the falling?"

"A stone falls from the top of a cliff.
The initial vertical velocity of the falling is 0 m/s.
The duration of the falling is 3.50 s.
The distance of the falling equals the height of the cliff.
What is the height of the cliff?"

"An airplane is flying.
The horizontal velocity of the airplane is 160 km/h.
A package is dropped from the airplane.
The package falls to the ground.
The initial horizontal velocity of the falling is 160 km/h.
The initial vertical velocity of the falling is 0.
The vertical distance of the falling is 160 m.
What is the duration of the falling?"

"A child is on a sled.
The child is part of an object.
The sled is part of the object.
The object is moving.
The mass of the object is 60.0 kg.
The acceleration of the moving is 1.15 m/s^2.
What is the force on the object?"

"A rider is on a bike.
The rider is part of an object.
The bike is part of the object.
The object is moving.
The force on the object is 225 N.
The acceleration of the move is 2.20 m/s^2.
What is the mass of the object?"

"An object is moving inside a centrifuge.
The mass of the object is 9.0 g.
The acceleration of the moving is 10000 gravities.
What is the force on the object?"

"A rope is pulling a car.
The mass of the car is 1050 kg.
The car is moving.
The acceleration of the moving is 1.20 m/s^2.
What is the force on the car?"

"A rope is pulling a car.
The mass of the car is 1050 kg.
The car is moving.
The acceleration of the moving is 1.20 m/s^2.
The force on the rope equals the force on the car.
What is the force on the rope?"

"The mass of an astronaut is 66 kg.
The astronaut is on the Earth.
What is the weight of the astronaut?"

"The mass of an astronaut is 66 kg.
The astronaut is on the moon.
On the moon gravity equals 1.7 m/s^2.
What is the weight of the astronaut?"

"The mass of an astronaut is 66 kg.
The astronaut is on planet X.
On planet X gravity equals 3.7 m/s^2.
What is the weight of the astronaut?"

"The mass of an astronaut is 66 kg.
The astronaut is in space.
In space gravity equals 0.
What is the weight of the astronaut?"

"A car is moving.
The mass of the car is 1100 kg.
The initial velocity of the car is 90 km/h.
The final velocity of the car is 0.
The duration of the moving is 8.0 s.
What is the average force on the car?"

"An elevator hangs from a cable.
The mass of the elevator is 2100 kg.
A machine is above the cable.
The machine is pulling the cable.
The force on the cable is 21750 N.
What is the acceleration of the pull?"

"A crate is sliding on a floor.
The mass of the crate is 35 kg.
The coefficient of kinetic friction of the sliding is 0.30 units.
The horizontal velocity of the crate is constant.
What is the horizontal force on the crate?"

"A 2nd crate is sliding on a floor.
The mass of the 2nd crate is 35 kg.
The coefficient of kinetic friction of the sliding is 0.
The horizontal velocity of the 2nd crate is constant.
What is the horizontal force on the 2nd crate?"

"A box is sliding on a horizontal floor.
The mass of the box is 5.0 kg.
The horizontal force on the box is 40.0 N.
What is the coefficient of static friction?"

"A 2nd box is sliding on a horizontal floor.
The mass of the 2nd box is 5.0 kg.
The horizontal force on the 2nd box is 40.0 N.
The acceleration of the sliding is 0.70 m/s^2.
What is the coefficient of kinetic friction?"

))
