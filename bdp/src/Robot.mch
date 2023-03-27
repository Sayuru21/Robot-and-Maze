/* Robot
 * Author: Sayuru_W1790000
 * Creation date: 01/02/2023
 */

MACHINE
    Robot
SEES
    Maze

SETS
    TOAST_NOTIFICATION = {
       SUCCESSFUL_MOVE ,
       INTERNAL_BLOCK_DETECTED_CANNOT_MOVE ,
       EXTERNAL_WALL_DETECTED_CANNOT_MOVE ,
       TELEPORTATION_SUCCESSFUL ,
       TELEPORT_TO_SAME_WALLS_DENIED ,
       TELEPORT_TO_INTERNAL_WALLS_DENIED ,
       TELEPORT_TO_EXIT_AT_START_DENIED ,
       UNSUCCESSFUL_MOVE } ;
    BOOLEAN_SET = { YES , NO } ;
    MOVEMENT_DIRECTIONS = { NORTH , SOUTH , EAST , WEST , TELEPORT }







ABSTRACT_VARIABLES
   xPositionOfRobot ,
   yPositionOfRobot ,
   visitedSquaresOfMaze ,
   movedDirectionsOfMaze

INVARIANT

    xPositionOfRobot : x_axis_range_of_maze &
    yPositionOfRobot : y_axis_range_of_maze &
    xPositionOfRobot : NATURAL1 &
    yPositionOfRobot : NATURAL1 &
    xPositionOfRobot |-> yPositionOfRobot : free_square_of_maze &
    visitedSquaresOfMaze : seq ( free_square_of_maze ) &
    movedDirectionsOfMaze : seq ( MOVEMENT_DIRECTIONS )

INITIALISATION

    xPositionOfRobot := prj1 ( x_axis_range_of_maze , y_axis_range_of_maze ) ( initial_square_of_maze ) ||
    yPositionOfRobot := prj2 ( x_axis_range_of_maze , y_axis_range_of_maze ) ( initial_square_of_maze ) ||
    visitedSquaresOfMaze := [ initial_square_of_maze ] ||
    movedDirectionsOfMaze := <>


OPERATIONS

  /* Moves the Robot North - Checks for the following set constraints
   * Moved square is not a blocked square
   * Moved square is not outside the border of the wall
   * Checks if the robot has reached final square
   */

  toast_notification <-- MoveNorth =

    PRE
        not ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) ) & ( xPositionOfRobot |-> yPositionOfRobot : maze_area )
    THEN
        IF
            ( xPositionOfRobot |-> yPositionOfRobot + 1 : free_square_of_maze ) & ( xPositionOfRobot |-> yPositionOfRobot + 1 : maze_area )

        THEN

            yPositionOfRobot := yPositionOfRobot + 1 ||

            visitedSquaresOfMaze := visitedSquaresOfMaze <- ( xPositionOfRobot |-> yPositionOfRobot ) ||

            toast_notification := SUCCESSFUL_MOVE ||

            movedDirectionsOfMaze := movedDirectionsOfMaze <- NORTH

        ELSIF
            ( xPositionOfRobot |-> yPositionOfRobot + 1 : internal_walls_of_maze )

        THEN
            toast_notification := INTERNAL_BLOCK_DETECTED_CANNOT_MOVE

        ELSE
             toast_notification := EXTERNAL_WALL_DETECTED_CANNOT_MOVE

            END
        END ;

 /* Moves the robot south - Checks for the following set constraints
  *  Moved square is not a blocked square
  *  Moved square is not outside the border of the wall
  *  Checks if the robot has reached final square
  */

  toast_notification <-- MoveSouth =

    PRE
         not ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) ) & ( xPositionOfRobot |-> yPositionOfRobot : maze_area )
    THEN

        IF
            ( xPositionOfRobot |-> yPositionOfRobot - 1 : free_square_of_maze ) & ( xPositionOfRobot |-> yPositionOfRobot - 1 : maze_area )

        THEN
            yPositionOfRobot := yPositionOfRobot - 1 ||

            visitedSquaresOfMaze := visitedSquaresOfMaze <- ( xPositionOfRobot |-> yPositionOfRobot ) ||

            toast_notification := SUCCESSFUL_MOVE ||

            movedDirectionsOfMaze := movedDirectionsOfMaze <- SOUTH

        ELSIF
                ( xPositionOfRobot |-> yPositionOfRobot - 1 : internal_walls_of_maze )

        THEN
                toast_notification := INTERNAL_BLOCK_DETECTED_CANNOT_MOVE
        ELSE
                toast_notification := EXTERNAL_WALL_DETECTED_CANNOT_MOVE

        END

  END ;

 /* Moves the robot east - Checks for the following set constraints
  *  Moved square is not a blocked square
  *  Moved square is not outside the border of the wall
  *  Checks if the robot has reached final square
  */
  toast_notification <-- MoveEast =

    PRE
         not ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) ) & ( xPositionOfRobot |-> yPositionOfRobot : maze_area )
    THEN
        IF
            ( xPositionOfRobot + 1 |-> yPositionOfRobot : free_square_of_maze ) & ( xPositionOfRobot + 1 |-> yPositionOfRobot : maze_area )

        THEN
            xPositionOfRobot := xPositionOfRobot + 1 ||

            visitedSquaresOfMaze := visitedSquaresOfMaze <- ( xPositionOfRobot |-> yPositionOfRobot ) ||

            toast_notification := SUCCESSFUL_MOVE ||

            movedDirectionsOfMaze := movedDirectionsOfMaze <-  EAST

        ELSIF
                ( xPositionOfRobot + 1 |-> yPositionOfRobot : internal_walls_of_maze )

        THEN
                toast_notification := INTERNAL_BLOCK_DETECTED_CANNOT_MOVE
        ELSE
                 toast_notification := EXTERNAL_WALL_DETECTED_CANNOT_MOVE


        END
  END ;

 /* Moves the robot west - Checks for the following set constraints
  *  Moved square is not a blocked square
  *  Moved square is not outside the border of the wall
  *  Checks if the robot has reached final square
  */
  toast_notification <-- MoveWest =

    PRE
        not ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) ) & ( xPositionOfRobot |-> yPositionOfRobot : maze_area )
    THEN
        IF
           ( xPositionOfRobot - 1 |-> yPositionOfRobot : free_square_of_maze ) & ( xPositionOfRobot - 1 |-> yPositionOfRobot : maze_area )

        THEN

        xPositionOfRobot := xPositionOfRobot - 1 ||

        visitedSquaresOfMaze := visitedSquaresOfMaze <- ( xPositionOfRobot |-> yPositionOfRobot ) ||

        toast_notification := SUCCESSFUL_MOVE ||

         movedDirectionsOfMaze := movedDirectionsOfMaze <- WEST

        ELSIF
            ( xPositionOfRobot - 1 |-> yPositionOfRobot : internal_walls_of_maze )
        THEN
            toast_notification := INTERNAL_BLOCK_DETECTED_CANNOT_MOVE
        ELSE
             toast_notification := EXTERNAL_WALL_DETECTED_CANNOT_MOVE


        END
  END ;

  // Give the route's root that it has traveled through
  route <-- getRobotRoute =
    BEGIN
        route := visitedSquaresOfMaze
    END ;


  // Reset the game, and starts again from the beginning
  resetMazeRobot =
    PRE
        ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze )
    THEN

    xPositionOfRobot := prj1 ( x_axis_range_of_maze , y_axis_range_of_maze ) ( initial_square_of_maze ) ||
    yPositionOfRobot := prj2 ( x_axis_range_of_maze , y_axis_range_of_maze ) ( initial_square_of_maze ) ||
    visitedSquaresOfMaze := [ initial_square_of_maze ] ||
    movedDirectionsOfMaze := <>
    END ;

  // Report whether the robot has reached the exit maze square
  foundExitSquare <-- foundExit =
    PRE
        foundExitSquare : BOOLEAN_SET
    THEN
        IF
            ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) )
        THEN
            foundExitSquare := YES
        ELSE
            foundExitSquare := NO
        END
    END ;



    // Current status of the robot
    currentLocation <-- getRobotCurrentPosition =
    BEGIN
        currentLocation := xPositionOfRobot |-> yPositionOfRobot
    END ;

    // All movements taken by robot 
    allMovements <-- directionsTaken =
    BEGIN
        allMovements := movedDirectionsOfMaze
    END ;

  /* Attempts to Teleport the Robot - Checks for the following set constraints
   *  X and Y within range of the border wall
   *  X and Y not a element of Blocked square AND
   *  Not in the starting square OR ending square 
   *  Cannot teleport to same square
   */

  teleportMessage  <-- teleport ( newXTeleport , newYTeleport ) =
     PRE
        not ( ( xPositionOfRobot |-> yPositionOfRobot = exit_square_of_maze ) ) &
        newXTeleport : x_axis_range_of_maze &
        newYTeleport : y_axis_range_of_maze
     THEN

        IF
           ( newXTeleport |-> newYTeleport : free_square_of_maze )

        THEN
                           IF ( newXTeleport = xPositionOfRobot & newYTeleport = yPositionOfRobot )
                                THEN
                                            teleportMessage :=   TELEPORT_TO_SAME_WALLS_DENIED
                                ELSE
                                    IF
                                                ( size ( visitedSquaresOfMaze ) = 1 & newXTeleport |-> newYTeleport = exit_square_of_maze )
                                   THEN
                                                   teleportMessage :=    TELEPORT_TO_EXIT_AT_START_DENIED

                                   ELSE

                                       IF
                                                not ( ( newXTeleport |-> newYTeleport : internal_walls_of_maze ) )
                                       THEN
                                             xPositionOfRobot := newXTeleport ||
                                            yPositionOfRobot := newYTeleport ||
                                            visitedSquaresOfMaze := visitedSquaresOfMaze <- ( xPositionOfRobot |-> yPositionOfRobot ) ||
                                           teleportMessage := TELEPORTATION_SUCCESSFUL ||
                                movedDirectionsOfMaze := movedDirectionsOfMaze <- TELEPORT

                                       ELSE
                                          teleportMessage := TELEPORT_TO_INTERNAL_WALLS_DENIED


                            END
                        END
                    END
            ELSE
                teleportMessage := TELEPORT_TO_INTERNAL_WALLS_DENIED
      END
  END ;

/* Discover whether a robot visits the square or not.
 * The inputs must be contained within the border as a precondition.
 * If it is true, the range is obtained.
 */

  isVisited  <-- visitedSquare ( xPost , yPost ) =
       PRE
           xPost : x_axis_range_of_maze &
           yPost : y_axis_range_of_maze
       THEN
           IF
           ( xPost |-> yPost : ran ( visitedSquaresOfMaze ) )
           THEN
           isVisited := YES
           ELSE
           isVisited := NO
           END

       END

   END
