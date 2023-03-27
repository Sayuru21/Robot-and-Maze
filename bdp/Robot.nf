Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Robot))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Robot))==(Machine(Robot));
  Level(Machine(Robot))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Robot)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Robot))==(Maze)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Robot))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Robot))==(?);
  List_Includes(Machine(Robot))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Robot))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Robot))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Robot))==(?);
  Context_List_Variables(Machine(Robot))==(?);
  Abstract_List_Variables(Machine(Robot))==(?);
  Local_List_Variables(Machine(Robot))==(movedDirectionsOfMaze,visitedSquaresOfMaze,yPositionOfRobot,xPositionOfRobot);
  List_Variables(Machine(Robot))==(movedDirectionsOfMaze,visitedSquaresOfMaze,yPositionOfRobot,xPositionOfRobot);
  External_List_Variables(Machine(Robot))==(movedDirectionsOfMaze,visitedSquaresOfMaze,yPositionOfRobot,xPositionOfRobot)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Robot))==(?);
  Abstract_List_VisibleVariables(Machine(Robot))==(?);
  External_List_VisibleVariables(Machine(Robot))==(?);
  Expanded_List_VisibleVariables(Machine(Robot))==(?);
  List_VisibleVariables(Machine(Robot))==(?);
  Internal_List_VisibleVariables(Machine(Robot))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Robot))==(btrue);
  Gluing_List_Invariant(Machine(Robot))==(btrue);
  Expanded_List_Invariant(Machine(Robot))==(btrue);
  Abstract_List_Invariant(Machine(Robot))==(btrue);
  Context_List_Invariant(Machine(Robot))==(btrue);
  List_Invariant(Machine(Robot))==(xPositionOfRobot: x_axis_range_of_maze & yPositionOfRobot: y_axis_range_of_maze & xPositionOfRobot: NATURAL1 & yPositionOfRobot: NATURAL1 & xPositionOfRobot|->yPositionOfRobot: free_square_of_maze & visitedSquaresOfMaze: seq(free_square_of_maze) & movedDirectionsOfMaze: seq(MOVEMENT_DIRECTIONS))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Robot))==(btrue);
  Abstract_List_Assertions(Machine(Robot))==(btrue);
  Context_List_Assertions(Machine(Robot))==(btrue);
  List_Assertions(Machine(Robot))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Robot))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Robot))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Robot))==(xPositionOfRobot,yPositionOfRobot,visitedSquaresOfMaze,movedDirectionsOfMaze:=prj1(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze),prj2(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze),[initial_square_of_maze],<>);
  Context_List_Initialisation(Machine(Robot))==(skip);
  List_Initialisation(Machine(Robot))==(xPositionOfRobot:=prj1(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze) || yPositionOfRobot:=prj2(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze) || visitedSquaresOfMaze:=[initial_square_of_maze] || movedDirectionsOfMaze:=<>)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Robot))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Robot),Machine(Maze))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Robot))==(btrue);
  List_Constraints(Machine(Robot))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Robot))==(MoveNorth,MoveSouth,MoveEast,MoveWest,getRobotRoute,resetMazeRobot,foundExit,getRobotCurrentPosition,directionsTaken,teleport,visitedSquare);
  List_Operations(Machine(Robot))==(MoveNorth,MoveSouth,MoveEast,MoveWest,getRobotRoute,resetMazeRobot,foundExit,getRobotCurrentPosition,directionsTaken,teleport,visitedSquare)
END
&
THEORY ListInputX IS
  List_Input(Machine(Robot),MoveNorth)==(?);
  List_Input(Machine(Robot),MoveSouth)==(?);
  List_Input(Machine(Robot),MoveEast)==(?);
  List_Input(Machine(Robot),MoveWest)==(?);
  List_Input(Machine(Robot),getRobotRoute)==(?);
  List_Input(Machine(Robot),resetMazeRobot)==(?);
  List_Input(Machine(Robot),foundExit)==(?);
  List_Input(Machine(Robot),getRobotCurrentPosition)==(?);
  List_Input(Machine(Robot),directionsTaken)==(?);
  List_Input(Machine(Robot),teleport)==(newXTeleport,newYTeleport);
  List_Input(Machine(Robot),visitedSquare)==(xPost,yPost)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Robot),MoveNorth)==(toast_notification);
  List_Output(Machine(Robot),MoveSouth)==(toast_notification);
  List_Output(Machine(Robot),MoveEast)==(toast_notification);
  List_Output(Machine(Robot),MoveWest)==(toast_notification);
  List_Output(Machine(Robot),getRobotRoute)==(route);
  List_Output(Machine(Robot),resetMazeRobot)==(?);
  List_Output(Machine(Robot),foundExit)==(foundExitSquare);
  List_Output(Machine(Robot),getRobotCurrentPosition)==(currentLocation);
  List_Output(Machine(Robot),directionsTaken)==(allMovements);
  List_Output(Machine(Robot),teleport)==(teleportMessage);
  List_Output(Machine(Robot),visitedSquare)==(isVisited)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Robot),MoveNorth)==(toast_notification <-- MoveNorth);
  List_Header(Machine(Robot),MoveSouth)==(toast_notification <-- MoveSouth);
  List_Header(Machine(Robot),MoveEast)==(toast_notification <-- MoveEast);
  List_Header(Machine(Robot),MoveWest)==(toast_notification <-- MoveWest);
  List_Header(Machine(Robot),getRobotRoute)==(route <-- getRobotRoute);
  List_Header(Machine(Robot),resetMazeRobot)==(resetMazeRobot);
  List_Header(Machine(Robot),foundExit)==(foundExitSquare <-- foundExit);
  List_Header(Machine(Robot),getRobotCurrentPosition)==(currentLocation <-- getRobotCurrentPosition);
  List_Header(Machine(Robot),directionsTaken)==(allMovements <-- directionsTaken);
  List_Header(Machine(Robot),teleport)==(teleportMessage <-- teleport(newXTeleport,newYTeleport));
  List_Header(Machine(Robot),visitedSquare)==(isVisited <-- visitedSquare(xPost,yPost))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Robot),MoveNorth)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area);
  List_Precondition(Machine(Robot),MoveSouth)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area);
  List_Precondition(Machine(Robot),MoveEast)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area);
  List_Precondition(Machine(Robot),MoveWest)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area);
  List_Precondition(Machine(Robot),getRobotRoute)==(btrue);
  List_Precondition(Machine(Robot),resetMazeRobot)==(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze);
  List_Precondition(Machine(Robot),foundExit)==(foundExitSquare: BOOLEAN_SET);
  List_Precondition(Machine(Robot),getRobotCurrentPosition)==(btrue);
  List_Precondition(Machine(Robot),directionsTaken)==(btrue);
  List_Precondition(Machine(Robot),teleport)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & newXTeleport: x_axis_range_of_maze & newYTeleport: y_axis_range_of_maze);
  List_Precondition(Machine(Robot),visitedSquare)==(xPost: x_axis_range_of_maze & yPost: y_axis_range_of_maze)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Robot),visitedSquare)==(xPost: x_axis_range_of_maze & yPost: y_axis_range_of_maze | xPost|->yPost: ran(visitedSquaresOfMaze) ==> isVisited:=YES [] not(xPost|->yPost: ran(visitedSquaresOfMaze)) ==> isVisited:=NO);
  Expanded_List_Substitution(Machine(Robot),teleport)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & newXTeleport: x_axis_range_of_maze & newYTeleport: y_axis_range_of_maze | newXTeleport|->newYTeleport: free_square_of_maze ==> (newXTeleport = xPositionOfRobot & newYTeleport = yPositionOfRobot ==> teleportMessage:=TELEPORT_TO_SAME_WALLS_DENIED [] not(newXTeleport = xPositionOfRobot & newYTeleport = yPositionOfRobot) ==> (size(visitedSquaresOfMaze) = 1 & newXTeleport|->newYTeleport = exit_square_of_maze ==> teleportMessage:=TELEPORT_TO_EXIT_AT_START_DENIED [] not(size(visitedSquaresOfMaze) = 1 & newXTeleport|->newYTeleport = exit_square_of_maze) ==> (not(newXTeleport|->newYTeleport: internal_walls_of_maze) ==> xPositionOfRobot,yPositionOfRobot,visitedSquaresOfMaze,teleportMessage,movedDirectionsOfMaze:=newXTeleport,newYTeleport,visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot),TELEPORTATION_SUCCESSFUL,movedDirectionsOfMaze<-TELEPORT [] not(not(newXTeleport|->newYTeleport: internal_walls_of_maze)) ==> teleportMessage:=TELEPORT_TO_INTERNAL_WALLS_DENIED))) [] not(newXTeleport|->newYTeleport: free_square_of_maze) ==> teleportMessage:=TELEPORT_TO_INTERNAL_WALLS_DENIED);
  Expanded_List_Substitution(Machine(Robot),directionsTaken)==(btrue | allMovements:=movedDirectionsOfMaze);
  Expanded_List_Substitution(Machine(Robot),getRobotCurrentPosition)==(btrue | currentLocation:=xPositionOfRobot|->yPositionOfRobot);
  Expanded_List_Substitution(Machine(Robot),foundExit)==(foundExitSquare: BOOLEAN_SET | xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze ==> foundExitSquare:=YES [] not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) ==> foundExitSquare:=NO);
  Expanded_List_Substitution(Machine(Robot),resetMazeRobot)==(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze | xPositionOfRobot,yPositionOfRobot,visitedSquaresOfMaze,movedDirectionsOfMaze:=prj1(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze),prj2(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze),[initial_square_of_maze],<>);
  Expanded_List_Substitution(Machine(Robot),getRobotRoute)==(btrue | route:=visitedSquaresOfMaze);
  Expanded_List_Substitution(Machine(Robot),MoveWest)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area | xPositionOfRobot-1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot-1|->yPositionOfRobot: maze_area ==> xPositionOfRobot,visitedSquaresOfMaze,toast_notification,movedDirectionsOfMaze:=xPositionOfRobot-1,visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot),SUCCESSFUL_MOVE,movedDirectionsOfMaze<-WEST [] not(xPositionOfRobot-1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot-1|->yPositionOfRobot: maze_area) ==> (xPositionOfRobot-1|->yPositionOfRobot: internal_walls_of_maze ==> toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE [] not(xPositionOfRobot-1|->yPositionOfRobot: internal_walls_of_maze) ==> toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE));
  Expanded_List_Substitution(Machine(Robot),MoveEast)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area | xPositionOfRobot+1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot+1|->yPositionOfRobot: maze_area ==> xPositionOfRobot,visitedSquaresOfMaze,toast_notification,movedDirectionsOfMaze:=xPositionOfRobot+1,visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot),SUCCESSFUL_MOVE,movedDirectionsOfMaze<-EAST [] not(xPositionOfRobot+1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot+1|->yPositionOfRobot: maze_area) ==> (xPositionOfRobot+1|->yPositionOfRobot: internal_walls_of_maze ==> toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE [] not(xPositionOfRobot+1|->yPositionOfRobot: internal_walls_of_maze) ==> toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE));
  Expanded_List_Substitution(Machine(Robot),MoveSouth)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area | xPositionOfRobot|->yPositionOfRobot-1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot-1: maze_area ==> yPositionOfRobot,visitedSquaresOfMaze,toast_notification,movedDirectionsOfMaze:=yPositionOfRobot-1,visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot),SUCCESSFUL_MOVE,movedDirectionsOfMaze<-SOUTH [] not(xPositionOfRobot|->yPositionOfRobot-1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot-1: maze_area) ==> (xPositionOfRobot|->yPositionOfRobot-1: internal_walls_of_maze ==> toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE [] not(xPositionOfRobot|->yPositionOfRobot-1: internal_walls_of_maze) ==> toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE));
  Expanded_List_Substitution(Machine(Robot),MoveNorth)==(not(xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze) & xPositionOfRobot|->yPositionOfRobot: maze_area | xPositionOfRobot|->yPositionOfRobot+1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot+1: maze_area ==> yPositionOfRobot,visitedSquaresOfMaze,toast_notification,movedDirectionsOfMaze:=yPositionOfRobot+1,visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot),SUCCESSFUL_MOVE,movedDirectionsOfMaze<-NORTH [] not(xPositionOfRobot|->yPositionOfRobot+1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot+1: maze_area) ==> (xPositionOfRobot|->yPositionOfRobot+1: internal_walls_of_maze ==> toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE [] not(xPositionOfRobot|->yPositionOfRobot+1: internal_walls_of_maze) ==> toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE));
  List_Substitution(Machine(Robot),MoveNorth)==(IF xPositionOfRobot|->yPositionOfRobot+1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot+1: maze_area THEN yPositionOfRobot:=yPositionOfRobot+1 || visitedSquaresOfMaze:=visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot) || toast_notification:=SUCCESSFUL_MOVE || movedDirectionsOfMaze:=movedDirectionsOfMaze<-NORTH ELSIF xPositionOfRobot|->yPositionOfRobot+1: internal_walls_of_maze THEN toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE ELSE toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE END);
  List_Substitution(Machine(Robot),MoveSouth)==(IF xPositionOfRobot|->yPositionOfRobot-1: free_square_of_maze & xPositionOfRobot|->yPositionOfRobot-1: maze_area THEN yPositionOfRobot:=yPositionOfRobot-1 || visitedSquaresOfMaze:=visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot) || toast_notification:=SUCCESSFUL_MOVE || movedDirectionsOfMaze:=movedDirectionsOfMaze<-SOUTH ELSIF xPositionOfRobot|->yPositionOfRobot-1: internal_walls_of_maze THEN toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE ELSE toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE END);
  List_Substitution(Machine(Robot),MoveEast)==(IF xPositionOfRobot+1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot+1|->yPositionOfRobot: maze_area THEN xPositionOfRobot:=xPositionOfRobot+1 || visitedSquaresOfMaze:=visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot) || toast_notification:=SUCCESSFUL_MOVE || movedDirectionsOfMaze:=movedDirectionsOfMaze<-EAST ELSIF xPositionOfRobot+1|->yPositionOfRobot: internal_walls_of_maze THEN toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE ELSE toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE END);
  List_Substitution(Machine(Robot),MoveWest)==(IF xPositionOfRobot-1|->yPositionOfRobot: free_square_of_maze & xPositionOfRobot-1|->yPositionOfRobot: maze_area THEN xPositionOfRobot:=xPositionOfRobot-1 || visitedSquaresOfMaze:=visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot) || toast_notification:=SUCCESSFUL_MOVE || movedDirectionsOfMaze:=movedDirectionsOfMaze<-WEST ELSIF xPositionOfRobot-1|->yPositionOfRobot: internal_walls_of_maze THEN toast_notification:=INTERNAL_BLOCK_DETECTED_CANNOT_MOVE ELSE toast_notification:=EXTERNAL_WALL_DETECTED_CANNOT_MOVE END);
  List_Substitution(Machine(Robot),getRobotRoute)==(route:=visitedSquaresOfMaze);
  List_Substitution(Machine(Robot),resetMazeRobot)==(xPositionOfRobot:=prj1(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze) || yPositionOfRobot:=prj2(x_axis_range_of_maze,y_axis_range_of_maze)(initial_square_of_maze) || visitedSquaresOfMaze:=[initial_square_of_maze] || movedDirectionsOfMaze:=<>);
  List_Substitution(Machine(Robot),foundExit)==(IF xPositionOfRobot|->yPositionOfRobot = exit_square_of_maze THEN foundExitSquare:=YES ELSE foundExitSquare:=NO END);
  List_Substitution(Machine(Robot),getRobotCurrentPosition)==(currentLocation:=xPositionOfRobot|->yPositionOfRobot);
  List_Substitution(Machine(Robot),directionsTaken)==(allMovements:=movedDirectionsOfMaze);
  List_Substitution(Machine(Robot),teleport)==(IF newXTeleport|->newYTeleport: free_square_of_maze THEN IF newXTeleport = xPositionOfRobot & newYTeleport = yPositionOfRobot THEN teleportMessage:=TELEPORT_TO_SAME_WALLS_DENIED ELSE IF size(visitedSquaresOfMaze) = 1 & newXTeleport|->newYTeleport = exit_square_of_maze THEN teleportMessage:=TELEPORT_TO_EXIT_AT_START_DENIED ELSE IF not(newXTeleport|->newYTeleport: internal_walls_of_maze) THEN xPositionOfRobot:=newXTeleport || yPositionOfRobot:=newYTeleport || visitedSquaresOfMaze:=visitedSquaresOfMaze<-(xPositionOfRobot|->yPositionOfRobot) || teleportMessage:=TELEPORTATION_SUCCESSFUL || movedDirectionsOfMaze:=movedDirectionsOfMaze<-TELEPORT ELSE teleportMessage:=TELEPORT_TO_INTERNAL_WALLS_DENIED END END END ELSE teleportMessage:=TELEPORT_TO_INTERNAL_WALLS_DENIED END);
  List_Substitution(Machine(Robot),visitedSquare)==(IF xPost|->yPost: ran(visitedSquaresOfMaze) THEN isVisited:=YES ELSE isVisited:=NO END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Robot))==(?);
  Inherited_List_Constants(Machine(Robot))==(?);
  List_Constants(Machine(Robot))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Robot),TOAST_NOTIFICATION)==({SUCCESSFUL_MOVE,INTERNAL_BLOCK_DETECTED_CANNOT_MOVE,EXTERNAL_WALL_DETECTED_CANNOT_MOVE,TELEPORTATION_SUCCESSFUL,TELEPORT_TO_SAME_WALLS_DENIED,TELEPORT_TO_INTERNAL_WALLS_DENIED,TELEPORT_TO_EXIT_AT_START_DENIED,UNSUCCESSFUL_MOVE});
  Context_List_Enumerated(Machine(Robot))==(?);
  Context_List_Defered(Machine(Robot))==(?);
  Context_List_Sets(Machine(Robot))==(?);
  List_Valuable_Sets(Machine(Robot))==(?);
  Inherited_List_Enumerated(Machine(Robot))==(?);
  Inherited_List_Defered(Machine(Robot))==(?);
  Inherited_List_Sets(Machine(Robot))==(?);
  List_Enumerated(Machine(Robot))==(TOAST_NOTIFICATION,BOOLEAN_SET,MOVEMENT_DIRECTIONS);
  List_Defered(Machine(Robot))==(?);
  List_Sets(Machine(Robot))==(TOAST_NOTIFICATION,BOOLEAN_SET,MOVEMENT_DIRECTIONS);
  Set_Definition(Machine(Robot),BOOLEAN_SET)==({YES,NO});
  Set_Definition(Machine(Robot),MOVEMENT_DIRECTIONS)==({NORTH,SOUTH,EAST,WEST,TELEPORT})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Robot))==(?);
  Expanded_List_HiddenConstants(Machine(Robot))==(?);
  List_HiddenConstants(Machine(Robot))==(?);
  External_List_HiddenConstants(Machine(Robot))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Robot))==(btrue);
  Context_List_Properties(Machine(Robot))==(x_axis_range_of_maze = 1..7 & y_axis_range_of_maze = 1..5 & maze_area = x_axis_range_of_maze*y_axis_range_of_maze & internal_walls_of_maze <: maze_area & internal_walls_of_maze = {1|->3,2|->1,2|->3,2|->5,3|->3,4|->2,4|->3,4|->4,6|->1,6|->2,6|->4,7|->4} & free_square_of_maze <: maze_area & free_square_of_maze/\internal_walls_of_maze = {} & free_square_of_maze\/internal_walls_of_maze = maze_area & initial_square_of_maze: free_square_of_maze & initial_square_of_maze: maze_area & initial_square_of_maze = 1|->1 & exit_square_of_maze: free_square_of_maze & exit_square_of_maze: maze_area & exit_square_of_maze = 1|->5);
  Inherited_List_Properties(Machine(Robot))==(btrue);
  List_Properties(Machine(Robot))==(TOAST_NOTIFICATION: FIN(INTEGER) & not(TOAST_NOTIFICATION = {}) & BOOLEAN_SET: FIN(INTEGER) & not(BOOLEAN_SET = {}) & MOVEMENT_DIRECTIONS: FIN(INTEGER) & not(MOVEMENT_DIRECTIONS = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Context_List_Enumerated(Machine(Robot))==(?);
  Seen_Context_List_Invariant(Machine(Robot))==(btrue);
  Seen_Context_List_Assertions(Machine(Robot))==(btrue);
  Seen_Context_List_Properties(Machine(Robot))==(btrue);
  Seen_List_Constraints(Machine(Robot))==(btrue);
  Seen_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Expanded_List_Invariant(Machine(Robot),Machine(Maze))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Robot),MoveNorth)==(?);
  List_ANY_Var(Machine(Robot),MoveSouth)==(?);
  List_ANY_Var(Machine(Robot),MoveEast)==(?);
  List_ANY_Var(Machine(Robot),MoveWest)==(?);
  List_ANY_Var(Machine(Robot),getRobotRoute)==(?);
  List_ANY_Var(Machine(Robot),resetMazeRobot)==(?);
  List_ANY_Var(Machine(Robot),foundExit)==(?);
  List_ANY_Var(Machine(Robot),getRobotCurrentPosition)==(?);
  List_ANY_Var(Machine(Robot),directionsTaken)==(?);
  List_ANY_Var(Machine(Robot),teleport)==(?);
  List_ANY_Var(Machine(Robot),visitedSquare)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Robot)) == (TOAST_NOTIFICATION,BOOLEAN_SET,MOVEMENT_DIRECTIONS,SUCCESSFUL_MOVE,INTERNAL_BLOCK_DETECTED_CANNOT_MOVE,EXTERNAL_WALL_DETECTED_CANNOT_MOVE,TELEPORTATION_SUCCESSFUL,TELEPORT_TO_SAME_WALLS_DENIED,TELEPORT_TO_INTERNAL_WALLS_DENIED,TELEPORT_TO_EXIT_AT_START_DENIED,UNSUCCESSFUL_MOVE,YES,NO,NORTH,SOUTH,EAST,WEST,TELEPORT | ? | movedDirectionsOfMaze,visitedSquaresOfMaze,yPositionOfRobot,xPositionOfRobot | ? | MoveNorth,MoveSouth,MoveEast,MoveWest,getRobotRoute,resetMazeRobot,foundExit,getRobotCurrentPosition,directionsTaken,teleport,visitedSquare | ? | seen(Machine(Maze)) | ? | Robot);
  List_Of_HiddenCst_Ids(Machine(Robot)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Robot)) == (?);
  List_Of_VisibleVar_Ids(Machine(Robot)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Robot)) == (?: ?);
  List_Of_Ids(Machine(Maze)) == (x_axis_range_of_maze,y_axis_range_of_maze,maze_area,internal_walls_of_maze,free_square_of_maze,initial_square_of_maze,exit_square_of_maze | ? | ? | ? | ? | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (x_axis_range_of_maze,y_axis_range_of_maze,maze_area,internal_walls_of_maze,free_square_of_maze,initial_square_of_maze,exit_square_of_maze);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Robot)) == (Type(TOAST_NOTIFICATION) == Cst(SetOf(etype(TOAST_NOTIFICATION,0,7)));Type(BOOLEAN_SET) == Cst(SetOf(etype(BOOLEAN_SET,0,1)));Type(MOVEMENT_DIRECTIONS) == Cst(SetOf(etype(MOVEMENT_DIRECTIONS,0,4))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Robot)) == (Type(SUCCESSFUL_MOVE) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(INTERNAL_BLOCK_DETECTED_CANNOT_MOVE) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(EXTERNAL_WALL_DETECTED_CANNOT_MOVE) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(TELEPORTATION_SUCCESSFUL) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(TELEPORT_TO_SAME_WALLS_DENIED) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(TELEPORT_TO_INTERNAL_WALLS_DENIED) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(TELEPORT_TO_EXIT_AT_START_DENIED) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(UNSUCCESSFUL_MOVE) == Cst(etype(TOAST_NOTIFICATION,0,7));Type(YES) == Cst(etype(BOOLEAN_SET,0,1));Type(NO) == Cst(etype(BOOLEAN_SET,0,1));Type(NORTH) == Cst(etype(MOVEMENT_DIRECTIONS,0,4));Type(SOUTH) == Cst(etype(MOVEMENT_DIRECTIONS,0,4));Type(EAST) == Cst(etype(MOVEMENT_DIRECTIONS,0,4));Type(WEST) == Cst(etype(MOVEMENT_DIRECTIONS,0,4));Type(TELEPORT) == Cst(etype(MOVEMENT_DIRECTIONS,0,4)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Robot)) == (Type(movedDirectionsOfMaze) == Mvl(SetOf(btype(INTEGER,?,?)*etype(MOVEMENT_DIRECTIONS,?,?)));Type(visitedSquaresOfMaze) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(yPositionOfRobot) == Mvl(btype(INTEGER,?,?));Type(xPositionOfRobot) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Robot)) == (Type(visitedSquare) == Cst(etype(BOOLEAN_SET,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(teleport) == Cst(etype(TOAST_NOTIFICATION,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(directionsTaken) == Cst(SetOf(btype(INTEGER,?,?)*etype(MOVEMENT_DIRECTIONS,?,?)),No_type);Type(getRobotCurrentPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(foundExit) == Cst(etype(BOOLEAN_SET,?,?),No_type);Type(resetMazeRobot) == Cst(No_type,No_type);Type(getRobotRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(MoveWest) == Cst(etype(TOAST_NOTIFICATION,?,?),No_type);Type(MoveEast) == Cst(etype(TOAST_NOTIFICATION,?,?),No_type);Type(MoveSouth) == Cst(etype(TOAST_NOTIFICATION,?,?),No_type);Type(MoveNorth) == Cst(etype(TOAST_NOTIFICATION,?,?),No_type));
  Observers(Machine(Robot)) == (Type(visitedSquare) == Cst(etype(BOOLEAN_SET,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(directionsTaken) == Cst(SetOf(btype(INTEGER,?,?)*etype(MOVEMENT_DIRECTIONS,?,?)),No_type);Type(getRobotCurrentPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(foundExit) == Cst(etype(BOOLEAN_SET,?,?),No_type);Type(getRobotRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
